#version 300 es
precision highp float;

uniform vec2 u_Dimensions;
uniform float u_Time;
uniform float u_Size;
uniform float u_Color;

in vec2 fs_Pos;
in vec4 fs_LightVec;
out vec4 out_Col;

vec3 u_LightColor = vec3(1.0);
vec3 eye = vec3(0.7, -1.7, -12.0);
vec3 ref = vec3(1.0);
//Light setup
vec3 light = vec3(5.0, 5.0, 5.0);

//Object setup
vec4 sph1 = vec4( 0.0, 0.0, 0.0, 1.0);
float EPSILON = 0.1;
float PI = 3.1415926;
#define saturate(x) clamp(x,0.,1.)
#define rgb(r,g,b) (vec3(r,g,b)/255.)
#define GOLDEN_ANGLE 2.39996

#define ITERATIONS 150

mat2 rot = mat2(cos(GOLDEN_ANGLE), sin(GOLDEN_ANGLE), -sin(GOLDEN_ANGLE), cos(GOLDEN_ANGLE));

float rand(float x) { return fract(sin(x) * 71523.5413291); }

float rand(vec2 x) { return rand(dot(x, vec2(13.4251, 35.5128))); }

float random_wood (in vec2 st) {
  return fract(sin(dot(st.xy,
    vec2(12.9898,78.233)))
    * 43758.5453123);
  }

  // iq
  float noise(vec2 x)
  {
    vec2 i = floor(x);
    vec2 f = x - i;
    f *= f*(3.-2.*f);
    return mix(mix(rand(i), rand(i+vec2(1,0)), f.x),
    mix(rand(i+vec2(0,1)), rand(i+vec2(1,1)), f.x), f.y);
  }

  mat2 rotate2d(float angle){
    return mat2(cos(angle),-sin(angle),
    sin(angle),cos(angle));
  }

  float lines(in vec2 pos, float b){
    float scale = 10.0;
    pos *= scale;
    return smoothstep(0.0,
      .5+b*.5,
      abs((sin(pos.x*3.1415)+b*2.0))*.5);
    }

    float fbm(vec2 x)
    {
      float total = 0.0, freq = 1.0, amp = 1.0;
      float octaves = 10.;
      for (float i=0.; i< octaves; i = i + 1.)
      {
        freq = pow(2.0, i);
        amp *= 0.5;
        total += amp * noise(freq * x);
      }
      return total;
    }

    float cloud(vec2 uv, float scalex, float scaley, float density, float sharpness, float speed)
    {
      return pow(saturate(fbm(vec2(scalex,scaley)*(uv+vec2(speed,0)*u_Time / 5.0))-(1.0-density)), 1.0-sharpness);
    }

    vec3 render(vec2 uv)
    {
      // sky
      vec3 color = mix(rgb(244,195,198), rgb(173,216,230), uv.y);
      // sun
      vec2 spos = uv - vec2(0.95, 1.0);
      float sun = exp(-4.*dot(spos,spos));
      vec3 scol = rgb(255,155,102) * sun * 0.7;
      color += scol;
      // clouds
      vec3 cl1 = mix(rgb(166,191,224), rgb(127,139, 149), uv.y);
      color = mix(color, cl1, cloud(uv,3.,10.,0.55,0.05,0.01));

      // post
      color *= vec3(1.0,0.93,0.81)*1.04;
      //color = mix(0.75*rgb(255,205,161), color, smoothstep(-0.1,0.3,uv.y));
      //color = pow(color,vec3(1.3));
      return color;
    }

    // noise functions
    float random1( vec2 p , vec2 seed) {
      return fract(sin(dot(p + seed, vec2(127.1, 311.7))) * 43758.5453);
    }

    // Raycasting
    vec3 rayCast() {
      float sx = fs_Pos.x;
      float sy = fs_Pos.y;
      float len = length(ref -  eye);
      vec3 look = normalize(ref -  eye);
      vec3 right = normalize(cross(look, vec3(0.0, 1.0, 0.0)));
      vec3 up = cross(right, look);
      float tan_fovy = tan(45.0 / 2.0);
      float aspect = u_Dimensions.x / u_Dimensions.y;
      vec3 V = up * len * tan_fovy;
      vec3 H = right * len * aspect * tan_fovy;

      vec3 p = ref + sx * H + sy * V;
      vec3 direct = normalize(p -  eye);
      return direct;
    }

    vec3 palette( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
    {
      return a + b*cos( 6.28318*(c*t+d) );
    }

    // primitives courtesy of http://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm


    float sdPlane( vec3 p )
    {
      return p.y;
    }

    float sdSphere( vec3 p, float s )
    {
      return length(p)-s;
    }

    float sdBox( vec3 p, vec3 b )
    {
      vec3 d = abs(p) - b;
      return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0));
    }

    float udRoundBox( vec3 p, vec3 b, float r )
    {
      return length(max(abs(p)-b,0.0))-r;
    }

    //----------------------------------------------------------------------

    float opS( float d1, float d2 )
    {
      return max(-d2,d1);
    }

    vec3 opRep( vec3 p, vec3 c )
    {
      return mod(p,c)-0.5*c;
    }

    // operations courtesy of http://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
    float opUnion( float d1, float d2 ) {
      return min(d1,d2);
    }

    vec2 opU( vec2 d1, vec2 d2 ) {
      return (d1.x<d2.x) ? d1 : d2;
    }

    float opSmoothUnion( float d1, float d2, float k ) {
      float h = clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 );
      return mix( d2, d1, h ) - k*h*(1.0-h);
    }

    float opIntersection( float d1, float d2 ) { return max(d1,d2);}

    float opSubtraction( float d1, float d2 ) { return max(-d1,d2); }

    vec3 opTwist(vec3 p)
    {
      float  c = cos(10.0*p.y+10.0);
      float  s = sin(10.0*p.y+10.0);
      mat2   m = mat2(c,-s,s,c);
      return vec3(m*p.xz,p.y);
    }

    float opOnion( in float sdf, in float thickness )
    {
      return abs(sdf)-thickness;
    }

    // ease in ease out
    float ease_in (float t) {
      return t * t;
    }


    //----------------------------------------------------------------------

    // rotation matrix
    mat3 rotX(float a)
    {
      float c=cos(a);
      float s=sin(a);
      return mat3(1.0,0.0,0.0,0.0,c,-s,0.0,s,c);
    }

    mat3 rotY(float a)
    {
      float c=cos(a);
      float s=sin(a);
      return mat3(c,0.0,s,0.0,1.0,0.0,-s,0.0,c);
    }

    mat3 rotZ(float a)
    {
      float c=cos(a);
      float s=sin(a);
      return mat3(c,-s,0.0,s,c,0.0,0.0,0.0,1.0);
    }
    // scale
    mat3 scale(float s) {
      s = 1. / s;
      return mat3(s, 0., 0., 0., s, 0., 0., 0., s);
    }

    // rotate the box and
    float sdQuaterBox( vec3 p, vec3 b, vec2 sign )
    {
      b.xy *= sqrt(2.0);
      float boxBase = udRoundBox( p * rotZ( 0.25 * 3.14 ), b, 0.01 );

      return max( sign.y * p.y, max( sign.x * p.x, boxBase ) );
    }

    float sdHalfBox( vec3 p, vec3 b, float cutRot )
    {
      float boxBase = udRoundBox( p, b, 0.01 );
      vec3 cp = p * rotZ( cutRot );

      return max( cp.x, boxBase );
    }

    // adding all the primitives into one map

    vec2 buildingOne(vec3 pos) {
      float bevel = 0.01;
      float side = 0.25;
      float PI = 3.1415;

      pos = pos - vec3(10.0, -4.3, 1.0);
      pos = scale(2.0) * pos;
      vec2 res2 = vec2(       udRoundBox(   pos-vec3( 0.0, 0.25, 0.0), vec3(side), bevel ), 3.0 );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, 0.5), vec3(side), bevel), 3.0));
      res2 = opU( res2, vec2( sdHalfBox(    pos-vec3( 0.5, 1.25, 0.0), vec3(side), -0.25 * PI ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(sdPlane(pos- vec3(5.0, 0.0, 3.0)), 5.0));
      return res2;
    }

    vec2 buildingTwo(vec3 pos) {
      float bevel = 0.01;
      float side = 0.25;
      float PI = 3.1415;

      pos = pos - vec3(8.0, -4.5, 0.2);
      pos = scale(1.8) * pos;
      vec2 res2 = vec2(       udRoundBox(   pos-vec3( 0.0, 0.25, 0.0), vec3(side), bevel ), 3.0 );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, 0.5), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(    pos-vec3( 0.5, 1.25, 0.0), vec3(side), -0.25 * PI ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(pos-vec3( 0.0, 1.75, 0.0), vec3(side), -0.25 * PI ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, 0.0), vec3(side), bevel), 3.0));
      return res2;
    }

    vec2 buildingThree(vec3 pos) {
      float bevel = 0.01;
      float side = 0.25;
      float PI = 3.1415;

      pos = pos - vec3(6.0, -5.0, 1.3);
      pos = scale(1.7) * pos;
      pos = rotY(1.85 * PI) * pos;
      vec2 res2 = vec2( udRoundBox(   pos-vec3( 0.0, 0.25, 0.0), vec3(side), bevel ), 3.0 );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, 0.5), vec3(side), bevel), 3.0));

      //res2 = opU( res2, vec2( sdHalfBox(    pos-vec3( 0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.25, 0.0), vec3(side), bevel), 3.0));

      //res2 = opU( res2, vec2( sdHalfBox(pos-vec3( 0.0, 1.75, 0.0), vec3(side),bevel ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      // the other side
      pos = pos - vec3(-2.7, 0.05, 1.3);
      pos = rotY(PI * -2.65) * pos;
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, 0.0), vec3(side), bevel ), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, -0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, -0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, -0.5), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, -1.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, -1.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, -1.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, -1.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, -1.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, -1.5), vec3(side), bevel), 3.0));

      //res2 = opU( res2, vec2( sdHalfBox(    pos-vec3( 0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.25, 0.0), vec3(side), bevel), 3.0));

      //res2 = opU( res2, vec2( sdHalfBox(pos-vec3( 0.0, 1.75, 0.0), vec3(side),bevel ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, -0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, -1.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, -1.5), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, -0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, -1.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, -1.5), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, -.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, -1.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, -1.5), vec3(side), bevel), 3.0));

      // building 4
      pos = pos - vec3(0.7, 0.0, -0.5);
      pos = rotY(PI * 0.8) * pos;
      res2 = opU(res2, vec2(udRoundBox(   pos-vec3( 0.0, 0.25, 0.0), vec3(side), bevel ), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, 0.5), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(    pos-vec3( 0.5, 1.25, 0.0), vec3(side),  bevel), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(pos-vec3( 0.0, 1.75, 0.0), vec3(side),bevel ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      // building 5
      pos = pos - vec3(-3.5, 0.0, -0.5);
      pos = rotY(PI * 1.0) * pos;
      res2 = opU(res2, vec2(udRoundBox(   pos-vec3( 0.0, 0.25, 0.0), vec3(side), bevel ), 3.0 ));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(    pos-vec3( 0.5, 1.25, 0.0), vec3(side), -0.25 * PI ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(pos-vec3( 0.0, 1.75, 0.0), vec3(side), -0.25 * PI ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.5, 1.75, 0.0), vec3(side), bevel), 3.0));

      // building 6
      pos = pos - vec3(2.6, 0.0, -1.7);
      res2 = opU(res2, vec2(udRoundBox(   pos-vec3( 0.0, 0.25, 0.0), vec3(side), bevel ), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 1.25, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.75, 0.5), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.0, 0.25, 0.5), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(    pos-vec3( 0.5, 1.25, 0.0), vec3(side),  bevel), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( 0.5, 0.25, 0.0), vec3(side), bevel), 3.0));

      res2 = opU( res2, vec2( sdHalfBox(pos-vec3( 0.0, 1.75, 0.0), vec3(side),bevel ), 3.0 ) );
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 1.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 2.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -0.5, 2.75, 0.0), vec3(side), bevel), 3.0));

      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 0.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 1.75, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 2.25, 0.0), vec3(side), bevel), 3.0));
      res2 = opU(res2, vec2(udRoundBox(pos-vec3( -1.0, 2.75, 0.0), vec3(side), bevel), 3.0));

      return res2;
    }

    // Ray casting
    vec2 castRay_buildingOne( in vec3 ro, in vec3 rd, float t1)
    {
      float tmin = t1;
      float tmax = 20.0;

      float t = tmin;
      float m = -1.0;
      for( int i=0; i<64; i++ )
      {
        float precis = 0.0004*t;
        vec2 res = buildingOne(ro + rd * t );
        if( res.x<precis || t>tmax ) break;
        t += res.x;
        m = res.y;
      }

      if( t>tmax ) m=-1.0;
      return vec2( t, m );
    }

    vec2 castRay_buildingTwo( in vec3 ro, in vec3 rd, float t1)
    {
      float tmin = t1;
      float tmax = 20.0;

      float t = tmin;
      float m = -1.0;
      for( int i=0; i<64; i++ )
      {
        float precis = 0.0004*t;
        vec2 res = buildingTwo(ro + rd * t );
        if( res.x<precis || t>tmax ) break;
        t += res.x;
        m = res.y;
      }
      if( t>tmax ) m=-1.0;
      return vec2( t, m );
    }

    vec2 castRay_buildingThree( in vec3 ro, in vec3 rd, float t1)
    {
      float tmin = t1;
      float tmax = 20.0;

      float t = tmin;
      float m = -1.0;
      for( int i=0; i<64; i++ )
      {
        float precis = 0.0004*t;
        vec2 res = buildingThree(ro + rd * t );
        if( res.x<precis || t>tmax ) break;
        t += res.x;
        m = res.y;
      }
      if( t>tmax ) m=-1.0;
      return vec2( t, m );
    }

    // calculating normals

    vec3 estimateNormal_buildingOne(vec3 pos) {
      vec2 e = vec2(1.0,-1.0)*0.5773*0.0005;
      return normalize( e.xyy*buildingOne( pos + e.xyy ).x +
      e.yyx*buildingOne( pos + e.yyx ).x +
      e.yxy*buildingOne( pos + e.yxy ).x +
      e.xxx*buildingOne( pos + e.xxx ).x);
    }

    vec3 estimateNormal_buildingTwo(vec3 pos) {
      vec2 e = vec2(1.0,-1.0)*0.5773*0.0005;
      return normalize( e.xyy*buildingTwo( pos + e.xyy ).x +
      e.yyx*buildingTwo( pos + e.yyx ).x +
      e.yxy*buildingTwo( pos + e.yxy ).x +
      e.xxx*buildingTwo( pos + e.xxx ).x);
    }

    vec3 estimateNormal_buildingThree(vec3 pos) {
      vec2 e = vec2(1.0,-1.0)*0.5773*0.0005;
      return normalize( e.xyy*buildingThree( pos + e.xyy ).x +
      e.yyx*buildingThree( pos + e.yyx ).x +
      e.yxy*buildingThree( pos + e.yxy ).x +
      e.xxx*buildingThree( pos + e.xxx ).x);
    }

    // IQ's physically plausible shadow
    float softshadow( in vec3 ro, in vec3 rd, float mint, float maxt, float k )
    {
      float res = 1.0;
      for( float t=mint; t < maxt; )
      {
        float h = buildingThree(ro + rd*t).x;
        if( h<0.001 )
        return 0.0;
        res = min( res, k*h/t );
        t += h;
      }
      return res;
    }

    // calculate lambert shading

    vec3 lambert(vec3 normal, vec3 direction, vec3 color, vec3 lightColor, vec3 pos) {
      // float diffuseTerm = dot(normalize(normal), normalize(direction));
      // diffuseTerm = clamp(diffuseTerm, 0.0, 1.0);
      // float ambientTerm = 0.2;
      // float lightIntensity = diffuseTerm + ambientTerm;
      // return clamp(vec3(color.rgb * lightIntensity * lightColor), 0.0, 1.0);

      vec3 outpt;
      vec3 lambert = color * min(max(dot(normal, normalize(direction - pos)), 0.0f), 1.0f) * lightColor * 3.0;
      outpt += lambert * softshadow(pos, normalize(direction - pos), 0.01, 3.0, 3.0);
      return outpt;
    }

    vec3 applyFog( in vec3  rgb,       // original color of the pixel
      in float dist ) // camera to point distance
      {
        float fogAmount = 1.0 - exp( -dist* 0.1);
        vec3  fogColor  = vec3(0.5,0.6,0.7);
        return mix( rgb, fogColor, fogAmount );
      }

      vec3 Bokeh(vec3 color, vec2 uv, float radius)
      {
        vec3 acc = vec3(0), div = acc;
        float r = 1.;
        vec2 vangle = vec2(0.0,radius*.01 / sqrt(float(150)));

        for (int j = 0; j < 150; j++)
        {
          // the approx increase in the scale of sqrt(0, 1, 2, 3...)
          r += 1. / r;
          vangle = rot * vangle;
          vec3 col = color;
          //col = color * color *1.8; // ... Contrast it for better highlights - leave this out elsewhere.
          vec3 bokeh = pow(col, vec3(1));
          acc += col * bokeh;
          div += bokeh;
        }
        return acc / div;
      }

      void main() {
        vec2 uv = fs_Pos.xy;
        uv.y += 0.5;
        //uv.y *= u_Dimensions.y / u_Dimensions.x;

        vec4 color = vec4(0.0);
        float t_back = 0.01;
        vec3 p = eye + t_back * rayCast();
        color = vec4(render(uv), 1.);
        //color = pow(color, vec4(1.0f /2.2f));
        vec3 ro = eye;
        vec3 rd = rayCast();
        float t1 = 1.0;
        float t = 0.0;
        //float id = intersect(ro.xyz, rd, t1, t);

        vec3 col;
        vec4 lights[3];
        vec3 lightColor[3];

        // Light positions with intensity as w-component
        lights[0] = vec4(-10.0, 10.0, -10.0, 1.0); //vec4(6.0, 3.0, 5.0, 2.0); // key light
        lights[1] = vec4(-3.0, 3.0, 3.0, 0.5); //vec4(-6.0, 3.0, 5.0, 1.5); // fill light
        lights[2] = vec4(-10.0, 15.0, -20.0, 1.0); //vec4(6.0, 5.0, -1.75, 4.0); // back light


        lightColor[0] = vec3(1.0, 1.0, .2);
        lightColor[1] = rgb(173,216,230);
        lightColor[2] = vec3(0.9, 0.5, 0.9);

        vec2 res1 = castRay_buildingOne(ro,rd, t1);
        float t_1 = res1.x;
        float m = res1.y;
        vec3 sum = vec3(0.0);
        float dist;

        if(m > -0.2)
        {
          vec3 pos = ro + t_1 *rd;
          vec3 nor = estimateNormal_buildingOne(pos);
          dist = distance(eye, pos);
          sum = vec3(0.0);
          for (int i = 0; i < 3; i++) { // Compute lambert for each light
            sum += lambert(nor, lights[i].xyz, rgb(225,247,213), lightColor[i], pos);

          }
          col = sum / 3.0; // Average resulting color
          color = vec4(col, 1.0);

          if (m > 3.0) {
            float u = 2.0;
            // Uncomment to animate
            // t = abs(1.0-sin(u_time*.1))*5.;
            // Comment and uncomment the following lines:
            //uv += noise(uv*2.)*u; // Animate the coordinate spacev
            vec3 pal = palette(uv.x, vec3(0.5, 0.5, 0.5), vec3(0.5, 0.5, 0.5),
            vec3(1.0, 1.0, 1.0), vec3(0.0, 0.10, 0.20));
            // vec3 c = pal * 0.4; // initial colors
            // c += smoothstep(.15,.2,noise(uv*60.)); // Black splatter
            // c -= smoothstep(.35,.4,noise(uv*60.)); // Holes on splatter
            pal = pal * 0.6;
            vec2 pos_sand = vec2(uv*60.0);
            vec3 c = vec3(noise(pos_sand)*.5+.5 + pal) * .5;

            vec3 sum1 = vec3(0.0);
            for (int i = 0; i < 3; i++) { // Compute lambert for each light
              sum1 += lambert(nor, lights[i].xyz, c, lightColor[i], pos);

            }
            c = sum1 / 3.0;
            color = vec4(c, 0.6);
          }
          //color = vec4(1.0);
          vec3 fog = applyFog(color.xyz, dist);
          vec3 finalCol = color.xyz *(1.0-exp(-dist*0.2)) + fog*exp(-dist*0.2);
          color = vec4(finalCol, 1.);
        }

        vec2 res2 = castRay_buildingTwo(ro,rd, t1);
        float t2 = res2.x;
        float m2 = res2.y;

        if (m2 > -0.2) {
          vec3 pos2 = ro + t2 *rd;
          dist = distance(eye, pos2);
          vec3 nor2 = estimateNormal_buildingTwo(pos2);
          sum = vec3(0.0);
          for (int i = 0; i < 3; i++) { // Compute lambert for each light
            sum += lambert(nor2, lights[i].xyz, rgb(201,201,255), lightColor[i], pos2);

          }
          col = sum / 3.0; // Average resulting color
          color = vec4(col, 1.0);
          vec3 fog = applyFog(color.xyz, dist);
          vec3 finalCol = color.xyz *(1.0-exp(-dist*0.2)) + fog*exp(-dist*0.2);
          color = vec4(finalCol, 1.);
        }

        vec2 res3 = castRay_buildingThree(ro,rd, t1);
        float t3 = res3.x;
        float m3 = res3.y;

        if (m3 > -0.2) {
          vec3 pos3 = ro + t3 *rd;
          dist = distance(eye, pos3);
          vec3 nor3 = estimateNormal_buildingThree(pos3);
          sum = vec3(0.0);
          for (int i = 0; i < 3; i++) { // Compute lambert for each light
            if (pos3.z > 1.56) {
              sum += lambert(nor3, lights[i].xyz, rgb(241,203,255), lightColor[i], pos3);
            }

            else {
              sum += lambert(nor3, lights[i].xyz, rgb(255,189,189), lightColor[i], pos3);
            }
          }
          col = sum / 3.0; // Average resulting color
          color = vec4(col, 1.0);
          vec3 fog = applyFog(color.xyz, dist);
          vec3 finalCol = color.xyz *(1.0-exp(-dist*0.2)) + fog*exp(-dist*0.2);
          color = vec4(finalCol, 1.);
        }

        // vignette
        vec2 coord = (uv - 0.5) * (u_Dimensions.x/u_Dimensions.y) * 1.3;
        float rf = sqrt(dot(coord, coord)) * 0.12;
        float rf2_1 = rf * rf + 1.0;
        float e = 1.0 / (rf2_1 * rf2_1);
        color = vec4(color.xyz * e, color.w);

        //glitter (incorporating noise)
        float result = 0.0;
        result += rand(uv * 0.6 + 0.005 * vec2(u_Time*-0.00003));
        result *= rand(uv * 0.09 + 0.002 * vec2(u_Time*+0.002));
        result = pow(result, 50.0);
        color = color + result;
        out_Col = color;
      }

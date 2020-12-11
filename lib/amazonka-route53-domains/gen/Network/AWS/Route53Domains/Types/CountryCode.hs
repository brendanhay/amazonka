-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.CountryCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.CountryCode
  ( CountryCode
      ( CountryCode',
        AD,
        AE,
        AF,
        AG,
        AI,
        AL,
        AM,
        AN,
        AO,
        AQ,
        AR,
        AS,
        AT,
        AU,
        AW,
        AZ,
        BA,
        BB,
        BD,
        BE,
        BF,
        BG,
        BH,
        BI,
        BJ,
        BL,
        BM,
        BN,
        BO,
        BR,
        BS,
        BT,
        BW,
        BY,
        BZ,
        CA,
        CC,
        CD,
        CF,
        CG,
        CH,
        CI,
        CK,
        CL,
        CM,
        CN,
        CO,
        CR,
        CU,
        CV,
        CX,
        CY,
        CZ,
        DE,
        DJ,
        DK,
        DM,
        DO,
        DZ,
        EC,
        EE,
        EG,
        ER,
        ES,
        ET,
        FI,
        FJ,
        FK,
        FM,
        FO,
        FR,
        GA,
        GB,
        GD,
        GE,
        GH,
        GI,
        GL,
        GM,
        GN,
        GQ,
        GR,
        GT,
        GU,
        GW,
        GY,
        HK,
        HN,
        HR,
        HT,
        HU,
        IE,
        IL,
        IM,
        IN,
        IQ,
        IR,
        IS,
        IT,
        Id,
        JM,
        JO,
        JP,
        KE,
        KG,
        KH,
        KI,
        KM,
        KN,
        KP,
        KR,
        KW,
        KY,
        KZ,
        LA,
        LB,
        LC,
        LI,
        LK,
        LR,
        LS,
        LT,
        LU,
        LV,
        LY,
        MA,
        MC,
        MD,
        ME,
        MF,
        MG,
        MH,
        MK,
        ML,
        MM,
        MN,
        MO,
        MP,
        MR,
        MS,
        MT,
        MU,
        MV,
        MW,
        MX,
        MY,
        MZ,
        NA,
        NC,
        NE,
        NG,
        NI,
        NL,
        NO,
        NP,
        NR,
        NU,
        NZ,
        OM,
        PA,
        PE,
        PF,
        PG,
        PH,
        PK,
        PL,
        PM,
        PN,
        PR,
        PT,
        PW,
        PY,
        QA,
        RO,
        RS,
        RU,
        RW,
        SA,
        SB,
        SC,
        SD,
        SE,
        SG,
        SH,
        SI,
        SK,
        SL,
        SM,
        SN,
        SO,
        SR,
        ST,
        SV,
        SY,
        SZ,
        TC,
        TD,
        TG,
        TH,
        TJ,
        TK,
        TL,
        TM,
        TN,
        TO,
        TR,
        TT,
        TV,
        TW,
        TZ,
        UA,
        UG,
        US,
        UY,
        UZ,
        VA,
        VC,
        VE,
        VG,
        VI,
        VN,
        VU,
        WF,
        WS,
        YE,
        YT,
        ZA,
        ZM,
        ZW
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CountryCode = CountryCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AD :: CountryCode
pattern AD = CountryCode' "AD"

pattern AE :: CountryCode
pattern AE = CountryCode' "AE"

pattern AF :: CountryCode
pattern AF = CountryCode' "AF"

pattern AG :: CountryCode
pattern AG = CountryCode' "AG"

pattern AI :: CountryCode
pattern AI = CountryCode' "AI"

pattern AL :: CountryCode
pattern AL = CountryCode' "AL"

pattern AM :: CountryCode
pattern AM = CountryCode' "AM"

pattern AN :: CountryCode
pattern AN = CountryCode' "AN"

pattern AO :: CountryCode
pattern AO = CountryCode' "AO"

pattern AQ :: CountryCode
pattern AQ = CountryCode' "AQ"

pattern AR :: CountryCode
pattern AR = CountryCode' "AR"

pattern AS :: CountryCode
pattern AS = CountryCode' "AS"

pattern AT :: CountryCode
pattern AT = CountryCode' "AT"

pattern AU :: CountryCode
pattern AU = CountryCode' "AU"

pattern AW :: CountryCode
pattern AW = CountryCode' "AW"

pattern AZ :: CountryCode
pattern AZ = CountryCode' "AZ"

pattern BA :: CountryCode
pattern BA = CountryCode' "BA"

pattern BB :: CountryCode
pattern BB = CountryCode' "BB"

pattern BD :: CountryCode
pattern BD = CountryCode' "BD"

pattern BE :: CountryCode
pattern BE = CountryCode' "BE"

pattern BF :: CountryCode
pattern BF = CountryCode' "BF"

pattern BG :: CountryCode
pattern BG = CountryCode' "BG"

pattern BH :: CountryCode
pattern BH = CountryCode' "BH"

pattern BI :: CountryCode
pattern BI = CountryCode' "BI"

pattern BJ :: CountryCode
pattern BJ = CountryCode' "BJ"

pattern BL :: CountryCode
pattern BL = CountryCode' "BL"

pattern BM :: CountryCode
pattern BM = CountryCode' "BM"

pattern BN :: CountryCode
pattern BN = CountryCode' "BN"

pattern BO :: CountryCode
pattern BO = CountryCode' "BO"

pattern BR :: CountryCode
pattern BR = CountryCode' "BR"

pattern BS :: CountryCode
pattern BS = CountryCode' "BS"

pattern BT :: CountryCode
pattern BT = CountryCode' "BT"

pattern BW :: CountryCode
pattern BW = CountryCode' "BW"

pattern BY :: CountryCode
pattern BY = CountryCode' "BY"

pattern BZ :: CountryCode
pattern BZ = CountryCode' "BZ"

pattern CA :: CountryCode
pattern CA = CountryCode' "CA"

pattern CC :: CountryCode
pattern CC = CountryCode' "CC"

pattern CD :: CountryCode
pattern CD = CountryCode' "CD"

pattern CF :: CountryCode
pattern CF = CountryCode' "CF"

pattern CG :: CountryCode
pattern CG = CountryCode' "CG"

pattern CH :: CountryCode
pattern CH = CountryCode' "CH"

pattern CI :: CountryCode
pattern CI = CountryCode' "CI"

pattern CK :: CountryCode
pattern CK = CountryCode' "CK"

pattern CL :: CountryCode
pattern CL = CountryCode' "CL"

pattern CM :: CountryCode
pattern CM = CountryCode' "CM"

pattern CN :: CountryCode
pattern CN = CountryCode' "CN"

pattern CO :: CountryCode
pattern CO = CountryCode' "CO"

pattern CR :: CountryCode
pattern CR = CountryCode' "CR"

pattern CU :: CountryCode
pattern CU = CountryCode' "CU"

pattern CV :: CountryCode
pattern CV = CountryCode' "CV"

pattern CX :: CountryCode
pattern CX = CountryCode' "CX"

pattern CY :: CountryCode
pattern CY = CountryCode' "CY"

pattern CZ :: CountryCode
pattern CZ = CountryCode' "CZ"

pattern DE :: CountryCode
pattern DE = CountryCode' "DE"

pattern DJ :: CountryCode
pattern DJ = CountryCode' "DJ"

pattern DK :: CountryCode
pattern DK = CountryCode' "DK"

pattern DM :: CountryCode
pattern DM = CountryCode' "DM"

pattern DO :: CountryCode
pattern DO = CountryCode' "DO"

pattern DZ :: CountryCode
pattern DZ = CountryCode' "DZ"

pattern EC :: CountryCode
pattern EC = CountryCode' "EC"

pattern EE :: CountryCode
pattern EE = CountryCode' "EE"

pattern EG :: CountryCode
pattern EG = CountryCode' "EG"

pattern ER :: CountryCode
pattern ER = CountryCode' "ER"

pattern ES :: CountryCode
pattern ES = CountryCode' "ES"

pattern ET :: CountryCode
pattern ET = CountryCode' "ET"

pattern FI :: CountryCode
pattern FI = CountryCode' "FI"

pattern FJ :: CountryCode
pattern FJ = CountryCode' "FJ"

pattern FK :: CountryCode
pattern FK = CountryCode' "FK"

pattern FM :: CountryCode
pattern FM = CountryCode' "FM"

pattern FO :: CountryCode
pattern FO = CountryCode' "FO"

pattern FR :: CountryCode
pattern FR = CountryCode' "FR"

pattern GA :: CountryCode
pattern GA = CountryCode' "GA"

pattern GB :: CountryCode
pattern GB = CountryCode' "GB"

pattern GD :: CountryCode
pattern GD = CountryCode' "GD"

pattern GE :: CountryCode
pattern GE = CountryCode' "GE"

pattern GH :: CountryCode
pattern GH = CountryCode' "GH"

pattern GI :: CountryCode
pattern GI = CountryCode' "GI"

pattern GL :: CountryCode
pattern GL = CountryCode' "GL"

pattern GM :: CountryCode
pattern GM = CountryCode' "GM"

pattern GN :: CountryCode
pattern GN = CountryCode' "GN"

pattern GQ :: CountryCode
pattern GQ = CountryCode' "GQ"

pattern GR :: CountryCode
pattern GR = CountryCode' "GR"

pattern GT :: CountryCode
pattern GT = CountryCode' "GT"

pattern GU :: CountryCode
pattern GU = CountryCode' "GU"

pattern GW :: CountryCode
pattern GW = CountryCode' "GW"

pattern GY :: CountryCode
pattern GY = CountryCode' "GY"

pattern HK :: CountryCode
pattern HK = CountryCode' "HK"

pattern HN :: CountryCode
pattern HN = CountryCode' "HN"

pattern HR :: CountryCode
pattern HR = CountryCode' "HR"

pattern HT :: CountryCode
pattern HT = CountryCode' "HT"

pattern HU :: CountryCode
pattern HU = CountryCode' "HU"

pattern IE :: CountryCode
pattern IE = CountryCode' "IE"

pattern IL :: CountryCode
pattern IL = CountryCode' "IL"

pattern IM :: CountryCode
pattern IM = CountryCode' "IM"

pattern IN :: CountryCode
pattern IN = CountryCode' "IN"

pattern IQ :: CountryCode
pattern IQ = CountryCode' "IQ"

pattern IR :: CountryCode
pattern IR = CountryCode' "IR"

pattern IS :: CountryCode
pattern IS = CountryCode' "IS"

pattern IT :: CountryCode
pattern IT = CountryCode' "IT"

pattern Id :: CountryCode
pattern Id = CountryCode' "ID"

pattern JM :: CountryCode
pattern JM = CountryCode' "JM"

pattern JO :: CountryCode
pattern JO = CountryCode' "JO"

pattern JP :: CountryCode
pattern JP = CountryCode' "JP"

pattern KE :: CountryCode
pattern KE = CountryCode' "KE"

pattern KG :: CountryCode
pattern KG = CountryCode' "KG"

pattern KH :: CountryCode
pattern KH = CountryCode' "KH"

pattern KI :: CountryCode
pattern KI = CountryCode' "KI"

pattern KM :: CountryCode
pattern KM = CountryCode' "KM"

pattern KN :: CountryCode
pattern KN = CountryCode' "KN"

pattern KP :: CountryCode
pattern KP = CountryCode' "KP"

pattern KR :: CountryCode
pattern KR = CountryCode' "KR"

pattern KW :: CountryCode
pattern KW = CountryCode' "KW"

pattern KY :: CountryCode
pattern KY = CountryCode' "KY"

pattern KZ :: CountryCode
pattern KZ = CountryCode' "KZ"

pattern LA :: CountryCode
pattern LA = CountryCode' "LA"

pattern LB :: CountryCode
pattern LB = CountryCode' "LB"

pattern LC :: CountryCode
pattern LC = CountryCode' "LC"

pattern LI :: CountryCode
pattern LI = CountryCode' "LI"

pattern LK :: CountryCode
pattern LK = CountryCode' "LK"

pattern LR :: CountryCode
pattern LR = CountryCode' "LR"

pattern LS :: CountryCode
pattern LS = CountryCode' "LS"

pattern LT :: CountryCode
pattern LT = CountryCode' "LT"

pattern LU :: CountryCode
pattern LU = CountryCode' "LU"

pattern LV :: CountryCode
pattern LV = CountryCode' "LV"

pattern LY :: CountryCode
pattern LY = CountryCode' "LY"

pattern MA :: CountryCode
pattern MA = CountryCode' "MA"

pattern MC :: CountryCode
pattern MC = CountryCode' "MC"

pattern MD :: CountryCode
pattern MD = CountryCode' "MD"

pattern ME :: CountryCode
pattern ME = CountryCode' "ME"

pattern MF :: CountryCode
pattern MF = CountryCode' "MF"

pattern MG :: CountryCode
pattern MG = CountryCode' "MG"

pattern MH :: CountryCode
pattern MH = CountryCode' "MH"

pattern MK :: CountryCode
pattern MK = CountryCode' "MK"

pattern ML :: CountryCode
pattern ML = CountryCode' "ML"

pattern MM :: CountryCode
pattern MM = CountryCode' "MM"

pattern MN :: CountryCode
pattern MN = CountryCode' "MN"

pattern MO :: CountryCode
pattern MO = CountryCode' "MO"

pattern MP :: CountryCode
pattern MP = CountryCode' "MP"

pattern MR :: CountryCode
pattern MR = CountryCode' "MR"

pattern MS :: CountryCode
pattern MS = CountryCode' "MS"

pattern MT :: CountryCode
pattern MT = CountryCode' "MT"

pattern MU :: CountryCode
pattern MU = CountryCode' "MU"

pattern MV :: CountryCode
pattern MV = CountryCode' "MV"

pattern MW :: CountryCode
pattern MW = CountryCode' "MW"

pattern MX :: CountryCode
pattern MX = CountryCode' "MX"

pattern MY :: CountryCode
pattern MY = CountryCode' "MY"

pattern MZ :: CountryCode
pattern MZ = CountryCode' "MZ"

pattern NA :: CountryCode
pattern NA = CountryCode' "NA"

pattern NC :: CountryCode
pattern NC = CountryCode' "NC"

pattern NE :: CountryCode
pattern NE = CountryCode' "NE"

pattern NG :: CountryCode
pattern NG = CountryCode' "NG"

pattern NI :: CountryCode
pattern NI = CountryCode' "NI"

pattern NL :: CountryCode
pattern NL = CountryCode' "NL"

pattern NO :: CountryCode
pattern NO = CountryCode' "NO"

pattern NP :: CountryCode
pattern NP = CountryCode' "NP"

pattern NR :: CountryCode
pattern NR = CountryCode' "NR"

pattern NU :: CountryCode
pattern NU = CountryCode' "NU"

pattern NZ :: CountryCode
pattern NZ = CountryCode' "NZ"

pattern OM :: CountryCode
pattern OM = CountryCode' "OM"

pattern PA :: CountryCode
pattern PA = CountryCode' "PA"

pattern PE :: CountryCode
pattern PE = CountryCode' "PE"

pattern PF :: CountryCode
pattern PF = CountryCode' "PF"

pattern PG :: CountryCode
pattern PG = CountryCode' "PG"

pattern PH :: CountryCode
pattern PH = CountryCode' "PH"

pattern PK :: CountryCode
pattern PK = CountryCode' "PK"

pattern PL :: CountryCode
pattern PL = CountryCode' "PL"

pattern PM :: CountryCode
pattern PM = CountryCode' "PM"

pattern PN :: CountryCode
pattern PN = CountryCode' "PN"

pattern PR :: CountryCode
pattern PR = CountryCode' "PR"

pattern PT :: CountryCode
pattern PT = CountryCode' "PT"

pattern PW :: CountryCode
pattern PW = CountryCode' "PW"

pattern PY :: CountryCode
pattern PY = CountryCode' "PY"

pattern QA :: CountryCode
pattern QA = CountryCode' "QA"

pattern RO :: CountryCode
pattern RO = CountryCode' "RO"

pattern RS :: CountryCode
pattern RS = CountryCode' "RS"

pattern RU :: CountryCode
pattern RU = CountryCode' "RU"

pattern RW :: CountryCode
pattern RW = CountryCode' "RW"

pattern SA :: CountryCode
pattern SA = CountryCode' "SA"

pattern SB :: CountryCode
pattern SB = CountryCode' "SB"

pattern SC :: CountryCode
pattern SC = CountryCode' "SC"

pattern SD :: CountryCode
pattern SD = CountryCode' "SD"

pattern SE :: CountryCode
pattern SE = CountryCode' "SE"

pattern SG :: CountryCode
pattern SG = CountryCode' "SG"

pattern SH :: CountryCode
pattern SH = CountryCode' "SH"

pattern SI :: CountryCode
pattern SI = CountryCode' "SI"

pattern SK :: CountryCode
pattern SK = CountryCode' "SK"

pattern SL :: CountryCode
pattern SL = CountryCode' "SL"

pattern SM :: CountryCode
pattern SM = CountryCode' "SM"

pattern SN :: CountryCode
pattern SN = CountryCode' "SN"

pattern SO :: CountryCode
pattern SO = CountryCode' "SO"

pattern SR :: CountryCode
pattern SR = CountryCode' "SR"

pattern ST :: CountryCode
pattern ST = CountryCode' "ST"

pattern SV :: CountryCode
pattern SV = CountryCode' "SV"

pattern SY :: CountryCode
pattern SY = CountryCode' "SY"

pattern SZ :: CountryCode
pattern SZ = CountryCode' "SZ"

pattern TC :: CountryCode
pattern TC = CountryCode' "TC"

pattern TD :: CountryCode
pattern TD = CountryCode' "TD"

pattern TG :: CountryCode
pattern TG = CountryCode' "TG"

pattern TH :: CountryCode
pattern TH = CountryCode' "TH"

pattern TJ :: CountryCode
pattern TJ = CountryCode' "TJ"

pattern TK :: CountryCode
pattern TK = CountryCode' "TK"

pattern TL :: CountryCode
pattern TL = CountryCode' "TL"

pattern TM :: CountryCode
pattern TM = CountryCode' "TM"

pattern TN :: CountryCode
pattern TN = CountryCode' "TN"

pattern TO :: CountryCode
pattern TO = CountryCode' "TO"

pattern TR :: CountryCode
pattern TR = CountryCode' "TR"

pattern TT :: CountryCode
pattern TT = CountryCode' "TT"

pattern TV :: CountryCode
pattern TV = CountryCode' "TV"

pattern TW :: CountryCode
pattern TW = CountryCode' "TW"

pattern TZ :: CountryCode
pattern TZ = CountryCode' "TZ"

pattern UA :: CountryCode
pattern UA = CountryCode' "UA"

pattern UG :: CountryCode
pattern UG = CountryCode' "UG"

pattern US :: CountryCode
pattern US = CountryCode' "US"

pattern UY :: CountryCode
pattern UY = CountryCode' "UY"

pattern UZ :: CountryCode
pattern UZ = CountryCode' "UZ"

pattern VA :: CountryCode
pattern VA = CountryCode' "VA"

pattern VC :: CountryCode
pattern VC = CountryCode' "VC"

pattern VE :: CountryCode
pattern VE = CountryCode' "VE"

pattern VG :: CountryCode
pattern VG = CountryCode' "VG"

pattern VI :: CountryCode
pattern VI = CountryCode' "VI"

pattern VN :: CountryCode
pattern VN = CountryCode' "VN"

pattern VU :: CountryCode
pattern VU = CountryCode' "VU"

pattern WF :: CountryCode
pattern WF = CountryCode' "WF"

pattern WS :: CountryCode
pattern WS = CountryCode' "WS"

pattern YE :: CountryCode
pattern YE = CountryCode' "YE"

pattern YT :: CountryCode
pattern YT = CountryCode' "YT"

pattern ZA :: CountryCode
pattern ZA = CountryCode' "ZA"

pattern ZM :: CountryCode
pattern ZM = CountryCode' "ZM"

pattern ZW :: CountryCode
pattern ZW = CountryCode' "ZW"

{-# COMPLETE
  AD,
  AE,
  AF,
  AG,
  AI,
  AL,
  AM,
  AN,
  AO,
  AQ,
  AR,
  AS,
  AT,
  AU,
  AW,
  AZ,
  BA,
  BB,
  BD,
  BE,
  BF,
  BG,
  BH,
  BI,
  BJ,
  BL,
  BM,
  BN,
  BO,
  BR,
  BS,
  BT,
  BW,
  BY,
  BZ,
  CA,
  CC,
  CD,
  CF,
  CG,
  CH,
  CI,
  CK,
  CL,
  CM,
  CN,
  CO,
  CR,
  CU,
  CV,
  CX,
  CY,
  CZ,
  DE,
  DJ,
  DK,
  DM,
  DO,
  DZ,
  EC,
  EE,
  EG,
  ER,
  ES,
  ET,
  FI,
  FJ,
  FK,
  FM,
  FO,
  FR,
  GA,
  GB,
  GD,
  GE,
  GH,
  GI,
  GL,
  GM,
  GN,
  GQ,
  GR,
  GT,
  GU,
  GW,
  GY,
  HK,
  HN,
  HR,
  HT,
  HU,
  IE,
  IL,
  IM,
  IN,
  IQ,
  IR,
  IS,
  IT,
  Id,
  JM,
  JO,
  JP,
  KE,
  KG,
  KH,
  KI,
  KM,
  KN,
  KP,
  KR,
  KW,
  KY,
  KZ,
  LA,
  LB,
  LC,
  LI,
  LK,
  LR,
  LS,
  LT,
  LU,
  LV,
  LY,
  MA,
  MC,
  MD,
  ME,
  MF,
  MG,
  MH,
  MK,
  ML,
  MM,
  MN,
  MO,
  MP,
  MR,
  MS,
  MT,
  MU,
  MV,
  MW,
  MX,
  MY,
  MZ,
  NA,
  NC,
  NE,
  NG,
  NI,
  NL,
  NO,
  NP,
  NR,
  NU,
  NZ,
  OM,
  PA,
  PE,
  PF,
  PG,
  PH,
  PK,
  PL,
  PM,
  PN,
  PR,
  PT,
  PW,
  PY,
  QA,
  RO,
  RS,
  RU,
  RW,
  SA,
  SB,
  SC,
  SD,
  SE,
  SG,
  SH,
  SI,
  SK,
  SL,
  SM,
  SN,
  SO,
  SR,
  ST,
  SV,
  SY,
  SZ,
  TC,
  TD,
  TG,
  TH,
  TJ,
  TK,
  TL,
  TM,
  TN,
  TO,
  TR,
  TT,
  TV,
  TW,
  TZ,
  UA,
  UG,
  US,
  UY,
  UZ,
  VA,
  VC,
  VE,
  VG,
  VI,
  VN,
  VU,
  WF,
  WS,
  YE,
  YT,
  ZA,
  ZM,
  ZW,
  CountryCode'
  #-}

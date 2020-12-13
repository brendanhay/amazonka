{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberCountryCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberCountryCode
  ( PhoneNumberCountryCode
      ( PhoneNumberCountryCode',
        AF,
        AL,
        DZ,
        AS,
        AD,
        AO,
        AI,
        AQ,
        AG,
        AR,
        AM,
        AW,
        AU,
        AT,
        AZ,
        BS,
        BH,
        BD,
        BB,
        BY,
        BE,
        BZ,
        BJ,
        BM,
        BT,
        BO,
        BA,
        BW,
        BR,
        IO,
        VG,
        BN,
        BG,
        BF,
        BI,
        KH,
        CM,
        CA,
        CV,
        KY,
        CF,
        TD,
        CL,
        CN,
        CX,
        CC,
        CO,
        KM,
        CK,
        CR,
        HR,
        CU,
        CW,
        CY,
        CZ,
        CD,
        DK,
        DJ,
        DM,
        DO,
        TL,
        EC,
        EG,
        SV,
        GQ,
        ER,
        EE,
        ET,
        FK,
        FO,
        FJ,
        FI,
        FR,
        PF,
        GA,
        GM,
        GE,
        DE,
        GH,
        GI,
        GR,
        GL,
        GD,
        GU,
        GT,
        GG,
        GN,
        GW,
        GY,
        HT,
        HN,
        HK,
        HU,
        IS,
        IN,
        Id,
        IR,
        IQ,
        IE,
        IM,
        IL,
        IT,
        CI,
        JM,
        JP,
        JE,
        JO,
        KZ,
        KE,
        KI,
        KW,
        KG,
        LA,
        LV,
        LB,
        LS,
        LR,
        LY,
        LI,
        LT,
        LU,
        MO,
        MK,
        MG,
        MW,
        MY,
        MV,
        ML,
        MT,
        MH,
        MR,
        MU,
        YT,
        MX,
        FM,
        MD,
        MC,
        MN,
        ME,
        MS,
        MA,
        MZ,
        MM,
        NA,
        NR,
        NP,
        NL,
        AN,
        NC,
        NZ,
        NI,
        NE,
        NG,
        NU,
        KP,
        MP,
        NO,
        OM,
        PK,
        PW,
        PA,
        PG,
        PY,
        PE,
        PH,
        PN,
        PL,
        PT,
        PR,
        QA,
        CG,
        RE,
        RO,
        RU,
        RW,
        BL,
        SH,
        KN,
        LC,
        MF,
        PM,
        VC,
        WS,
        SM,
        ST,
        SA,
        SN,
        RS,
        SC,
        SL,
        SG,
        SX,
        SK,
        SI,
        SB,
        SO,
        ZA,
        KR,
        ES,
        LK,
        SD,
        SR,
        SJ,
        SZ,
        SE,
        CH,
        SY,
        TW,
        TJ,
        TZ,
        TH,
        TG,
        TK,
        TO,
        TT,
        TN,
        TR,
        TM,
        TC,
        TV,
        VI,
        UG,
        UA,
        AE,
        GB,
        US,
        UY,
        UZ,
        VU,
        VA,
        VE,
        VN,
        WF,
        EH,
        YE,
        ZM,
        ZW
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PhoneNumberCountryCode = PhoneNumberCountryCode' Lude.Text
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

pattern AF :: PhoneNumberCountryCode
pattern AF = PhoneNumberCountryCode' "AF"

pattern AL :: PhoneNumberCountryCode
pattern AL = PhoneNumberCountryCode' "AL"

pattern DZ :: PhoneNumberCountryCode
pattern DZ = PhoneNumberCountryCode' "DZ"

pattern AS :: PhoneNumberCountryCode
pattern AS = PhoneNumberCountryCode' "AS"

pattern AD :: PhoneNumberCountryCode
pattern AD = PhoneNumberCountryCode' "AD"

pattern AO :: PhoneNumberCountryCode
pattern AO = PhoneNumberCountryCode' "AO"

pattern AI :: PhoneNumberCountryCode
pattern AI = PhoneNumberCountryCode' "AI"

pattern AQ :: PhoneNumberCountryCode
pattern AQ = PhoneNumberCountryCode' "AQ"

pattern AG :: PhoneNumberCountryCode
pattern AG = PhoneNumberCountryCode' "AG"

pattern AR :: PhoneNumberCountryCode
pattern AR = PhoneNumberCountryCode' "AR"

pattern AM :: PhoneNumberCountryCode
pattern AM = PhoneNumberCountryCode' "AM"

pattern AW :: PhoneNumberCountryCode
pattern AW = PhoneNumberCountryCode' "AW"

pattern AU :: PhoneNumberCountryCode
pattern AU = PhoneNumberCountryCode' "AU"

pattern AT :: PhoneNumberCountryCode
pattern AT = PhoneNumberCountryCode' "AT"

pattern AZ :: PhoneNumberCountryCode
pattern AZ = PhoneNumberCountryCode' "AZ"

pattern BS :: PhoneNumberCountryCode
pattern BS = PhoneNumberCountryCode' "BS"

pattern BH :: PhoneNumberCountryCode
pattern BH = PhoneNumberCountryCode' "BH"

pattern BD :: PhoneNumberCountryCode
pattern BD = PhoneNumberCountryCode' "BD"

pattern BB :: PhoneNumberCountryCode
pattern BB = PhoneNumberCountryCode' "BB"

pattern BY :: PhoneNumberCountryCode
pattern BY = PhoneNumberCountryCode' "BY"

pattern BE :: PhoneNumberCountryCode
pattern BE = PhoneNumberCountryCode' "BE"

pattern BZ :: PhoneNumberCountryCode
pattern BZ = PhoneNumberCountryCode' "BZ"

pattern BJ :: PhoneNumberCountryCode
pattern BJ = PhoneNumberCountryCode' "BJ"

pattern BM :: PhoneNumberCountryCode
pattern BM = PhoneNumberCountryCode' "BM"

pattern BT :: PhoneNumberCountryCode
pattern BT = PhoneNumberCountryCode' "BT"

pattern BO :: PhoneNumberCountryCode
pattern BO = PhoneNumberCountryCode' "BO"

pattern BA :: PhoneNumberCountryCode
pattern BA = PhoneNumberCountryCode' "BA"

pattern BW :: PhoneNumberCountryCode
pattern BW = PhoneNumberCountryCode' "BW"

pattern BR :: PhoneNumberCountryCode
pattern BR = PhoneNumberCountryCode' "BR"

pattern IO :: PhoneNumberCountryCode
pattern IO = PhoneNumberCountryCode' "IO"

pattern VG :: PhoneNumberCountryCode
pattern VG = PhoneNumberCountryCode' "VG"

pattern BN :: PhoneNumberCountryCode
pattern BN = PhoneNumberCountryCode' "BN"

pattern BG :: PhoneNumberCountryCode
pattern BG = PhoneNumberCountryCode' "BG"

pattern BF :: PhoneNumberCountryCode
pattern BF = PhoneNumberCountryCode' "BF"

pattern BI :: PhoneNumberCountryCode
pattern BI = PhoneNumberCountryCode' "BI"

pattern KH :: PhoneNumberCountryCode
pattern KH = PhoneNumberCountryCode' "KH"

pattern CM :: PhoneNumberCountryCode
pattern CM = PhoneNumberCountryCode' "CM"

pattern CA :: PhoneNumberCountryCode
pattern CA = PhoneNumberCountryCode' "CA"

pattern CV :: PhoneNumberCountryCode
pattern CV = PhoneNumberCountryCode' "CV"

pattern KY :: PhoneNumberCountryCode
pattern KY = PhoneNumberCountryCode' "KY"

pattern CF :: PhoneNumberCountryCode
pattern CF = PhoneNumberCountryCode' "CF"

pattern TD :: PhoneNumberCountryCode
pattern TD = PhoneNumberCountryCode' "TD"

pattern CL :: PhoneNumberCountryCode
pattern CL = PhoneNumberCountryCode' "CL"

pattern CN :: PhoneNumberCountryCode
pattern CN = PhoneNumberCountryCode' "CN"

pattern CX :: PhoneNumberCountryCode
pattern CX = PhoneNumberCountryCode' "CX"

pattern CC :: PhoneNumberCountryCode
pattern CC = PhoneNumberCountryCode' "CC"

pattern CO :: PhoneNumberCountryCode
pattern CO = PhoneNumberCountryCode' "CO"

pattern KM :: PhoneNumberCountryCode
pattern KM = PhoneNumberCountryCode' "KM"

pattern CK :: PhoneNumberCountryCode
pattern CK = PhoneNumberCountryCode' "CK"

pattern CR :: PhoneNumberCountryCode
pattern CR = PhoneNumberCountryCode' "CR"

pattern HR :: PhoneNumberCountryCode
pattern HR = PhoneNumberCountryCode' "HR"

pattern CU :: PhoneNumberCountryCode
pattern CU = PhoneNumberCountryCode' "CU"

pattern CW :: PhoneNumberCountryCode
pattern CW = PhoneNumberCountryCode' "CW"

pattern CY :: PhoneNumberCountryCode
pattern CY = PhoneNumberCountryCode' "CY"

pattern CZ :: PhoneNumberCountryCode
pattern CZ = PhoneNumberCountryCode' "CZ"

pattern CD :: PhoneNumberCountryCode
pattern CD = PhoneNumberCountryCode' "CD"

pattern DK :: PhoneNumberCountryCode
pattern DK = PhoneNumberCountryCode' "DK"

pattern DJ :: PhoneNumberCountryCode
pattern DJ = PhoneNumberCountryCode' "DJ"

pattern DM :: PhoneNumberCountryCode
pattern DM = PhoneNumberCountryCode' "DM"

pattern DO :: PhoneNumberCountryCode
pattern DO = PhoneNumberCountryCode' "DO"

pattern TL :: PhoneNumberCountryCode
pattern TL = PhoneNumberCountryCode' "TL"

pattern EC :: PhoneNumberCountryCode
pattern EC = PhoneNumberCountryCode' "EC"

pattern EG :: PhoneNumberCountryCode
pattern EG = PhoneNumberCountryCode' "EG"

pattern SV :: PhoneNumberCountryCode
pattern SV = PhoneNumberCountryCode' "SV"

pattern GQ :: PhoneNumberCountryCode
pattern GQ = PhoneNumberCountryCode' "GQ"

pattern ER :: PhoneNumberCountryCode
pattern ER = PhoneNumberCountryCode' "ER"

pattern EE :: PhoneNumberCountryCode
pattern EE = PhoneNumberCountryCode' "EE"

pattern ET :: PhoneNumberCountryCode
pattern ET = PhoneNumberCountryCode' "ET"

pattern FK :: PhoneNumberCountryCode
pattern FK = PhoneNumberCountryCode' "FK"

pattern FO :: PhoneNumberCountryCode
pattern FO = PhoneNumberCountryCode' "FO"

pattern FJ :: PhoneNumberCountryCode
pattern FJ = PhoneNumberCountryCode' "FJ"

pattern FI :: PhoneNumberCountryCode
pattern FI = PhoneNumberCountryCode' "FI"

pattern FR :: PhoneNumberCountryCode
pattern FR = PhoneNumberCountryCode' "FR"

pattern PF :: PhoneNumberCountryCode
pattern PF = PhoneNumberCountryCode' "PF"

pattern GA :: PhoneNumberCountryCode
pattern GA = PhoneNumberCountryCode' "GA"

pattern GM :: PhoneNumberCountryCode
pattern GM = PhoneNumberCountryCode' "GM"

pattern GE :: PhoneNumberCountryCode
pattern GE = PhoneNumberCountryCode' "GE"

pattern DE :: PhoneNumberCountryCode
pattern DE = PhoneNumberCountryCode' "DE"

pattern GH :: PhoneNumberCountryCode
pattern GH = PhoneNumberCountryCode' "GH"

pattern GI :: PhoneNumberCountryCode
pattern GI = PhoneNumberCountryCode' "GI"

pattern GR :: PhoneNumberCountryCode
pattern GR = PhoneNumberCountryCode' "GR"

pattern GL :: PhoneNumberCountryCode
pattern GL = PhoneNumberCountryCode' "GL"

pattern GD :: PhoneNumberCountryCode
pattern GD = PhoneNumberCountryCode' "GD"

pattern GU :: PhoneNumberCountryCode
pattern GU = PhoneNumberCountryCode' "GU"

pattern GT :: PhoneNumberCountryCode
pattern GT = PhoneNumberCountryCode' "GT"

pattern GG :: PhoneNumberCountryCode
pattern GG = PhoneNumberCountryCode' "GG"

pattern GN :: PhoneNumberCountryCode
pattern GN = PhoneNumberCountryCode' "GN"

pattern GW :: PhoneNumberCountryCode
pattern GW = PhoneNumberCountryCode' "GW"

pattern GY :: PhoneNumberCountryCode
pattern GY = PhoneNumberCountryCode' "GY"

pattern HT :: PhoneNumberCountryCode
pattern HT = PhoneNumberCountryCode' "HT"

pattern HN :: PhoneNumberCountryCode
pattern HN = PhoneNumberCountryCode' "HN"

pattern HK :: PhoneNumberCountryCode
pattern HK = PhoneNumberCountryCode' "HK"

pattern HU :: PhoneNumberCountryCode
pattern HU = PhoneNumberCountryCode' "HU"

pattern IS :: PhoneNumberCountryCode
pattern IS = PhoneNumberCountryCode' "IS"

pattern IN :: PhoneNumberCountryCode
pattern IN = PhoneNumberCountryCode' "IN"

pattern Id :: PhoneNumberCountryCode
pattern Id = PhoneNumberCountryCode' "ID"

pattern IR :: PhoneNumberCountryCode
pattern IR = PhoneNumberCountryCode' "IR"

pattern IQ :: PhoneNumberCountryCode
pattern IQ = PhoneNumberCountryCode' "IQ"

pattern IE :: PhoneNumberCountryCode
pattern IE = PhoneNumberCountryCode' "IE"

pattern IM :: PhoneNumberCountryCode
pattern IM = PhoneNumberCountryCode' "IM"

pattern IL :: PhoneNumberCountryCode
pattern IL = PhoneNumberCountryCode' "IL"

pattern IT :: PhoneNumberCountryCode
pattern IT = PhoneNumberCountryCode' "IT"

pattern CI :: PhoneNumberCountryCode
pattern CI = PhoneNumberCountryCode' "CI"

pattern JM :: PhoneNumberCountryCode
pattern JM = PhoneNumberCountryCode' "JM"

pattern JP :: PhoneNumberCountryCode
pattern JP = PhoneNumberCountryCode' "JP"

pattern JE :: PhoneNumberCountryCode
pattern JE = PhoneNumberCountryCode' "JE"

pattern JO :: PhoneNumberCountryCode
pattern JO = PhoneNumberCountryCode' "JO"

pattern KZ :: PhoneNumberCountryCode
pattern KZ = PhoneNumberCountryCode' "KZ"

pattern KE :: PhoneNumberCountryCode
pattern KE = PhoneNumberCountryCode' "KE"

pattern KI :: PhoneNumberCountryCode
pattern KI = PhoneNumberCountryCode' "KI"

pattern KW :: PhoneNumberCountryCode
pattern KW = PhoneNumberCountryCode' "KW"

pattern KG :: PhoneNumberCountryCode
pattern KG = PhoneNumberCountryCode' "KG"

pattern LA :: PhoneNumberCountryCode
pattern LA = PhoneNumberCountryCode' "LA"

pattern LV :: PhoneNumberCountryCode
pattern LV = PhoneNumberCountryCode' "LV"

pattern LB :: PhoneNumberCountryCode
pattern LB = PhoneNumberCountryCode' "LB"

pattern LS :: PhoneNumberCountryCode
pattern LS = PhoneNumberCountryCode' "LS"

pattern LR :: PhoneNumberCountryCode
pattern LR = PhoneNumberCountryCode' "LR"

pattern LY :: PhoneNumberCountryCode
pattern LY = PhoneNumberCountryCode' "LY"

pattern LI :: PhoneNumberCountryCode
pattern LI = PhoneNumberCountryCode' "LI"

pattern LT :: PhoneNumberCountryCode
pattern LT = PhoneNumberCountryCode' "LT"

pattern LU :: PhoneNumberCountryCode
pattern LU = PhoneNumberCountryCode' "LU"

pattern MO :: PhoneNumberCountryCode
pattern MO = PhoneNumberCountryCode' "MO"

pattern MK :: PhoneNumberCountryCode
pattern MK = PhoneNumberCountryCode' "MK"

pattern MG :: PhoneNumberCountryCode
pattern MG = PhoneNumberCountryCode' "MG"

pattern MW :: PhoneNumberCountryCode
pattern MW = PhoneNumberCountryCode' "MW"

pattern MY :: PhoneNumberCountryCode
pattern MY = PhoneNumberCountryCode' "MY"

pattern MV :: PhoneNumberCountryCode
pattern MV = PhoneNumberCountryCode' "MV"

pattern ML :: PhoneNumberCountryCode
pattern ML = PhoneNumberCountryCode' "ML"

pattern MT :: PhoneNumberCountryCode
pattern MT = PhoneNumberCountryCode' "MT"

pattern MH :: PhoneNumberCountryCode
pattern MH = PhoneNumberCountryCode' "MH"

pattern MR :: PhoneNumberCountryCode
pattern MR = PhoneNumberCountryCode' "MR"

pattern MU :: PhoneNumberCountryCode
pattern MU = PhoneNumberCountryCode' "MU"

pattern YT :: PhoneNumberCountryCode
pattern YT = PhoneNumberCountryCode' "YT"

pattern MX :: PhoneNumberCountryCode
pattern MX = PhoneNumberCountryCode' "MX"

pattern FM :: PhoneNumberCountryCode
pattern FM = PhoneNumberCountryCode' "FM"

pattern MD :: PhoneNumberCountryCode
pattern MD = PhoneNumberCountryCode' "MD"

pattern MC :: PhoneNumberCountryCode
pattern MC = PhoneNumberCountryCode' "MC"

pattern MN :: PhoneNumberCountryCode
pattern MN = PhoneNumberCountryCode' "MN"

pattern ME :: PhoneNumberCountryCode
pattern ME = PhoneNumberCountryCode' "ME"

pattern MS :: PhoneNumberCountryCode
pattern MS = PhoneNumberCountryCode' "MS"

pattern MA :: PhoneNumberCountryCode
pattern MA = PhoneNumberCountryCode' "MA"

pattern MZ :: PhoneNumberCountryCode
pattern MZ = PhoneNumberCountryCode' "MZ"

pattern MM :: PhoneNumberCountryCode
pattern MM = PhoneNumberCountryCode' "MM"

pattern NA :: PhoneNumberCountryCode
pattern NA = PhoneNumberCountryCode' "NA"

pattern NR :: PhoneNumberCountryCode
pattern NR = PhoneNumberCountryCode' "NR"

pattern NP :: PhoneNumberCountryCode
pattern NP = PhoneNumberCountryCode' "NP"

pattern NL :: PhoneNumberCountryCode
pattern NL = PhoneNumberCountryCode' "NL"

pattern AN :: PhoneNumberCountryCode
pattern AN = PhoneNumberCountryCode' "AN"

pattern NC :: PhoneNumberCountryCode
pattern NC = PhoneNumberCountryCode' "NC"

pattern NZ :: PhoneNumberCountryCode
pattern NZ = PhoneNumberCountryCode' "NZ"

pattern NI :: PhoneNumberCountryCode
pattern NI = PhoneNumberCountryCode' "NI"

pattern NE :: PhoneNumberCountryCode
pattern NE = PhoneNumberCountryCode' "NE"

pattern NG :: PhoneNumberCountryCode
pattern NG = PhoneNumberCountryCode' "NG"

pattern NU :: PhoneNumberCountryCode
pattern NU = PhoneNumberCountryCode' "NU"

pattern KP :: PhoneNumberCountryCode
pattern KP = PhoneNumberCountryCode' "KP"

pattern MP :: PhoneNumberCountryCode
pattern MP = PhoneNumberCountryCode' "MP"

pattern NO :: PhoneNumberCountryCode
pattern NO = PhoneNumberCountryCode' "NO"

pattern OM :: PhoneNumberCountryCode
pattern OM = PhoneNumberCountryCode' "OM"

pattern PK :: PhoneNumberCountryCode
pattern PK = PhoneNumberCountryCode' "PK"

pattern PW :: PhoneNumberCountryCode
pattern PW = PhoneNumberCountryCode' "PW"

pattern PA :: PhoneNumberCountryCode
pattern PA = PhoneNumberCountryCode' "PA"

pattern PG :: PhoneNumberCountryCode
pattern PG = PhoneNumberCountryCode' "PG"

pattern PY :: PhoneNumberCountryCode
pattern PY = PhoneNumberCountryCode' "PY"

pattern PE :: PhoneNumberCountryCode
pattern PE = PhoneNumberCountryCode' "PE"

pattern PH :: PhoneNumberCountryCode
pattern PH = PhoneNumberCountryCode' "PH"

pattern PN :: PhoneNumberCountryCode
pattern PN = PhoneNumberCountryCode' "PN"

pattern PL :: PhoneNumberCountryCode
pattern PL = PhoneNumberCountryCode' "PL"

pattern PT :: PhoneNumberCountryCode
pattern PT = PhoneNumberCountryCode' "PT"

pattern PR :: PhoneNumberCountryCode
pattern PR = PhoneNumberCountryCode' "PR"

pattern QA :: PhoneNumberCountryCode
pattern QA = PhoneNumberCountryCode' "QA"

pattern CG :: PhoneNumberCountryCode
pattern CG = PhoneNumberCountryCode' "CG"

pattern RE :: PhoneNumberCountryCode
pattern RE = PhoneNumberCountryCode' "RE"

pattern RO :: PhoneNumberCountryCode
pattern RO = PhoneNumberCountryCode' "RO"

pattern RU :: PhoneNumberCountryCode
pattern RU = PhoneNumberCountryCode' "RU"

pattern RW :: PhoneNumberCountryCode
pattern RW = PhoneNumberCountryCode' "RW"

pattern BL :: PhoneNumberCountryCode
pattern BL = PhoneNumberCountryCode' "BL"

pattern SH :: PhoneNumberCountryCode
pattern SH = PhoneNumberCountryCode' "SH"

pattern KN :: PhoneNumberCountryCode
pattern KN = PhoneNumberCountryCode' "KN"

pattern LC :: PhoneNumberCountryCode
pattern LC = PhoneNumberCountryCode' "LC"

pattern MF :: PhoneNumberCountryCode
pattern MF = PhoneNumberCountryCode' "MF"

pattern PM :: PhoneNumberCountryCode
pattern PM = PhoneNumberCountryCode' "PM"

pattern VC :: PhoneNumberCountryCode
pattern VC = PhoneNumberCountryCode' "VC"

pattern WS :: PhoneNumberCountryCode
pattern WS = PhoneNumberCountryCode' "WS"

pattern SM :: PhoneNumberCountryCode
pattern SM = PhoneNumberCountryCode' "SM"

pattern ST :: PhoneNumberCountryCode
pattern ST = PhoneNumberCountryCode' "ST"

pattern SA :: PhoneNumberCountryCode
pattern SA = PhoneNumberCountryCode' "SA"

pattern SN :: PhoneNumberCountryCode
pattern SN = PhoneNumberCountryCode' "SN"

pattern RS :: PhoneNumberCountryCode
pattern RS = PhoneNumberCountryCode' "RS"

pattern SC :: PhoneNumberCountryCode
pattern SC = PhoneNumberCountryCode' "SC"

pattern SL :: PhoneNumberCountryCode
pattern SL = PhoneNumberCountryCode' "SL"

pattern SG :: PhoneNumberCountryCode
pattern SG = PhoneNumberCountryCode' "SG"

pattern SX :: PhoneNumberCountryCode
pattern SX = PhoneNumberCountryCode' "SX"

pattern SK :: PhoneNumberCountryCode
pattern SK = PhoneNumberCountryCode' "SK"

pattern SI :: PhoneNumberCountryCode
pattern SI = PhoneNumberCountryCode' "SI"

pattern SB :: PhoneNumberCountryCode
pattern SB = PhoneNumberCountryCode' "SB"

pattern SO :: PhoneNumberCountryCode
pattern SO = PhoneNumberCountryCode' "SO"

pattern ZA :: PhoneNumberCountryCode
pattern ZA = PhoneNumberCountryCode' "ZA"

pattern KR :: PhoneNumberCountryCode
pattern KR = PhoneNumberCountryCode' "KR"

pattern ES :: PhoneNumberCountryCode
pattern ES = PhoneNumberCountryCode' "ES"

pattern LK :: PhoneNumberCountryCode
pattern LK = PhoneNumberCountryCode' "LK"

pattern SD :: PhoneNumberCountryCode
pattern SD = PhoneNumberCountryCode' "SD"

pattern SR :: PhoneNumberCountryCode
pattern SR = PhoneNumberCountryCode' "SR"

pattern SJ :: PhoneNumberCountryCode
pattern SJ = PhoneNumberCountryCode' "SJ"

pattern SZ :: PhoneNumberCountryCode
pattern SZ = PhoneNumberCountryCode' "SZ"

pattern SE :: PhoneNumberCountryCode
pattern SE = PhoneNumberCountryCode' "SE"

pattern CH :: PhoneNumberCountryCode
pattern CH = PhoneNumberCountryCode' "CH"

pattern SY :: PhoneNumberCountryCode
pattern SY = PhoneNumberCountryCode' "SY"

pattern TW :: PhoneNumberCountryCode
pattern TW = PhoneNumberCountryCode' "TW"

pattern TJ :: PhoneNumberCountryCode
pattern TJ = PhoneNumberCountryCode' "TJ"

pattern TZ :: PhoneNumberCountryCode
pattern TZ = PhoneNumberCountryCode' "TZ"

pattern TH :: PhoneNumberCountryCode
pattern TH = PhoneNumberCountryCode' "TH"

pattern TG :: PhoneNumberCountryCode
pattern TG = PhoneNumberCountryCode' "TG"

pattern TK :: PhoneNumberCountryCode
pattern TK = PhoneNumberCountryCode' "TK"

pattern TO :: PhoneNumberCountryCode
pattern TO = PhoneNumberCountryCode' "TO"

pattern TT :: PhoneNumberCountryCode
pattern TT = PhoneNumberCountryCode' "TT"

pattern TN :: PhoneNumberCountryCode
pattern TN = PhoneNumberCountryCode' "TN"

pattern TR :: PhoneNumberCountryCode
pattern TR = PhoneNumberCountryCode' "TR"

pattern TM :: PhoneNumberCountryCode
pattern TM = PhoneNumberCountryCode' "TM"

pattern TC :: PhoneNumberCountryCode
pattern TC = PhoneNumberCountryCode' "TC"

pattern TV :: PhoneNumberCountryCode
pattern TV = PhoneNumberCountryCode' "TV"

pattern VI :: PhoneNumberCountryCode
pattern VI = PhoneNumberCountryCode' "VI"

pattern UG :: PhoneNumberCountryCode
pattern UG = PhoneNumberCountryCode' "UG"

pattern UA :: PhoneNumberCountryCode
pattern UA = PhoneNumberCountryCode' "UA"

pattern AE :: PhoneNumberCountryCode
pattern AE = PhoneNumberCountryCode' "AE"

pattern GB :: PhoneNumberCountryCode
pattern GB = PhoneNumberCountryCode' "GB"

pattern US :: PhoneNumberCountryCode
pattern US = PhoneNumberCountryCode' "US"

pattern UY :: PhoneNumberCountryCode
pattern UY = PhoneNumberCountryCode' "UY"

pattern UZ :: PhoneNumberCountryCode
pattern UZ = PhoneNumberCountryCode' "UZ"

pattern VU :: PhoneNumberCountryCode
pattern VU = PhoneNumberCountryCode' "VU"

pattern VA :: PhoneNumberCountryCode
pattern VA = PhoneNumberCountryCode' "VA"

pattern VE :: PhoneNumberCountryCode
pattern VE = PhoneNumberCountryCode' "VE"

pattern VN :: PhoneNumberCountryCode
pattern VN = PhoneNumberCountryCode' "VN"

pattern WF :: PhoneNumberCountryCode
pattern WF = PhoneNumberCountryCode' "WF"

pattern EH :: PhoneNumberCountryCode
pattern EH = PhoneNumberCountryCode' "EH"

pattern YE :: PhoneNumberCountryCode
pattern YE = PhoneNumberCountryCode' "YE"

pattern ZM :: PhoneNumberCountryCode
pattern ZM = PhoneNumberCountryCode' "ZM"

pattern ZW :: PhoneNumberCountryCode
pattern ZW = PhoneNumberCountryCode' "ZW"

{-# COMPLETE
  AF,
  AL,
  DZ,
  AS,
  AD,
  AO,
  AI,
  AQ,
  AG,
  AR,
  AM,
  AW,
  AU,
  AT,
  AZ,
  BS,
  BH,
  BD,
  BB,
  BY,
  BE,
  BZ,
  BJ,
  BM,
  BT,
  BO,
  BA,
  BW,
  BR,
  IO,
  VG,
  BN,
  BG,
  BF,
  BI,
  KH,
  CM,
  CA,
  CV,
  KY,
  CF,
  TD,
  CL,
  CN,
  CX,
  CC,
  CO,
  KM,
  CK,
  CR,
  HR,
  CU,
  CW,
  CY,
  CZ,
  CD,
  DK,
  DJ,
  DM,
  DO,
  TL,
  EC,
  EG,
  SV,
  GQ,
  ER,
  EE,
  ET,
  FK,
  FO,
  FJ,
  FI,
  FR,
  PF,
  GA,
  GM,
  GE,
  DE,
  GH,
  GI,
  GR,
  GL,
  GD,
  GU,
  GT,
  GG,
  GN,
  GW,
  GY,
  HT,
  HN,
  HK,
  HU,
  IS,
  IN,
  Id,
  IR,
  IQ,
  IE,
  IM,
  IL,
  IT,
  CI,
  JM,
  JP,
  JE,
  JO,
  KZ,
  KE,
  KI,
  KW,
  KG,
  LA,
  LV,
  LB,
  LS,
  LR,
  LY,
  LI,
  LT,
  LU,
  MO,
  MK,
  MG,
  MW,
  MY,
  MV,
  ML,
  MT,
  MH,
  MR,
  MU,
  YT,
  MX,
  FM,
  MD,
  MC,
  MN,
  ME,
  MS,
  MA,
  MZ,
  MM,
  NA,
  NR,
  NP,
  NL,
  AN,
  NC,
  NZ,
  NI,
  NE,
  NG,
  NU,
  KP,
  MP,
  NO,
  OM,
  PK,
  PW,
  PA,
  PG,
  PY,
  PE,
  PH,
  PN,
  PL,
  PT,
  PR,
  QA,
  CG,
  RE,
  RO,
  RU,
  RW,
  BL,
  SH,
  KN,
  LC,
  MF,
  PM,
  VC,
  WS,
  SM,
  ST,
  SA,
  SN,
  RS,
  SC,
  SL,
  SG,
  SX,
  SK,
  SI,
  SB,
  SO,
  ZA,
  KR,
  ES,
  LK,
  SD,
  SR,
  SJ,
  SZ,
  SE,
  CH,
  SY,
  TW,
  TJ,
  TZ,
  TH,
  TG,
  TK,
  TO,
  TT,
  TN,
  TR,
  TM,
  TC,
  TV,
  VI,
  UG,
  UA,
  AE,
  GB,
  US,
  UY,
  UZ,
  VU,
  VA,
  VE,
  VN,
  WF,
  EH,
  YE,
  ZM,
  ZW,
  PhoneNumberCountryCode'
  #-}

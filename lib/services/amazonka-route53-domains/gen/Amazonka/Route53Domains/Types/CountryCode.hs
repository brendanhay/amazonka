{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53Domains.Types.CountryCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.CountryCode
  ( CountryCode
      ( ..,
        CountryCode_AC,
        CountryCode_AD,
        CountryCode_AE,
        CountryCode_AF,
        CountryCode_AG,
        CountryCode_AI,
        CountryCode_AL,
        CountryCode_AM,
        CountryCode_AN,
        CountryCode_AO,
        CountryCode_AQ,
        CountryCode_AR,
        CountryCode_AS,
        CountryCode_AT,
        CountryCode_AU,
        CountryCode_AW,
        CountryCode_AX,
        CountryCode_AZ,
        CountryCode_BA,
        CountryCode_BB,
        CountryCode_BD,
        CountryCode_BE,
        CountryCode_BF,
        CountryCode_BG,
        CountryCode_BH,
        CountryCode_BI,
        CountryCode_BJ,
        CountryCode_BL,
        CountryCode_BM,
        CountryCode_BN,
        CountryCode_BO,
        CountryCode_BQ,
        CountryCode_BR,
        CountryCode_BS,
        CountryCode_BT,
        CountryCode_BV,
        CountryCode_BW,
        CountryCode_BY,
        CountryCode_BZ,
        CountryCode_CA,
        CountryCode_CC,
        CountryCode_CD,
        CountryCode_CF,
        CountryCode_CG,
        CountryCode_CH,
        CountryCode_CI,
        CountryCode_CK,
        CountryCode_CL,
        CountryCode_CM,
        CountryCode_CN,
        CountryCode_CO,
        CountryCode_CR,
        CountryCode_CU,
        CountryCode_CV,
        CountryCode_CW,
        CountryCode_CX,
        CountryCode_CY,
        CountryCode_CZ,
        CountryCode_DE,
        CountryCode_DJ,
        CountryCode_DK,
        CountryCode_DM,
        CountryCode_DO,
        CountryCode_DZ,
        CountryCode_EC,
        CountryCode_EE,
        CountryCode_EG,
        CountryCode_EH,
        CountryCode_ER,
        CountryCode_ES,
        CountryCode_ET,
        CountryCode_FI,
        CountryCode_FJ,
        CountryCode_FK,
        CountryCode_FM,
        CountryCode_FO,
        CountryCode_FR,
        CountryCode_GA,
        CountryCode_GB,
        CountryCode_GD,
        CountryCode_GE,
        CountryCode_GF,
        CountryCode_GG,
        CountryCode_GH,
        CountryCode_GI,
        CountryCode_GL,
        CountryCode_GM,
        CountryCode_GN,
        CountryCode_GP,
        CountryCode_GQ,
        CountryCode_GR,
        CountryCode_GS,
        CountryCode_GT,
        CountryCode_GU,
        CountryCode_GW,
        CountryCode_GY,
        CountryCode_HK,
        CountryCode_HM,
        CountryCode_HN,
        CountryCode_HR,
        CountryCode_HT,
        CountryCode_HU,
        CountryCode_ID,
        CountryCode_IE,
        CountryCode_IL,
        CountryCode_IM,
        CountryCode_IN,
        CountryCode_IO,
        CountryCode_IQ,
        CountryCode_IR,
        CountryCode_IS,
        CountryCode_IT,
        CountryCode_JE,
        CountryCode_JM,
        CountryCode_JO,
        CountryCode_JP,
        CountryCode_KE,
        CountryCode_KG,
        CountryCode_KH,
        CountryCode_KI,
        CountryCode_KM,
        CountryCode_KN,
        CountryCode_KP,
        CountryCode_KR,
        CountryCode_KW,
        CountryCode_KY,
        CountryCode_KZ,
        CountryCode_LA,
        CountryCode_LB,
        CountryCode_LC,
        CountryCode_LI,
        CountryCode_LK,
        CountryCode_LR,
        CountryCode_LS,
        CountryCode_LT,
        CountryCode_LU,
        CountryCode_LV,
        CountryCode_LY,
        CountryCode_MA,
        CountryCode_MC,
        CountryCode_MD,
        CountryCode_ME,
        CountryCode_MF,
        CountryCode_MG,
        CountryCode_MH,
        CountryCode_MK,
        CountryCode_ML,
        CountryCode_MM,
        CountryCode_MN,
        CountryCode_MO,
        CountryCode_MP,
        CountryCode_MQ,
        CountryCode_MR,
        CountryCode_MS,
        CountryCode_MT,
        CountryCode_MU,
        CountryCode_MV,
        CountryCode_MW,
        CountryCode_MX,
        CountryCode_MY,
        CountryCode_MZ,
        CountryCode_NA,
        CountryCode_NC,
        CountryCode_NE,
        CountryCode_NF,
        CountryCode_NG,
        CountryCode_NI,
        CountryCode_NL,
        CountryCode_NO,
        CountryCode_NP,
        CountryCode_NR,
        CountryCode_NU,
        CountryCode_NZ,
        CountryCode_OM,
        CountryCode_PA,
        CountryCode_PE,
        CountryCode_PF,
        CountryCode_PG,
        CountryCode_PH,
        CountryCode_PK,
        CountryCode_PL,
        CountryCode_PM,
        CountryCode_PN,
        CountryCode_PR,
        CountryCode_PS,
        CountryCode_PT,
        CountryCode_PW,
        CountryCode_PY,
        CountryCode_QA,
        CountryCode_RE,
        CountryCode_RO,
        CountryCode_RS,
        CountryCode_RU,
        CountryCode_RW,
        CountryCode_SA,
        CountryCode_SB,
        CountryCode_SC,
        CountryCode_SD,
        CountryCode_SE,
        CountryCode_SG,
        CountryCode_SH,
        CountryCode_SI,
        CountryCode_SJ,
        CountryCode_SK,
        CountryCode_SL,
        CountryCode_SM,
        CountryCode_SN,
        CountryCode_SO,
        CountryCode_SR,
        CountryCode_SS,
        CountryCode_ST,
        CountryCode_SV,
        CountryCode_SX,
        CountryCode_SY,
        CountryCode_SZ,
        CountryCode_TC,
        CountryCode_TD,
        CountryCode_TF,
        CountryCode_TG,
        CountryCode_TH,
        CountryCode_TJ,
        CountryCode_TK,
        CountryCode_TL,
        CountryCode_TM,
        CountryCode_TN,
        CountryCode_TO,
        CountryCode_TP,
        CountryCode_TR,
        CountryCode_TT,
        CountryCode_TV,
        CountryCode_TW,
        CountryCode_TZ,
        CountryCode_UA,
        CountryCode_UG,
        CountryCode_US,
        CountryCode_UY,
        CountryCode_UZ,
        CountryCode_VA,
        CountryCode_VC,
        CountryCode_VE,
        CountryCode_VG,
        CountryCode_VI,
        CountryCode_VN,
        CountryCode_VU,
        CountryCode_WF,
        CountryCode_WS,
        CountryCode_YE,
        CountryCode_YT,
        CountryCode_ZA,
        CountryCode_ZM,
        CountryCode_ZW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CountryCode = CountryCode'
  { fromCountryCode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern CountryCode_AC :: CountryCode
pattern CountryCode_AC = CountryCode' "AC"

pattern CountryCode_AD :: CountryCode
pattern CountryCode_AD = CountryCode' "AD"

pattern CountryCode_AE :: CountryCode
pattern CountryCode_AE = CountryCode' "AE"

pattern CountryCode_AF :: CountryCode
pattern CountryCode_AF = CountryCode' "AF"

pattern CountryCode_AG :: CountryCode
pattern CountryCode_AG = CountryCode' "AG"

pattern CountryCode_AI :: CountryCode
pattern CountryCode_AI = CountryCode' "AI"

pattern CountryCode_AL :: CountryCode
pattern CountryCode_AL = CountryCode' "AL"

pattern CountryCode_AM :: CountryCode
pattern CountryCode_AM = CountryCode' "AM"

pattern CountryCode_AN :: CountryCode
pattern CountryCode_AN = CountryCode' "AN"

pattern CountryCode_AO :: CountryCode
pattern CountryCode_AO = CountryCode' "AO"

pattern CountryCode_AQ :: CountryCode
pattern CountryCode_AQ = CountryCode' "AQ"

pattern CountryCode_AR :: CountryCode
pattern CountryCode_AR = CountryCode' "AR"

pattern CountryCode_AS :: CountryCode
pattern CountryCode_AS = CountryCode' "AS"

pattern CountryCode_AT :: CountryCode
pattern CountryCode_AT = CountryCode' "AT"

pattern CountryCode_AU :: CountryCode
pattern CountryCode_AU = CountryCode' "AU"

pattern CountryCode_AW :: CountryCode
pattern CountryCode_AW = CountryCode' "AW"

pattern CountryCode_AX :: CountryCode
pattern CountryCode_AX = CountryCode' "AX"

pattern CountryCode_AZ :: CountryCode
pattern CountryCode_AZ = CountryCode' "AZ"

pattern CountryCode_BA :: CountryCode
pattern CountryCode_BA = CountryCode' "BA"

pattern CountryCode_BB :: CountryCode
pattern CountryCode_BB = CountryCode' "BB"

pattern CountryCode_BD :: CountryCode
pattern CountryCode_BD = CountryCode' "BD"

pattern CountryCode_BE :: CountryCode
pattern CountryCode_BE = CountryCode' "BE"

pattern CountryCode_BF :: CountryCode
pattern CountryCode_BF = CountryCode' "BF"

pattern CountryCode_BG :: CountryCode
pattern CountryCode_BG = CountryCode' "BG"

pattern CountryCode_BH :: CountryCode
pattern CountryCode_BH = CountryCode' "BH"

pattern CountryCode_BI :: CountryCode
pattern CountryCode_BI = CountryCode' "BI"

pattern CountryCode_BJ :: CountryCode
pattern CountryCode_BJ = CountryCode' "BJ"

pattern CountryCode_BL :: CountryCode
pattern CountryCode_BL = CountryCode' "BL"

pattern CountryCode_BM :: CountryCode
pattern CountryCode_BM = CountryCode' "BM"

pattern CountryCode_BN :: CountryCode
pattern CountryCode_BN = CountryCode' "BN"

pattern CountryCode_BO :: CountryCode
pattern CountryCode_BO = CountryCode' "BO"

pattern CountryCode_BQ :: CountryCode
pattern CountryCode_BQ = CountryCode' "BQ"

pattern CountryCode_BR :: CountryCode
pattern CountryCode_BR = CountryCode' "BR"

pattern CountryCode_BS :: CountryCode
pattern CountryCode_BS = CountryCode' "BS"

pattern CountryCode_BT :: CountryCode
pattern CountryCode_BT = CountryCode' "BT"

pattern CountryCode_BV :: CountryCode
pattern CountryCode_BV = CountryCode' "BV"

pattern CountryCode_BW :: CountryCode
pattern CountryCode_BW = CountryCode' "BW"

pattern CountryCode_BY :: CountryCode
pattern CountryCode_BY = CountryCode' "BY"

pattern CountryCode_BZ :: CountryCode
pattern CountryCode_BZ = CountryCode' "BZ"

pattern CountryCode_CA :: CountryCode
pattern CountryCode_CA = CountryCode' "CA"

pattern CountryCode_CC :: CountryCode
pattern CountryCode_CC = CountryCode' "CC"

pattern CountryCode_CD :: CountryCode
pattern CountryCode_CD = CountryCode' "CD"

pattern CountryCode_CF :: CountryCode
pattern CountryCode_CF = CountryCode' "CF"

pattern CountryCode_CG :: CountryCode
pattern CountryCode_CG = CountryCode' "CG"

pattern CountryCode_CH :: CountryCode
pattern CountryCode_CH = CountryCode' "CH"

pattern CountryCode_CI :: CountryCode
pattern CountryCode_CI = CountryCode' "CI"

pattern CountryCode_CK :: CountryCode
pattern CountryCode_CK = CountryCode' "CK"

pattern CountryCode_CL :: CountryCode
pattern CountryCode_CL = CountryCode' "CL"

pattern CountryCode_CM :: CountryCode
pattern CountryCode_CM = CountryCode' "CM"

pattern CountryCode_CN :: CountryCode
pattern CountryCode_CN = CountryCode' "CN"

pattern CountryCode_CO :: CountryCode
pattern CountryCode_CO = CountryCode' "CO"

pattern CountryCode_CR :: CountryCode
pattern CountryCode_CR = CountryCode' "CR"

pattern CountryCode_CU :: CountryCode
pattern CountryCode_CU = CountryCode' "CU"

pattern CountryCode_CV :: CountryCode
pattern CountryCode_CV = CountryCode' "CV"

pattern CountryCode_CW :: CountryCode
pattern CountryCode_CW = CountryCode' "CW"

pattern CountryCode_CX :: CountryCode
pattern CountryCode_CX = CountryCode' "CX"

pattern CountryCode_CY :: CountryCode
pattern CountryCode_CY = CountryCode' "CY"

pattern CountryCode_CZ :: CountryCode
pattern CountryCode_CZ = CountryCode' "CZ"

pattern CountryCode_DE :: CountryCode
pattern CountryCode_DE = CountryCode' "DE"

pattern CountryCode_DJ :: CountryCode
pattern CountryCode_DJ = CountryCode' "DJ"

pattern CountryCode_DK :: CountryCode
pattern CountryCode_DK = CountryCode' "DK"

pattern CountryCode_DM :: CountryCode
pattern CountryCode_DM = CountryCode' "DM"

pattern CountryCode_DO :: CountryCode
pattern CountryCode_DO = CountryCode' "DO"

pattern CountryCode_DZ :: CountryCode
pattern CountryCode_DZ = CountryCode' "DZ"

pattern CountryCode_EC :: CountryCode
pattern CountryCode_EC = CountryCode' "EC"

pattern CountryCode_EE :: CountryCode
pattern CountryCode_EE = CountryCode' "EE"

pattern CountryCode_EG :: CountryCode
pattern CountryCode_EG = CountryCode' "EG"

pattern CountryCode_EH :: CountryCode
pattern CountryCode_EH = CountryCode' "EH"

pattern CountryCode_ER :: CountryCode
pattern CountryCode_ER = CountryCode' "ER"

pattern CountryCode_ES :: CountryCode
pattern CountryCode_ES = CountryCode' "ES"

pattern CountryCode_ET :: CountryCode
pattern CountryCode_ET = CountryCode' "ET"

pattern CountryCode_FI :: CountryCode
pattern CountryCode_FI = CountryCode' "FI"

pattern CountryCode_FJ :: CountryCode
pattern CountryCode_FJ = CountryCode' "FJ"

pattern CountryCode_FK :: CountryCode
pattern CountryCode_FK = CountryCode' "FK"

pattern CountryCode_FM :: CountryCode
pattern CountryCode_FM = CountryCode' "FM"

pattern CountryCode_FO :: CountryCode
pattern CountryCode_FO = CountryCode' "FO"

pattern CountryCode_FR :: CountryCode
pattern CountryCode_FR = CountryCode' "FR"

pattern CountryCode_GA :: CountryCode
pattern CountryCode_GA = CountryCode' "GA"

pattern CountryCode_GB :: CountryCode
pattern CountryCode_GB = CountryCode' "GB"

pattern CountryCode_GD :: CountryCode
pattern CountryCode_GD = CountryCode' "GD"

pattern CountryCode_GE :: CountryCode
pattern CountryCode_GE = CountryCode' "GE"

pattern CountryCode_GF :: CountryCode
pattern CountryCode_GF = CountryCode' "GF"

pattern CountryCode_GG :: CountryCode
pattern CountryCode_GG = CountryCode' "GG"

pattern CountryCode_GH :: CountryCode
pattern CountryCode_GH = CountryCode' "GH"

pattern CountryCode_GI :: CountryCode
pattern CountryCode_GI = CountryCode' "GI"

pattern CountryCode_GL :: CountryCode
pattern CountryCode_GL = CountryCode' "GL"

pattern CountryCode_GM :: CountryCode
pattern CountryCode_GM = CountryCode' "GM"

pattern CountryCode_GN :: CountryCode
pattern CountryCode_GN = CountryCode' "GN"

pattern CountryCode_GP :: CountryCode
pattern CountryCode_GP = CountryCode' "GP"

pattern CountryCode_GQ :: CountryCode
pattern CountryCode_GQ = CountryCode' "GQ"

pattern CountryCode_GR :: CountryCode
pattern CountryCode_GR = CountryCode' "GR"

pattern CountryCode_GS :: CountryCode
pattern CountryCode_GS = CountryCode' "GS"

pattern CountryCode_GT :: CountryCode
pattern CountryCode_GT = CountryCode' "GT"

pattern CountryCode_GU :: CountryCode
pattern CountryCode_GU = CountryCode' "GU"

pattern CountryCode_GW :: CountryCode
pattern CountryCode_GW = CountryCode' "GW"

pattern CountryCode_GY :: CountryCode
pattern CountryCode_GY = CountryCode' "GY"

pattern CountryCode_HK :: CountryCode
pattern CountryCode_HK = CountryCode' "HK"

pattern CountryCode_HM :: CountryCode
pattern CountryCode_HM = CountryCode' "HM"

pattern CountryCode_HN :: CountryCode
pattern CountryCode_HN = CountryCode' "HN"

pattern CountryCode_HR :: CountryCode
pattern CountryCode_HR = CountryCode' "HR"

pattern CountryCode_HT :: CountryCode
pattern CountryCode_HT = CountryCode' "HT"

pattern CountryCode_HU :: CountryCode
pattern CountryCode_HU = CountryCode' "HU"

pattern CountryCode_ID :: CountryCode
pattern CountryCode_ID = CountryCode' "ID"

pattern CountryCode_IE :: CountryCode
pattern CountryCode_IE = CountryCode' "IE"

pattern CountryCode_IL :: CountryCode
pattern CountryCode_IL = CountryCode' "IL"

pattern CountryCode_IM :: CountryCode
pattern CountryCode_IM = CountryCode' "IM"

pattern CountryCode_IN :: CountryCode
pattern CountryCode_IN = CountryCode' "IN"

pattern CountryCode_IO :: CountryCode
pattern CountryCode_IO = CountryCode' "IO"

pattern CountryCode_IQ :: CountryCode
pattern CountryCode_IQ = CountryCode' "IQ"

pattern CountryCode_IR :: CountryCode
pattern CountryCode_IR = CountryCode' "IR"

pattern CountryCode_IS :: CountryCode
pattern CountryCode_IS = CountryCode' "IS"

pattern CountryCode_IT :: CountryCode
pattern CountryCode_IT = CountryCode' "IT"

pattern CountryCode_JE :: CountryCode
pattern CountryCode_JE = CountryCode' "JE"

pattern CountryCode_JM :: CountryCode
pattern CountryCode_JM = CountryCode' "JM"

pattern CountryCode_JO :: CountryCode
pattern CountryCode_JO = CountryCode' "JO"

pattern CountryCode_JP :: CountryCode
pattern CountryCode_JP = CountryCode' "JP"

pattern CountryCode_KE :: CountryCode
pattern CountryCode_KE = CountryCode' "KE"

pattern CountryCode_KG :: CountryCode
pattern CountryCode_KG = CountryCode' "KG"

pattern CountryCode_KH :: CountryCode
pattern CountryCode_KH = CountryCode' "KH"

pattern CountryCode_KI :: CountryCode
pattern CountryCode_KI = CountryCode' "KI"

pattern CountryCode_KM :: CountryCode
pattern CountryCode_KM = CountryCode' "KM"

pattern CountryCode_KN :: CountryCode
pattern CountryCode_KN = CountryCode' "KN"

pattern CountryCode_KP :: CountryCode
pattern CountryCode_KP = CountryCode' "KP"

pattern CountryCode_KR :: CountryCode
pattern CountryCode_KR = CountryCode' "KR"

pattern CountryCode_KW :: CountryCode
pattern CountryCode_KW = CountryCode' "KW"

pattern CountryCode_KY :: CountryCode
pattern CountryCode_KY = CountryCode' "KY"

pattern CountryCode_KZ :: CountryCode
pattern CountryCode_KZ = CountryCode' "KZ"

pattern CountryCode_LA :: CountryCode
pattern CountryCode_LA = CountryCode' "LA"

pattern CountryCode_LB :: CountryCode
pattern CountryCode_LB = CountryCode' "LB"

pattern CountryCode_LC :: CountryCode
pattern CountryCode_LC = CountryCode' "LC"

pattern CountryCode_LI :: CountryCode
pattern CountryCode_LI = CountryCode' "LI"

pattern CountryCode_LK :: CountryCode
pattern CountryCode_LK = CountryCode' "LK"

pattern CountryCode_LR :: CountryCode
pattern CountryCode_LR = CountryCode' "LR"

pattern CountryCode_LS :: CountryCode
pattern CountryCode_LS = CountryCode' "LS"

pattern CountryCode_LT :: CountryCode
pattern CountryCode_LT = CountryCode' "LT"

pattern CountryCode_LU :: CountryCode
pattern CountryCode_LU = CountryCode' "LU"

pattern CountryCode_LV :: CountryCode
pattern CountryCode_LV = CountryCode' "LV"

pattern CountryCode_LY :: CountryCode
pattern CountryCode_LY = CountryCode' "LY"

pattern CountryCode_MA :: CountryCode
pattern CountryCode_MA = CountryCode' "MA"

pattern CountryCode_MC :: CountryCode
pattern CountryCode_MC = CountryCode' "MC"

pattern CountryCode_MD :: CountryCode
pattern CountryCode_MD = CountryCode' "MD"

pattern CountryCode_ME :: CountryCode
pattern CountryCode_ME = CountryCode' "ME"

pattern CountryCode_MF :: CountryCode
pattern CountryCode_MF = CountryCode' "MF"

pattern CountryCode_MG :: CountryCode
pattern CountryCode_MG = CountryCode' "MG"

pattern CountryCode_MH :: CountryCode
pattern CountryCode_MH = CountryCode' "MH"

pattern CountryCode_MK :: CountryCode
pattern CountryCode_MK = CountryCode' "MK"

pattern CountryCode_ML :: CountryCode
pattern CountryCode_ML = CountryCode' "ML"

pattern CountryCode_MM :: CountryCode
pattern CountryCode_MM = CountryCode' "MM"

pattern CountryCode_MN :: CountryCode
pattern CountryCode_MN = CountryCode' "MN"

pattern CountryCode_MO :: CountryCode
pattern CountryCode_MO = CountryCode' "MO"

pattern CountryCode_MP :: CountryCode
pattern CountryCode_MP = CountryCode' "MP"

pattern CountryCode_MQ :: CountryCode
pattern CountryCode_MQ = CountryCode' "MQ"

pattern CountryCode_MR :: CountryCode
pattern CountryCode_MR = CountryCode' "MR"

pattern CountryCode_MS :: CountryCode
pattern CountryCode_MS = CountryCode' "MS"

pattern CountryCode_MT :: CountryCode
pattern CountryCode_MT = CountryCode' "MT"

pattern CountryCode_MU :: CountryCode
pattern CountryCode_MU = CountryCode' "MU"

pattern CountryCode_MV :: CountryCode
pattern CountryCode_MV = CountryCode' "MV"

pattern CountryCode_MW :: CountryCode
pattern CountryCode_MW = CountryCode' "MW"

pattern CountryCode_MX :: CountryCode
pattern CountryCode_MX = CountryCode' "MX"

pattern CountryCode_MY :: CountryCode
pattern CountryCode_MY = CountryCode' "MY"

pattern CountryCode_MZ :: CountryCode
pattern CountryCode_MZ = CountryCode' "MZ"

pattern CountryCode_NA :: CountryCode
pattern CountryCode_NA = CountryCode' "NA"

pattern CountryCode_NC :: CountryCode
pattern CountryCode_NC = CountryCode' "NC"

pattern CountryCode_NE :: CountryCode
pattern CountryCode_NE = CountryCode' "NE"

pattern CountryCode_NF :: CountryCode
pattern CountryCode_NF = CountryCode' "NF"

pattern CountryCode_NG :: CountryCode
pattern CountryCode_NG = CountryCode' "NG"

pattern CountryCode_NI :: CountryCode
pattern CountryCode_NI = CountryCode' "NI"

pattern CountryCode_NL :: CountryCode
pattern CountryCode_NL = CountryCode' "NL"

pattern CountryCode_NO :: CountryCode
pattern CountryCode_NO = CountryCode' "NO"

pattern CountryCode_NP :: CountryCode
pattern CountryCode_NP = CountryCode' "NP"

pattern CountryCode_NR :: CountryCode
pattern CountryCode_NR = CountryCode' "NR"

pattern CountryCode_NU :: CountryCode
pattern CountryCode_NU = CountryCode' "NU"

pattern CountryCode_NZ :: CountryCode
pattern CountryCode_NZ = CountryCode' "NZ"

pattern CountryCode_OM :: CountryCode
pattern CountryCode_OM = CountryCode' "OM"

pattern CountryCode_PA :: CountryCode
pattern CountryCode_PA = CountryCode' "PA"

pattern CountryCode_PE :: CountryCode
pattern CountryCode_PE = CountryCode' "PE"

pattern CountryCode_PF :: CountryCode
pattern CountryCode_PF = CountryCode' "PF"

pattern CountryCode_PG :: CountryCode
pattern CountryCode_PG = CountryCode' "PG"

pattern CountryCode_PH :: CountryCode
pattern CountryCode_PH = CountryCode' "PH"

pattern CountryCode_PK :: CountryCode
pattern CountryCode_PK = CountryCode' "PK"

pattern CountryCode_PL :: CountryCode
pattern CountryCode_PL = CountryCode' "PL"

pattern CountryCode_PM :: CountryCode
pattern CountryCode_PM = CountryCode' "PM"

pattern CountryCode_PN :: CountryCode
pattern CountryCode_PN = CountryCode' "PN"

pattern CountryCode_PR :: CountryCode
pattern CountryCode_PR = CountryCode' "PR"

pattern CountryCode_PS :: CountryCode
pattern CountryCode_PS = CountryCode' "PS"

pattern CountryCode_PT :: CountryCode
pattern CountryCode_PT = CountryCode' "PT"

pattern CountryCode_PW :: CountryCode
pattern CountryCode_PW = CountryCode' "PW"

pattern CountryCode_PY :: CountryCode
pattern CountryCode_PY = CountryCode' "PY"

pattern CountryCode_QA :: CountryCode
pattern CountryCode_QA = CountryCode' "QA"

pattern CountryCode_RE :: CountryCode
pattern CountryCode_RE = CountryCode' "RE"

pattern CountryCode_RO :: CountryCode
pattern CountryCode_RO = CountryCode' "RO"

pattern CountryCode_RS :: CountryCode
pattern CountryCode_RS = CountryCode' "RS"

pattern CountryCode_RU :: CountryCode
pattern CountryCode_RU = CountryCode' "RU"

pattern CountryCode_RW :: CountryCode
pattern CountryCode_RW = CountryCode' "RW"

pattern CountryCode_SA :: CountryCode
pattern CountryCode_SA = CountryCode' "SA"

pattern CountryCode_SB :: CountryCode
pattern CountryCode_SB = CountryCode' "SB"

pattern CountryCode_SC :: CountryCode
pattern CountryCode_SC = CountryCode' "SC"

pattern CountryCode_SD :: CountryCode
pattern CountryCode_SD = CountryCode' "SD"

pattern CountryCode_SE :: CountryCode
pattern CountryCode_SE = CountryCode' "SE"

pattern CountryCode_SG :: CountryCode
pattern CountryCode_SG = CountryCode' "SG"

pattern CountryCode_SH :: CountryCode
pattern CountryCode_SH = CountryCode' "SH"

pattern CountryCode_SI :: CountryCode
pattern CountryCode_SI = CountryCode' "SI"

pattern CountryCode_SJ :: CountryCode
pattern CountryCode_SJ = CountryCode' "SJ"

pattern CountryCode_SK :: CountryCode
pattern CountryCode_SK = CountryCode' "SK"

pattern CountryCode_SL :: CountryCode
pattern CountryCode_SL = CountryCode' "SL"

pattern CountryCode_SM :: CountryCode
pattern CountryCode_SM = CountryCode' "SM"

pattern CountryCode_SN :: CountryCode
pattern CountryCode_SN = CountryCode' "SN"

pattern CountryCode_SO :: CountryCode
pattern CountryCode_SO = CountryCode' "SO"

pattern CountryCode_SR :: CountryCode
pattern CountryCode_SR = CountryCode' "SR"

pattern CountryCode_SS :: CountryCode
pattern CountryCode_SS = CountryCode' "SS"

pattern CountryCode_ST :: CountryCode
pattern CountryCode_ST = CountryCode' "ST"

pattern CountryCode_SV :: CountryCode
pattern CountryCode_SV = CountryCode' "SV"

pattern CountryCode_SX :: CountryCode
pattern CountryCode_SX = CountryCode' "SX"

pattern CountryCode_SY :: CountryCode
pattern CountryCode_SY = CountryCode' "SY"

pattern CountryCode_SZ :: CountryCode
pattern CountryCode_SZ = CountryCode' "SZ"

pattern CountryCode_TC :: CountryCode
pattern CountryCode_TC = CountryCode' "TC"

pattern CountryCode_TD :: CountryCode
pattern CountryCode_TD = CountryCode' "TD"

pattern CountryCode_TF :: CountryCode
pattern CountryCode_TF = CountryCode' "TF"

pattern CountryCode_TG :: CountryCode
pattern CountryCode_TG = CountryCode' "TG"

pattern CountryCode_TH :: CountryCode
pattern CountryCode_TH = CountryCode' "TH"

pattern CountryCode_TJ :: CountryCode
pattern CountryCode_TJ = CountryCode' "TJ"

pattern CountryCode_TK :: CountryCode
pattern CountryCode_TK = CountryCode' "TK"

pattern CountryCode_TL :: CountryCode
pattern CountryCode_TL = CountryCode' "TL"

pattern CountryCode_TM :: CountryCode
pattern CountryCode_TM = CountryCode' "TM"

pattern CountryCode_TN :: CountryCode
pattern CountryCode_TN = CountryCode' "TN"

pattern CountryCode_TO :: CountryCode
pattern CountryCode_TO = CountryCode' "TO"

pattern CountryCode_TP :: CountryCode
pattern CountryCode_TP = CountryCode' "TP"

pattern CountryCode_TR :: CountryCode
pattern CountryCode_TR = CountryCode' "TR"

pattern CountryCode_TT :: CountryCode
pattern CountryCode_TT = CountryCode' "TT"

pattern CountryCode_TV :: CountryCode
pattern CountryCode_TV = CountryCode' "TV"

pattern CountryCode_TW :: CountryCode
pattern CountryCode_TW = CountryCode' "TW"

pattern CountryCode_TZ :: CountryCode
pattern CountryCode_TZ = CountryCode' "TZ"

pattern CountryCode_UA :: CountryCode
pattern CountryCode_UA = CountryCode' "UA"

pattern CountryCode_UG :: CountryCode
pattern CountryCode_UG = CountryCode' "UG"

pattern CountryCode_US :: CountryCode
pattern CountryCode_US = CountryCode' "US"

pattern CountryCode_UY :: CountryCode
pattern CountryCode_UY = CountryCode' "UY"

pattern CountryCode_UZ :: CountryCode
pattern CountryCode_UZ = CountryCode' "UZ"

pattern CountryCode_VA :: CountryCode
pattern CountryCode_VA = CountryCode' "VA"

pattern CountryCode_VC :: CountryCode
pattern CountryCode_VC = CountryCode' "VC"

pattern CountryCode_VE :: CountryCode
pattern CountryCode_VE = CountryCode' "VE"

pattern CountryCode_VG :: CountryCode
pattern CountryCode_VG = CountryCode' "VG"

pattern CountryCode_VI :: CountryCode
pattern CountryCode_VI = CountryCode' "VI"

pattern CountryCode_VN :: CountryCode
pattern CountryCode_VN = CountryCode' "VN"

pattern CountryCode_VU :: CountryCode
pattern CountryCode_VU = CountryCode' "VU"

pattern CountryCode_WF :: CountryCode
pattern CountryCode_WF = CountryCode' "WF"

pattern CountryCode_WS :: CountryCode
pattern CountryCode_WS = CountryCode' "WS"

pattern CountryCode_YE :: CountryCode
pattern CountryCode_YE = CountryCode' "YE"

pattern CountryCode_YT :: CountryCode
pattern CountryCode_YT = CountryCode' "YT"

pattern CountryCode_ZA :: CountryCode
pattern CountryCode_ZA = CountryCode' "ZA"

pattern CountryCode_ZM :: CountryCode
pattern CountryCode_ZM = CountryCode' "ZM"

pattern CountryCode_ZW :: CountryCode
pattern CountryCode_ZW = CountryCode' "ZW"

{-# COMPLETE
  CountryCode_AC,
  CountryCode_AD,
  CountryCode_AE,
  CountryCode_AF,
  CountryCode_AG,
  CountryCode_AI,
  CountryCode_AL,
  CountryCode_AM,
  CountryCode_AN,
  CountryCode_AO,
  CountryCode_AQ,
  CountryCode_AR,
  CountryCode_AS,
  CountryCode_AT,
  CountryCode_AU,
  CountryCode_AW,
  CountryCode_AX,
  CountryCode_AZ,
  CountryCode_BA,
  CountryCode_BB,
  CountryCode_BD,
  CountryCode_BE,
  CountryCode_BF,
  CountryCode_BG,
  CountryCode_BH,
  CountryCode_BI,
  CountryCode_BJ,
  CountryCode_BL,
  CountryCode_BM,
  CountryCode_BN,
  CountryCode_BO,
  CountryCode_BQ,
  CountryCode_BR,
  CountryCode_BS,
  CountryCode_BT,
  CountryCode_BV,
  CountryCode_BW,
  CountryCode_BY,
  CountryCode_BZ,
  CountryCode_CA,
  CountryCode_CC,
  CountryCode_CD,
  CountryCode_CF,
  CountryCode_CG,
  CountryCode_CH,
  CountryCode_CI,
  CountryCode_CK,
  CountryCode_CL,
  CountryCode_CM,
  CountryCode_CN,
  CountryCode_CO,
  CountryCode_CR,
  CountryCode_CU,
  CountryCode_CV,
  CountryCode_CW,
  CountryCode_CX,
  CountryCode_CY,
  CountryCode_CZ,
  CountryCode_DE,
  CountryCode_DJ,
  CountryCode_DK,
  CountryCode_DM,
  CountryCode_DO,
  CountryCode_DZ,
  CountryCode_EC,
  CountryCode_EE,
  CountryCode_EG,
  CountryCode_EH,
  CountryCode_ER,
  CountryCode_ES,
  CountryCode_ET,
  CountryCode_FI,
  CountryCode_FJ,
  CountryCode_FK,
  CountryCode_FM,
  CountryCode_FO,
  CountryCode_FR,
  CountryCode_GA,
  CountryCode_GB,
  CountryCode_GD,
  CountryCode_GE,
  CountryCode_GF,
  CountryCode_GG,
  CountryCode_GH,
  CountryCode_GI,
  CountryCode_GL,
  CountryCode_GM,
  CountryCode_GN,
  CountryCode_GP,
  CountryCode_GQ,
  CountryCode_GR,
  CountryCode_GS,
  CountryCode_GT,
  CountryCode_GU,
  CountryCode_GW,
  CountryCode_GY,
  CountryCode_HK,
  CountryCode_HM,
  CountryCode_HN,
  CountryCode_HR,
  CountryCode_HT,
  CountryCode_HU,
  CountryCode_ID,
  CountryCode_IE,
  CountryCode_IL,
  CountryCode_IM,
  CountryCode_IN,
  CountryCode_IO,
  CountryCode_IQ,
  CountryCode_IR,
  CountryCode_IS,
  CountryCode_IT,
  CountryCode_JE,
  CountryCode_JM,
  CountryCode_JO,
  CountryCode_JP,
  CountryCode_KE,
  CountryCode_KG,
  CountryCode_KH,
  CountryCode_KI,
  CountryCode_KM,
  CountryCode_KN,
  CountryCode_KP,
  CountryCode_KR,
  CountryCode_KW,
  CountryCode_KY,
  CountryCode_KZ,
  CountryCode_LA,
  CountryCode_LB,
  CountryCode_LC,
  CountryCode_LI,
  CountryCode_LK,
  CountryCode_LR,
  CountryCode_LS,
  CountryCode_LT,
  CountryCode_LU,
  CountryCode_LV,
  CountryCode_LY,
  CountryCode_MA,
  CountryCode_MC,
  CountryCode_MD,
  CountryCode_ME,
  CountryCode_MF,
  CountryCode_MG,
  CountryCode_MH,
  CountryCode_MK,
  CountryCode_ML,
  CountryCode_MM,
  CountryCode_MN,
  CountryCode_MO,
  CountryCode_MP,
  CountryCode_MQ,
  CountryCode_MR,
  CountryCode_MS,
  CountryCode_MT,
  CountryCode_MU,
  CountryCode_MV,
  CountryCode_MW,
  CountryCode_MX,
  CountryCode_MY,
  CountryCode_MZ,
  CountryCode_NA,
  CountryCode_NC,
  CountryCode_NE,
  CountryCode_NF,
  CountryCode_NG,
  CountryCode_NI,
  CountryCode_NL,
  CountryCode_NO,
  CountryCode_NP,
  CountryCode_NR,
  CountryCode_NU,
  CountryCode_NZ,
  CountryCode_OM,
  CountryCode_PA,
  CountryCode_PE,
  CountryCode_PF,
  CountryCode_PG,
  CountryCode_PH,
  CountryCode_PK,
  CountryCode_PL,
  CountryCode_PM,
  CountryCode_PN,
  CountryCode_PR,
  CountryCode_PS,
  CountryCode_PT,
  CountryCode_PW,
  CountryCode_PY,
  CountryCode_QA,
  CountryCode_RE,
  CountryCode_RO,
  CountryCode_RS,
  CountryCode_RU,
  CountryCode_RW,
  CountryCode_SA,
  CountryCode_SB,
  CountryCode_SC,
  CountryCode_SD,
  CountryCode_SE,
  CountryCode_SG,
  CountryCode_SH,
  CountryCode_SI,
  CountryCode_SJ,
  CountryCode_SK,
  CountryCode_SL,
  CountryCode_SM,
  CountryCode_SN,
  CountryCode_SO,
  CountryCode_SR,
  CountryCode_SS,
  CountryCode_ST,
  CountryCode_SV,
  CountryCode_SX,
  CountryCode_SY,
  CountryCode_SZ,
  CountryCode_TC,
  CountryCode_TD,
  CountryCode_TF,
  CountryCode_TG,
  CountryCode_TH,
  CountryCode_TJ,
  CountryCode_TK,
  CountryCode_TL,
  CountryCode_TM,
  CountryCode_TN,
  CountryCode_TO,
  CountryCode_TP,
  CountryCode_TR,
  CountryCode_TT,
  CountryCode_TV,
  CountryCode_TW,
  CountryCode_TZ,
  CountryCode_UA,
  CountryCode_UG,
  CountryCode_US,
  CountryCode_UY,
  CountryCode_UZ,
  CountryCode_VA,
  CountryCode_VC,
  CountryCode_VE,
  CountryCode_VG,
  CountryCode_VI,
  CountryCode_VN,
  CountryCode_VU,
  CountryCode_WF,
  CountryCode_WS,
  CountryCode_YE,
  CountryCode_YT,
  CountryCode_ZA,
  CountryCode_ZM,
  CountryCode_ZW,
  CountryCode'
  #-}

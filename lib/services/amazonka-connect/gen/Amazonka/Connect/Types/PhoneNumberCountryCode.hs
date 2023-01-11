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
-- Module      : Amazonka.Connect.Types.PhoneNumberCountryCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PhoneNumberCountryCode
  ( PhoneNumberCountryCode
      ( ..,
        PhoneNumberCountryCode_AD,
        PhoneNumberCountryCode_AE,
        PhoneNumberCountryCode_AF,
        PhoneNumberCountryCode_AG,
        PhoneNumberCountryCode_AI,
        PhoneNumberCountryCode_AL,
        PhoneNumberCountryCode_AM,
        PhoneNumberCountryCode_AN,
        PhoneNumberCountryCode_AO,
        PhoneNumberCountryCode_AQ,
        PhoneNumberCountryCode_AR,
        PhoneNumberCountryCode_AS,
        PhoneNumberCountryCode_AT,
        PhoneNumberCountryCode_AU,
        PhoneNumberCountryCode_AW,
        PhoneNumberCountryCode_AZ,
        PhoneNumberCountryCode_BA,
        PhoneNumberCountryCode_BB,
        PhoneNumberCountryCode_BD,
        PhoneNumberCountryCode_BE,
        PhoneNumberCountryCode_BF,
        PhoneNumberCountryCode_BG,
        PhoneNumberCountryCode_BH,
        PhoneNumberCountryCode_BI,
        PhoneNumberCountryCode_BJ,
        PhoneNumberCountryCode_BL,
        PhoneNumberCountryCode_BM,
        PhoneNumberCountryCode_BN,
        PhoneNumberCountryCode_BO,
        PhoneNumberCountryCode_BR,
        PhoneNumberCountryCode_BS,
        PhoneNumberCountryCode_BT,
        PhoneNumberCountryCode_BW,
        PhoneNumberCountryCode_BY,
        PhoneNumberCountryCode_BZ,
        PhoneNumberCountryCode_CA,
        PhoneNumberCountryCode_CC,
        PhoneNumberCountryCode_CD,
        PhoneNumberCountryCode_CF,
        PhoneNumberCountryCode_CG,
        PhoneNumberCountryCode_CH,
        PhoneNumberCountryCode_CI,
        PhoneNumberCountryCode_CK,
        PhoneNumberCountryCode_CL,
        PhoneNumberCountryCode_CM,
        PhoneNumberCountryCode_CN,
        PhoneNumberCountryCode_CO,
        PhoneNumberCountryCode_CR,
        PhoneNumberCountryCode_CU,
        PhoneNumberCountryCode_CV,
        PhoneNumberCountryCode_CW,
        PhoneNumberCountryCode_CX,
        PhoneNumberCountryCode_CY,
        PhoneNumberCountryCode_CZ,
        PhoneNumberCountryCode_DE,
        PhoneNumberCountryCode_DJ,
        PhoneNumberCountryCode_DK,
        PhoneNumberCountryCode_DM,
        PhoneNumberCountryCode_DO,
        PhoneNumberCountryCode_DZ,
        PhoneNumberCountryCode_EC,
        PhoneNumberCountryCode_EE,
        PhoneNumberCountryCode_EG,
        PhoneNumberCountryCode_EH,
        PhoneNumberCountryCode_ER,
        PhoneNumberCountryCode_ES,
        PhoneNumberCountryCode_ET,
        PhoneNumberCountryCode_FI,
        PhoneNumberCountryCode_FJ,
        PhoneNumberCountryCode_FK,
        PhoneNumberCountryCode_FM,
        PhoneNumberCountryCode_FO,
        PhoneNumberCountryCode_FR,
        PhoneNumberCountryCode_GA,
        PhoneNumberCountryCode_GB,
        PhoneNumberCountryCode_GD,
        PhoneNumberCountryCode_GE,
        PhoneNumberCountryCode_GG,
        PhoneNumberCountryCode_GH,
        PhoneNumberCountryCode_GI,
        PhoneNumberCountryCode_GL,
        PhoneNumberCountryCode_GM,
        PhoneNumberCountryCode_GN,
        PhoneNumberCountryCode_GQ,
        PhoneNumberCountryCode_GR,
        PhoneNumberCountryCode_GT,
        PhoneNumberCountryCode_GU,
        PhoneNumberCountryCode_GW,
        PhoneNumberCountryCode_GY,
        PhoneNumberCountryCode_HK,
        PhoneNumberCountryCode_HN,
        PhoneNumberCountryCode_HR,
        PhoneNumberCountryCode_HT,
        PhoneNumberCountryCode_HU,
        PhoneNumberCountryCode_ID,
        PhoneNumberCountryCode_IE,
        PhoneNumberCountryCode_IL,
        PhoneNumberCountryCode_IM,
        PhoneNumberCountryCode_IN,
        PhoneNumberCountryCode_IO,
        PhoneNumberCountryCode_IQ,
        PhoneNumberCountryCode_IR,
        PhoneNumberCountryCode_IS,
        PhoneNumberCountryCode_IT,
        PhoneNumberCountryCode_JE,
        PhoneNumberCountryCode_JM,
        PhoneNumberCountryCode_JO,
        PhoneNumberCountryCode_JP,
        PhoneNumberCountryCode_KE,
        PhoneNumberCountryCode_KG,
        PhoneNumberCountryCode_KH,
        PhoneNumberCountryCode_KI,
        PhoneNumberCountryCode_KM,
        PhoneNumberCountryCode_KN,
        PhoneNumberCountryCode_KP,
        PhoneNumberCountryCode_KR,
        PhoneNumberCountryCode_KW,
        PhoneNumberCountryCode_KY,
        PhoneNumberCountryCode_KZ,
        PhoneNumberCountryCode_LA,
        PhoneNumberCountryCode_LB,
        PhoneNumberCountryCode_LC,
        PhoneNumberCountryCode_LI,
        PhoneNumberCountryCode_LK,
        PhoneNumberCountryCode_LR,
        PhoneNumberCountryCode_LS,
        PhoneNumberCountryCode_LT,
        PhoneNumberCountryCode_LU,
        PhoneNumberCountryCode_LV,
        PhoneNumberCountryCode_LY,
        PhoneNumberCountryCode_MA,
        PhoneNumberCountryCode_MC,
        PhoneNumberCountryCode_MD,
        PhoneNumberCountryCode_ME,
        PhoneNumberCountryCode_MF,
        PhoneNumberCountryCode_MG,
        PhoneNumberCountryCode_MH,
        PhoneNumberCountryCode_MK,
        PhoneNumberCountryCode_ML,
        PhoneNumberCountryCode_MM,
        PhoneNumberCountryCode_MN,
        PhoneNumberCountryCode_MO,
        PhoneNumberCountryCode_MP,
        PhoneNumberCountryCode_MR,
        PhoneNumberCountryCode_MS,
        PhoneNumberCountryCode_MT,
        PhoneNumberCountryCode_MU,
        PhoneNumberCountryCode_MV,
        PhoneNumberCountryCode_MW,
        PhoneNumberCountryCode_MX,
        PhoneNumberCountryCode_MY,
        PhoneNumberCountryCode_MZ,
        PhoneNumberCountryCode_NA,
        PhoneNumberCountryCode_NC,
        PhoneNumberCountryCode_NE,
        PhoneNumberCountryCode_NG,
        PhoneNumberCountryCode_NI,
        PhoneNumberCountryCode_NL,
        PhoneNumberCountryCode_NO,
        PhoneNumberCountryCode_NP,
        PhoneNumberCountryCode_NR,
        PhoneNumberCountryCode_NU,
        PhoneNumberCountryCode_NZ,
        PhoneNumberCountryCode_OM,
        PhoneNumberCountryCode_PA,
        PhoneNumberCountryCode_PE,
        PhoneNumberCountryCode_PF,
        PhoneNumberCountryCode_PG,
        PhoneNumberCountryCode_PH,
        PhoneNumberCountryCode_PK,
        PhoneNumberCountryCode_PL,
        PhoneNumberCountryCode_PM,
        PhoneNumberCountryCode_PN,
        PhoneNumberCountryCode_PR,
        PhoneNumberCountryCode_PT,
        PhoneNumberCountryCode_PW,
        PhoneNumberCountryCode_PY,
        PhoneNumberCountryCode_QA,
        PhoneNumberCountryCode_RE,
        PhoneNumberCountryCode_RO,
        PhoneNumberCountryCode_RS,
        PhoneNumberCountryCode_RU,
        PhoneNumberCountryCode_RW,
        PhoneNumberCountryCode_SA,
        PhoneNumberCountryCode_SB,
        PhoneNumberCountryCode_SC,
        PhoneNumberCountryCode_SD,
        PhoneNumberCountryCode_SE,
        PhoneNumberCountryCode_SG,
        PhoneNumberCountryCode_SH,
        PhoneNumberCountryCode_SI,
        PhoneNumberCountryCode_SJ,
        PhoneNumberCountryCode_SK,
        PhoneNumberCountryCode_SL,
        PhoneNumberCountryCode_SM,
        PhoneNumberCountryCode_SN,
        PhoneNumberCountryCode_SO,
        PhoneNumberCountryCode_SR,
        PhoneNumberCountryCode_ST,
        PhoneNumberCountryCode_SV,
        PhoneNumberCountryCode_SX,
        PhoneNumberCountryCode_SY,
        PhoneNumberCountryCode_SZ,
        PhoneNumberCountryCode_TC,
        PhoneNumberCountryCode_TD,
        PhoneNumberCountryCode_TG,
        PhoneNumberCountryCode_TH,
        PhoneNumberCountryCode_TJ,
        PhoneNumberCountryCode_TK,
        PhoneNumberCountryCode_TL,
        PhoneNumberCountryCode_TM,
        PhoneNumberCountryCode_TN,
        PhoneNumberCountryCode_TO,
        PhoneNumberCountryCode_TR,
        PhoneNumberCountryCode_TT,
        PhoneNumberCountryCode_TV,
        PhoneNumberCountryCode_TW,
        PhoneNumberCountryCode_TZ,
        PhoneNumberCountryCode_UA,
        PhoneNumberCountryCode_UG,
        PhoneNumberCountryCode_US,
        PhoneNumberCountryCode_UY,
        PhoneNumberCountryCode_UZ,
        PhoneNumberCountryCode_VA,
        PhoneNumberCountryCode_VC,
        PhoneNumberCountryCode_VE,
        PhoneNumberCountryCode_VG,
        PhoneNumberCountryCode_VI,
        PhoneNumberCountryCode_VN,
        PhoneNumberCountryCode_VU,
        PhoneNumberCountryCode_WF,
        PhoneNumberCountryCode_WS,
        PhoneNumberCountryCode_YE,
        PhoneNumberCountryCode_YT,
        PhoneNumberCountryCode_ZA,
        PhoneNumberCountryCode_ZM,
        PhoneNumberCountryCode_ZW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberCountryCode = PhoneNumberCountryCode'
  { fromPhoneNumberCountryCode ::
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

pattern PhoneNumberCountryCode_AD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AD = PhoneNumberCountryCode' "AD"

pattern PhoneNumberCountryCode_AE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AE = PhoneNumberCountryCode' "AE"

pattern PhoneNumberCountryCode_AF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AF = PhoneNumberCountryCode' "AF"

pattern PhoneNumberCountryCode_AG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AG = PhoneNumberCountryCode' "AG"

pattern PhoneNumberCountryCode_AI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AI = PhoneNumberCountryCode' "AI"

pattern PhoneNumberCountryCode_AL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AL = PhoneNumberCountryCode' "AL"

pattern PhoneNumberCountryCode_AM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AM = PhoneNumberCountryCode' "AM"

pattern PhoneNumberCountryCode_AN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AN = PhoneNumberCountryCode' "AN"

pattern PhoneNumberCountryCode_AO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AO = PhoneNumberCountryCode' "AO"

pattern PhoneNumberCountryCode_AQ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AQ = PhoneNumberCountryCode' "AQ"

pattern PhoneNumberCountryCode_AR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AR = PhoneNumberCountryCode' "AR"

pattern PhoneNumberCountryCode_AS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AS = PhoneNumberCountryCode' "AS"

pattern PhoneNumberCountryCode_AT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AT = PhoneNumberCountryCode' "AT"

pattern PhoneNumberCountryCode_AU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AU = PhoneNumberCountryCode' "AU"

pattern PhoneNumberCountryCode_AW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AW = PhoneNumberCountryCode' "AW"

pattern PhoneNumberCountryCode_AZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_AZ = PhoneNumberCountryCode' "AZ"

pattern PhoneNumberCountryCode_BA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BA = PhoneNumberCountryCode' "BA"

pattern PhoneNumberCountryCode_BB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BB = PhoneNumberCountryCode' "BB"

pattern PhoneNumberCountryCode_BD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BD = PhoneNumberCountryCode' "BD"

pattern PhoneNumberCountryCode_BE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BE = PhoneNumberCountryCode' "BE"

pattern PhoneNumberCountryCode_BF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BF = PhoneNumberCountryCode' "BF"

pattern PhoneNumberCountryCode_BG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BG = PhoneNumberCountryCode' "BG"

pattern PhoneNumberCountryCode_BH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BH = PhoneNumberCountryCode' "BH"

pattern PhoneNumberCountryCode_BI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BI = PhoneNumberCountryCode' "BI"

pattern PhoneNumberCountryCode_BJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BJ = PhoneNumberCountryCode' "BJ"

pattern PhoneNumberCountryCode_BL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BL = PhoneNumberCountryCode' "BL"

pattern PhoneNumberCountryCode_BM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BM = PhoneNumberCountryCode' "BM"

pattern PhoneNumberCountryCode_BN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BN = PhoneNumberCountryCode' "BN"

pattern PhoneNumberCountryCode_BO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BO = PhoneNumberCountryCode' "BO"

pattern PhoneNumberCountryCode_BR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BR = PhoneNumberCountryCode' "BR"

pattern PhoneNumberCountryCode_BS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BS = PhoneNumberCountryCode' "BS"

pattern PhoneNumberCountryCode_BT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BT = PhoneNumberCountryCode' "BT"

pattern PhoneNumberCountryCode_BW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BW = PhoneNumberCountryCode' "BW"

pattern PhoneNumberCountryCode_BY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BY = PhoneNumberCountryCode' "BY"

pattern PhoneNumberCountryCode_BZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_BZ = PhoneNumberCountryCode' "BZ"

pattern PhoneNumberCountryCode_CA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CA = PhoneNumberCountryCode' "CA"

pattern PhoneNumberCountryCode_CC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CC = PhoneNumberCountryCode' "CC"

pattern PhoneNumberCountryCode_CD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CD = PhoneNumberCountryCode' "CD"

pattern PhoneNumberCountryCode_CF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CF = PhoneNumberCountryCode' "CF"

pattern PhoneNumberCountryCode_CG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CG = PhoneNumberCountryCode' "CG"

pattern PhoneNumberCountryCode_CH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CH = PhoneNumberCountryCode' "CH"

pattern PhoneNumberCountryCode_CI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CI = PhoneNumberCountryCode' "CI"

pattern PhoneNumberCountryCode_CK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CK = PhoneNumberCountryCode' "CK"

pattern PhoneNumberCountryCode_CL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CL = PhoneNumberCountryCode' "CL"

pattern PhoneNumberCountryCode_CM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CM = PhoneNumberCountryCode' "CM"

pattern PhoneNumberCountryCode_CN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CN = PhoneNumberCountryCode' "CN"

pattern PhoneNumberCountryCode_CO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CO = PhoneNumberCountryCode' "CO"

pattern PhoneNumberCountryCode_CR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CR = PhoneNumberCountryCode' "CR"

pattern PhoneNumberCountryCode_CU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CU = PhoneNumberCountryCode' "CU"

pattern PhoneNumberCountryCode_CV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CV = PhoneNumberCountryCode' "CV"

pattern PhoneNumberCountryCode_CW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CW = PhoneNumberCountryCode' "CW"

pattern PhoneNumberCountryCode_CX :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CX = PhoneNumberCountryCode' "CX"

pattern PhoneNumberCountryCode_CY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CY = PhoneNumberCountryCode' "CY"

pattern PhoneNumberCountryCode_CZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_CZ = PhoneNumberCountryCode' "CZ"

pattern PhoneNumberCountryCode_DE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_DE = PhoneNumberCountryCode' "DE"

pattern PhoneNumberCountryCode_DJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_DJ = PhoneNumberCountryCode' "DJ"

pattern PhoneNumberCountryCode_DK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_DK = PhoneNumberCountryCode' "DK"

pattern PhoneNumberCountryCode_DM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_DM = PhoneNumberCountryCode' "DM"

pattern PhoneNumberCountryCode_DO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_DO = PhoneNumberCountryCode' "DO"

pattern PhoneNumberCountryCode_DZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_DZ = PhoneNumberCountryCode' "DZ"

pattern PhoneNumberCountryCode_EC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_EC = PhoneNumberCountryCode' "EC"

pattern PhoneNumberCountryCode_EE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_EE = PhoneNumberCountryCode' "EE"

pattern PhoneNumberCountryCode_EG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_EG = PhoneNumberCountryCode' "EG"

pattern PhoneNumberCountryCode_EH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_EH = PhoneNumberCountryCode' "EH"

pattern PhoneNumberCountryCode_ER :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ER = PhoneNumberCountryCode' "ER"

pattern PhoneNumberCountryCode_ES :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ES = PhoneNumberCountryCode' "ES"

pattern PhoneNumberCountryCode_ET :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ET = PhoneNumberCountryCode' "ET"

pattern PhoneNumberCountryCode_FI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_FI = PhoneNumberCountryCode' "FI"

pattern PhoneNumberCountryCode_FJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_FJ = PhoneNumberCountryCode' "FJ"

pattern PhoneNumberCountryCode_FK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_FK = PhoneNumberCountryCode' "FK"

pattern PhoneNumberCountryCode_FM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_FM = PhoneNumberCountryCode' "FM"

pattern PhoneNumberCountryCode_FO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_FO = PhoneNumberCountryCode' "FO"

pattern PhoneNumberCountryCode_FR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_FR = PhoneNumberCountryCode' "FR"

pattern PhoneNumberCountryCode_GA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GA = PhoneNumberCountryCode' "GA"

pattern PhoneNumberCountryCode_GB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GB = PhoneNumberCountryCode' "GB"

pattern PhoneNumberCountryCode_GD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GD = PhoneNumberCountryCode' "GD"

pattern PhoneNumberCountryCode_GE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GE = PhoneNumberCountryCode' "GE"

pattern PhoneNumberCountryCode_GG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GG = PhoneNumberCountryCode' "GG"

pattern PhoneNumberCountryCode_GH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GH = PhoneNumberCountryCode' "GH"

pattern PhoneNumberCountryCode_GI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GI = PhoneNumberCountryCode' "GI"

pattern PhoneNumberCountryCode_GL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GL = PhoneNumberCountryCode' "GL"

pattern PhoneNumberCountryCode_GM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GM = PhoneNumberCountryCode' "GM"

pattern PhoneNumberCountryCode_GN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GN = PhoneNumberCountryCode' "GN"

pattern PhoneNumberCountryCode_GQ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GQ = PhoneNumberCountryCode' "GQ"

pattern PhoneNumberCountryCode_GR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GR = PhoneNumberCountryCode' "GR"

pattern PhoneNumberCountryCode_GT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GT = PhoneNumberCountryCode' "GT"

pattern PhoneNumberCountryCode_GU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GU = PhoneNumberCountryCode' "GU"

pattern PhoneNumberCountryCode_GW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GW = PhoneNumberCountryCode' "GW"

pattern PhoneNumberCountryCode_GY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_GY = PhoneNumberCountryCode' "GY"

pattern PhoneNumberCountryCode_HK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_HK = PhoneNumberCountryCode' "HK"

pattern PhoneNumberCountryCode_HN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_HN = PhoneNumberCountryCode' "HN"

pattern PhoneNumberCountryCode_HR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_HR = PhoneNumberCountryCode' "HR"

pattern PhoneNumberCountryCode_HT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_HT = PhoneNumberCountryCode' "HT"

pattern PhoneNumberCountryCode_HU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_HU = PhoneNumberCountryCode' "HU"

pattern PhoneNumberCountryCode_ID :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ID = PhoneNumberCountryCode' "ID"

pattern PhoneNumberCountryCode_IE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IE = PhoneNumberCountryCode' "IE"

pattern PhoneNumberCountryCode_IL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IL = PhoneNumberCountryCode' "IL"

pattern PhoneNumberCountryCode_IM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IM = PhoneNumberCountryCode' "IM"

pattern PhoneNumberCountryCode_IN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IN = PhoneNumberCountryCode' "IN"

pattern PhoneNumberCountryCode_IO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IO = PhoneNumberCountryCode' "IO"

pattern PhoneNumberCountryCode_IQ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IQ = PhoneNumberCountryCode' "IQ"

pattern PhoneNumberCountryCode_IR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IR = PhoneNumberCountryCode' "IR"

pattern PhoneNumberCountryCode_IS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IS = PhoneNumberCountryCode' "IS"

pattern PhoneNumberCountryCode_IT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_IT = PhoneNumberCountryCode' "IT"

pattern PhoneNumberCountryCode_JE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_JE = PhoneNumberCountryCode' "JE"

pattern PhoneNumberCountryCode_JM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_JM = PhoneNumberCountryCode' "JM"

pattern PhoneNumberCountryCode_JO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_JO = PhoneNumberCountryCode' "JO"

pattern PhoneNumberCountryCode_JP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_JP = PhoneNumberCountryCode' "JP"

pattern PhoneNumberCountryCode_KE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KE = PhoneNumberCountryCode' "KE"

pattern PhoneNumberCountryCode_KG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KG = PhoneNumberCountryCode' "KG"

pattern PhoneNumberCountryCode_KH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KH = PhoneNumberCountryCode' "KH"

pattern PhoneNumberCountryCode_KI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KI = PhoneNumberCountryCode' "KI"

pattern PhoneNumberCountryCode_KM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KM = PhoneNumberCountryCode' "KM"

pattern PhoneNumberCountryCode_KN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KN = PhoneNumberCountryCode' "KN"

pattern PhoneNumberCountryCode_KP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KP = PhoneNumberCountryCode' "KP"

pattern PhoneNumberCountryCode_KR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KR = PhoneNumberCountryCode' "KR"

pattern PhoneNumberCountryCode_KW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KW = PhoneNumberCountryCode' "KW"

pattern PhoneNumberCountryCode_KY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KY = PhoneNumberCountryCode' "KY"

pattern PhoneNumberCountryCode_KZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_KZ = PhoneNumberCountryCode' "KZ"

pattern PhoneNumberCountryCode_LA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LA = PhoneNumberCountryCode' "LA"

pattern PhoneNumberCountryCode_LB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LB = PhoneNumberCountryCode' "LB"

pattern PhoneNumberCountryCode_LC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LC = PhoneNumberCountryCode' "LC"

pattern PhoneNumberCountryCode_LI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LI = PhoneNumberCountryCode' "LI"

pattern PhoneNumberCountryCode_LK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LK = PhoneNumberCountryCode' "LK"

pattern PhoneNumberCountryCode_LR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LR = PhoneNumberCountryCode' "LR"

pattern PhoneNumberCountryCode_LS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LS = PhoneNumberCountryCode' "LS"

pattern PhoneNumberCountryCode_LT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LT = PhoneNumberCountryCode' "LT"

pattern PhoneNumberCountryCode_LU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LU = PhoneNumberCountryCode' "LU"

pattern PhoneNumberCountryCode_LV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LV = PhoneNumberCountryCode' "LV"

pattern PhoneNumberCountryCode_LY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_LY = PhoneNumberCountryCode' "LY"

pattern PhoneNumberCountryCode_MA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MA = PhoneNumberCountryCode' "MA"

pattern PhoneNumberCountryCode_MC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MC = PhoneNumberCountryCode' "MC"

pattern PhoneNumberCountryCode_MD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MD = PhoneNumberCountryCode' "MD"

pattern PhoneNumberCountryCode_ME :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ME = PhoneNumberCountryCode' "ME"

pattern PhoneNumberCountryCode_MF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MF = PhoneNumberCountryCode' "MF"

pattern PhoneNumberCountryCode_MG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MG = PhoneNumberCountryCode' "MG"

pattern PhoneNumberCountryCode_MH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MH = PhoneNumberCountryCode' "MH"

pattern PhoneNumberCountryCode_MK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MK = PhoneNumberCountryCode' "MK"

pattern PhoneNumberCountryCode_ML :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ML = PhoneNumberCountryCode' "ML"

pattern PhoneNumberCountryCode_MM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MM = PhoneNumberCountryCode' "MM"

pattern PhoneNumberCountryCode_MN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MN = PhoneNumberCountryCode' "MN"

pattern PhoneNumberCountryCode_MO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MO = PhoneNumberCountryCode' "MO"

pattern PhoneNumberCountryCode_MP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MP = PhoneNumberCountryCode' "MP"

pattern PhoneNumberCountryCode_MR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MR = PhoneNumberCountryCode' "MR"

pattern PhoneNumberCountryCode_MS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MS = PhoneNumberCountryCode' "MS"

pattern PhoneNumberCountryCode_MT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MT = PhoneNumberCountryCode' "MT"

pattern PhoneNumberCountryCode_MU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MU = PhoneNumberCountryCode' "MU"

pattern PhoneNumberCountryCode_MV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MV = PhoneNumberCountryCode' "MV"

pattern PhoneNumberCountryCode_MW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MW = PhoneNumberCountryCode' "MW"

pattern PhoneNumberCountryCode_MX :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MX = PhoneNumberCountryCode' "MX"

pattern PhoneNumberCountryCode_MY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MY = PhoneNumberCountryCode' "MY"

pattern PhoneNumberCountryCode_MZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_MZ = PhoneNumberCountryCode' "MZ"

pattern PhoneNumberCountryCode_NA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NA = PhoneNumberCountryCode' "NA"

pattern PhoneNumberCountryCode_NC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NC = PhoneNumberCountryCode' "NC"

pattern PhoneNumberCountryCode_NE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NE = PhoneNumberCountryCode' "NE"

pattern PhoneNumberCountryCode_NG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NG = PhoneNumberCountryCode' "NG"

pattern PhoneNumberCountryCode_NI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NI = PhoneNumberCountryCode' "NI"

pattern PhoneNumberCountryCode_NL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NL = PhoneNumberCountryCode' "NL"

pattern PhoneNumberCountryCode_NO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NO = PhoneNumberCountryCode' "NO"

pattern PhoneNumberCountryCode_NP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NP = PhoneNumberCountryCode' "NP"

pattern PhoneNumberCountryCode_NR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NR = PhoneNumberCountryCode' "NR"

pattern PhoneNumberCountryCode_NU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NU = PhoneNumberCountryCode' "NU"

pattern PhoneNumberCountryCode_NZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_NZ = PhoneNumberCountryCode' "NZ"

pattern PhoneNumberCountryCode_OM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_OM = PhoneNumberCountryCode' "OM"

pattern PhoneNumberCountryCode_PA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PA = PhoneNumberCountryCode' "PA"

pattern PhoneNumberCountryCode_PE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PE = PhoneNumberCountryCode' "PE"

pattern PhoneNumberCountryCode_PF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PF = PhoneNumberCountryCode' "PF"

pattern PhoneNumberCountryCode_PG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PG = PhoneNumberCountryCode' "PG"

pattern PhoneNumberCountryCode_PH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PH = PhoneNumberCountryCode' "PH"

pattern PhoneNumberCountryCode_PK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PK = PhoneNumberCountryCode' "PK"

pattern PhoneNumberCountryCode_PL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PL = PhoneNumberCountryCode' "PL"

pattern PhoneNumberCountryCode_PM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PM = PhoneNumberCountryCode' "PM"

pattern PhoneNumberCountryCode_PN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PN = PhoneNumberCountryCode' "PN"

pattern PhoneNumberCountryCode_PR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PR = PhoneNumberCountryCode' "PR"

pattern PhoneNumberCountryCode_PT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PT = PhoneNumberCountryCode' "PT"

pattern PhoneNumberCountryCode_PW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PW = PhoneNumberCountryCode' "PW"

pattern PhoneNumberCountryCode_PY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_PY = PhoneNumberCountryCode' "PY"

pattern PhoneNumberCountryCode_QA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_QA = PhoneNumberCountryCode' "QA"

pattern PhoneNumberCountryCode_RE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_RE = PhoneNumberCountryCode' "RE"

pattern PhoneNumberCountryCode_RO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_RO = PhoneNumberCountryCode' "RO"

pattern PhoneNumberCountryCode_RS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_RS = PhoneNumberCountryCode' "RS"

pattern PhoneNumberCountryCode_RU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_RU = PhoneNumberCountryCode' "RU"

pattern PhoneNumberCountryCode_RW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_RW = PhoneNumberCountryCode' "RW"

pattern PhoneNumberCountryCode_SA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SA = PhoneNumberCountryCode' "SA"

pattern PhoneNumberCountryCode_SB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SB = PhoneNumberCountryCode' "SB"

pattern PhoneNumberCountryCode_SC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SC = PhoneNumberCountryCode' "SC"

pattern PhoneNumberCountryCode_SD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SD = PhoneNumberCountryCode' "SD"

pattern PhoneNumberCountryCode_SE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SE = PhoneNumberCountryCode' "SE"

pattern PhoneNumberCountryCode_SG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SG = PhoneNumberCountryCode' "SG"

pattern PhoneNumberCountryCode_SH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SH = PhoneNumberCountryCode' "SH"

pattern PhoneNumberCountryCode_SI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SI = PhoneNumberCountryCode' "SI"

pattern PhoneNumberCountryCode_SJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SJ = PhoneNumberCountryCode' "SJ"

pattern PhoneNumberCountryCode_SK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SK = PhoneNumberCountryCode' "SK"

pattern PhoneNumberCountryCode_SL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SL = PhoneNumberCountryCode' "SL"

pattern PhoneNumberCountryCode_SM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SM = PhoneNumberCountryCode' "SM"

pattern PhoneNumberCountryCode_SN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SN = PhoneNumberCountryCode' "SN"

pattern PhoneNumberCountryCode_SO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SO = PhoneNumberCountryCode' "SO"

pattern PhoneNumberCountryCode_SR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SR = PhoneNumberCountryCode' "SR"

pattern PhoneNumberCountryCode_ST :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ST = PhoneNumberCountryCode' "ST"

pattern PhoneNumberCountryCode_SV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SV = PhoneNumberCountryCode' "SV"

pattern PhoneNumberCountryCode_SX :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SX = PhoneNumberCountryCode' "SX"

pattern PhoneNumberCountryCode_SY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SY = PhoneNumberCountryCode' "SY"

pattern PhoneNumberCountryCode_SZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_SZ = PhoneNumberCountryCode' "SZ"

pattern PhoneNumberCountryCode_TC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TC = PhoneNumberCountryCode' "TC"

pattern PhoneNumberCountryCode_TD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TD = PhoneNumberCountryCode' "TD"

pattern PhoneNumberCountryCode_TG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TG = PhoneNumberCountryCode' "TG"

pattern PhoneNumberCountryCode_TH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TH = PhoneNumberCountryCode' "TH"

pattern PhoneNumberCountryCode_TJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TJ = PhoneNumberCountryCode' "TJ"

pattern PhoneNumberCountryCode_TK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TK = PhoneNumberCountryCode' "TK"

pattern PhoneNumberCountryCode_TL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TL = PhoneNumberCountryCode' "TL"

pattern PhoneNumberCountryCode_TM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TM = PhoneNumberCountryCode' "TM"

pattern PhoneNumberCountryCode_TN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TN = PhoneNumberCountryCode' "TN"

pattern PhoneNumberCountryCode_TO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TO = PhoneNumberCountryCode' "TO"

pattern PhoneNumberCountryCode_TR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TR = PhoneNumberCountryCode' "TR"

pattern PhoneNumberCountryCode_TT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TT = PhoneNumberCountryCode' "TT"

pattern PhoneNumberCountryCode_TV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TV = PhoneNumberCountryCode' "TV"

pattern PhoneNumberCountryCode_TW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TW = PhoneNumberCountryCode' "TW"

pattern PhoneNumberCountryCode_TZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_TZ = PhoneNumberCountryCode' "TZ"

pattern PhoneNumberCountryCode_UA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_UA = PhoneNumberCountryCode' "UA"

pattern PhoneNumberCountryCode_UG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_UG = PhoneNumberCountryCode' "UG"

pattern PhoneNumberCountryCode_US :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_US = PhoneNumberCountryCode' "US"

pattern PhoneNumberCountryCode_UY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_UY = PhoneNumberCountryCode' "UY"

pattern PhoneNumberCountryCode_UZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_UZ = PhoneNumberCountryCode' "UZ"

pattern PhoneNumberCountryCode_VA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_VA = PhoneNumberCountryCode' "VA"

pattern PhoneNumberCountryCode_VC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_VC = PhoneNumberCountryCode' "VC"

pattern PhoneNumberCountryCode_VE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_VE = PhoneNumberCountryCode' "VE"

pattern PhoneNumberCountryCode_VG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_VG = PhoneNumberCountryCode' "VG"

pattern PhoneNumberCountryCode_VI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_VI = PhoneNumberCountryCode' "VI"

pattern PhoneNumberCountryCode_VN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_VN = PhoneNumberCountryCode' "VN"

pattern PhoneNumberCountryCode_VU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_VU = PhoneNumberCountryCode' "VU"

pattern PhoneNumberCountryCode_WF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_WF = PhoneNumberCountryCode' "WF"

pattern PhoneNumberCountryCode_WS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_WS = PhoneNumberCountryCode' "WS"

pattern PhoneNumberCountryCode_YE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_YE = PhoneNumberCountryCode' "YE"

pattern PhoneNumberCountryCode_YT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_YT = PhoneNumberCountryCode' "YT"

pattern PhoneNumberCountryCode_ZA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ZA = PhoneNumberCountryCode' "ZA"

pattern PhoneNumberCountryCode_ZM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ZM = PhoneNumberCountryCode' "ZM"

pattern PhoneNumberCountryCode_ZW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCode_ZW = PhoneNumberCountryCode' "ZW"

{-# COMPLETE
  PhoneNumberCountryCode_AD,
  PhoneNumberCountryCode_AE,
  PhoneNumberCountryCode_AF,
  PhoneNumberCountryCode_AG,
  PhoneNumberCountryCode_AI,
  PhoneNumberCountryCode_AL,
  PhoneNumberCountryCode_AM,
  PhoneNumberCountryCode_AN,
  PhoneNumberCountryCode_AO,
  PhoneNumberCountryCode_AQ,
  PhoneNumberCountryCode_AR,
  PhoneNumberCountryCode_AS,
  PhoneNumberCountryCode_AT,
  PhoneNumberCountryCode_AU,
  PhoneNumberCountryCode_AW,
  PhoneNumberCountryCode_AZ,
  PhoneNumberCountryCode_BA,
  PhoneNumberCountryCode_BB,
  PhoneNumberCountryCode_BD,
  PhoneNumberCountryCode_BE,
  PhoneNumberCountryCode_BF,
  PhoneNumberCountryCode_BG,
  PhoneNumberCountryCode_BH,
  PhoneNumberCountryCode_BI,
  PhoneNumberCountryCode_BJ,
  PhoneNumberCountryCode_BL,
  PhoneNumberCountryCode_BM,
  PhoneNumberCountryCode_BN,
  PhoneNumberCountryCode_BO,
  PhoneNumberCountryCode_BR,
  PhoneNumberCountryCode_BS,
  PhoneNumberCountryCode_BT,
  PhoneNumberCountryCode_BW,
  PhoneNumberCountryCode_BY,
  PhoneNumberCountryCode_BZ,
  PhoneNumberCountryCode_CA,
  PhoneNumberCountryCode_CC,
  PhoneNumberCountryCode_CD,
  PhoneNumberCountryCode_CF,
  PhoneNumberCountryCode_CG,
  PhoneNumberCountryCode_CH,
  PhoneNumberCountryCode_CI,
  PhoneNumberCountryCode_CK,
  PhoneNumberCountryCode_CL,
  PhoneNumberCountryCode_CM,
  PhoneNumberCountryCode_CN,
  PhoneNumberCountryCode_CO,
  PhoneNumberCountryCode_CR,
  PhoneNumberCountryCode_CU,
  PhoneNumberCountryCode_CV,
  PhoneNumberCountryCode_CW,
  PhoneNumberCountryCode_CX,
  PhoneNumberCountryCode_CY,
  PhoneNumberCountryCode_CZ,
  PhoneNumberCountryCode_DE,
  PhoneNumberCountryCode_DJ,
  PhoneNumberCountryCode_DK,
  PhoneNumberCountryCode_DM,
  PhoneNumberCountryCode_DO,
  PhoneNumberCountryCode_DZ,
  PhoneNumberCountryCode_EC,
  PhoneNumberCountryCode_EE,
  PhoneNumberCountryCode_EG,
  PhoneNumberCountryCode_EH,
  PhoneNumberCountryCode_ER,
  PhoneNumberCountryCode_ES,
  PhoneNumberCountryCode_ET,
  PhoneNumberCountryCode_FI,
  PhoneNumberCountryCode_FJ,
  PhoneNumberCountryCode_FK,
  PhoneNumberCountryCode_FM,
  PhoneNumberCountryCode_FO,
  PhoneNumberCountryCode_FR,
  PhoneNumberCountryCode_GA,
  PhoneNumberCountryCode_GB,
  PhoneNumberCountryCode_GD,
  PhoneNumberCountryCode_GE,
  PhoneNumberCountryCode_GG,
  PhoneNumberCountryCode_GH,
  PhoneNumberCountryCode_GI,
  PhoneNumberCountryCode_GL,
  PhoneNumberCountryCode_GM,
  PhoneNumberCountryCode_GN,
  PhoneNumberCountryCode_GQ,
  PhoneNumberCountryCode_GR,
  PhoneNumberCountryCode_GT,
  PhoneNumberCountryCode_GU,
  PhoneNumberCountryCode_GW,
  PhoneNumberCountryCode_GY,
  PhoneNumberCountryCode_HK,
  PhoneNumberCountryCode_HN,
  PhoneNumberCountryCode_HR,
  PhoneNumberCountryCode_HT,
  PhoneNumberCountryCode_HU,
  PhoneNumberCountryCode_ID,
  PhoneNumberCountryCode_IE,
  PhoneNumberCountryCode_IL,
  PhoneNumberCountryCode_IM,
  PhoneNumberCountryCode_IN,
  PhoneNumberCountryCode_IO,
  PhoneNumberCountryCode_IQ,
  PhoneNumberCountryCode_IR,
  PhoneNumberCountryCode_IS,
  PhoneNumberCountryCode_IT,
  PhoneNumberCountryCode_JE,
  PhoneNumberCountryCode_JM,
  PhoneNumberCountryCode_JO,
  PhoneNumberCountryCode_JP,
  PhoneNumberCountryCode_KE,
  PhoneNumberCountryCode_KG,
  PhoneNumberCountryCode_KH,
  PhoneNumberCountryCode_KI,
  PhoneNumberCountryCode_KM,
  PhoneNumberCountryCode_KN,
  PhoneNumberCountryCode_KP,
  PhoneNumberCountryCode_KR,
  PhoneNumberCountryCode_KW,
  PhoneNumberCountryCode_KY,
  PhoneNumberCountryCode_KZ,
  PhoneNumberCountryCode_LA,
  PhoneNumberCountryCode_LB,
  PhoneNumberCountryCode_LC,
  PhoneNumberCountryCode_LI,
  PhoneNumberCountryCode_LK,
  PhoneNumberCountryCode_LR,
  PhoneNumberCountryCode_LS,
  PhoneNumberCountryCode_LT,
  PhoneNumberCountryCode_LU,
  PhoneNumberCountryCode_LV,
  PhoneNumberCountryCode_LY,
  PhoneNumberCountryCode_MA,
  PhoneNumberCountryCode_MC,
  PhoneNumberCountryCode_MD,
  PhoneNumberCountryCode_ME,
  PhoneNumberCountryCode_MF,
  PhoneNumberCountryCode_MG,
  PhoneNumberCountryCode_MH,
  PhoneNumberCountryCode_MK,
  PhoneNumberCountryCode_ML,
  PhoneNumberCountryCode_MM,
  PhoneNumberCountryCode_MN,
  PhoneNumberCountryCode_MO,
  PhoneNumberCountryCode_MP,
  PhoneNumberCountryCode_MR,
  PhoneNumberCountryCode_MS,
  PhoneNumberCountryCode_MT,
  PhoneNumberCountryCode_MU,
  PhoneNumberCountryCode_MV,
  PhoneNumberCountryCode_MW,
  PhoneNumberCountryCode_MX,
  PhoneNumberCountryCode_MY,
  PhoneNumberCountryCode_MZ,
  PhoneNumberCountryCode_NA,
  PhoneNumberCountryCode_NC,
  PhoneNumberCountryCode_NE,
  PhoneNumberCountryCode_NG,
  PhoneNumberCountryCode_NI,
  PhoneNumberCountryCode_NL,
  PhoneNumberCountryCode_NO,
  PhoneNumberCountryCode_NP,
  PhoneNumberCountryCode_NR,
  PhoneNumberCountryCode_NU,
  PhoneNumberCountryCode_NZ,
  PhoneNumberCountryCode_OM,
  PhoneNumberCountryCode_PA,
  PhoneNumberCountryCode_PE,
  PhoneNumberCountryCode_PF,
  PhoneNumberCountryCode_PG,
  PhoneNumberCountryCode_PH,
  PhoneNumberCountryCode_PK,
  PhoneNumberCountryCode_PL,
  PhoneNumberCountryCode_PM,
  PhoneNumberCountryCode_PN,
  PhoneNumberCountryCode_PR,
  PhoneNumberCountryCode_PT,
  PhoneNumberCountryCode_PW,
  PhoneNumberCountryCode_PY,
  PhoneNumberCountryCode_QA,
  PhoneNumberCountryCode_RE,
  PhoneNumberCountryCode_RO,
  PhoneNumberCountryCode_RS,
  PhoneNumberCountryCode_RU,
  PhoneNumberCountryCode_RW,
  PhoneNumberCountryCode_SA,
  PhoneNumberCountryCode_SB,
  PhoneNumberCountryCode_SC,
  PhoneNumberCountryCode_SD,
  PhoneNumberCountryCode_SE,
  PhoneNumberCountryCode_SG,
  PhoneNumberCountryCode_SH,
  PhoneNumberCountryCode_SI,
  PhoneNumberCountryCode_SJ,
  PhoneNumberCountryCode_SK,
  PhoneNumberCountryCode_SL,
  PhoneNumberCountryCode_SM,
  PhoneNumberCountryCode_SN,
  PhoneNumberCountryCode_SO,
  PhoneNumberCountryCode_SR,
  PhoneNumberCountryCode_ST,
  PhoneNumberCountryCode_SV,
  PhoneNumberCountryCode_SX,
  PhoneNumberCountryCode_SY,
  PhoneNumberCountryCode_SZ,
  PhoneNumberCountryCode_TC,
  PhoneNumberCountryCode_TD,
  PhoneNumberCountryCode_TG,
  PhoneNumberCountryCode_TH,
  PhoneNumberCountryCode_TJ,
  PhoneNumberCountryCode_TK,
  PhoneNumberCountryCode_TL,
  PhoneNumberCountryCode_TM,
  PhoneNumberCountryCode_TN,
  PhoneNumberCountryCode_TO,
  PhoneNumberCountryCode_TR,
  PhoneNumberCountryCode_TT,
  PhoneNumberCountryCode_TV,
  PhoneNumberCountryCode_TW,
  PhoneNumberCountryCode_TZ,
  PhoneNumberCountryCode_UA,
  PhoneNumberCountryCode_UG,
  PhoneNumberCountryCode_US,
  PhoneNumberCountryCode_UY,
  PhoneNumberCountryCode_UZ,
  PhoneNumberCountryCode_VA,
  PhoneNumberCountryCode_VC,
  PhoneNumberCountryCode_VE,
  PhoneNumberCountryCode_VG,
  PhoneNumberCountryCode_VI,
  PhoneNumberCountryCode_VN,
  PhoneNumberCountryCode_VU,
  PhoneNumberCountryCode_WF,
  PhoneNumberCountryCode_WS,
  PhoneNumberCountryCode_YE,
  PhoneNumberCountryCode_YT,
  PhoneNumberCountryCode_ZA,
  PhoneNumberCountryCode_ZM,
  PhoneNumberCountryCode_ZW,
  PhoneNumberCountryCode'
  #-}

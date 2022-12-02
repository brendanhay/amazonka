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
-- Module      : Amazonka.WAF.Types.GeoMatchConstraintValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.GeoMatchConstraintValue
  ( GeoMatchConstraintValue
      ( ..,
        GeoMatchConstraintValue_AD,
        GeoMatchConstraintValue_AE,
        GeoMatchConstraintValue_AF,
        GeoMatchConstraintValue_AG,
        GeoMatchConstraintValue_AI,
        GeoMatchConstraintValue_AL,
        GeoMatchConstraintValue_AM,
        GeoMatchConstraintValue_AO,
        GeoMatchConstraintValue_AQ,
        GeoMatchConstraintValue_AR,
        GeoMatchConstraintValue_AS,
        GeoMatchConstraintValue_AT,
        GeoMatchConstraintValue_AU,
        GeoMatchConstraintValue_AW,
        GeoMatchConstraintValue_AX,
        GeoMatchConstraintValue_AZ,
        GeoMatchConstraintValue_BA,
        GeoMatchConstraintValue_BB,
        GeoMatchConstraintValue_BD,
        GeoMatchConstraintValue_BE,
        GeoMatchConstraintValue_BF,
        GeoMatchConstraintValue_BG,
        GeoMatchConstraintValue_BH,
        GeoMatchConstraintValue_BI,
        GeoMatchConstraintValue_BJ,
        GeoMatchConstraintValue_BL,
        GeoMatchConstraintValue_BM,
        GeoMatchConstraintValue_BN,
        GeoMatchConstraintValue_BO,
        GeoMatchConstraintValue_BQ,
        GeoMatchConstraintValue_BR,
        GeoMatchConstraintValue_BS,
        GeoMatchConstraintValue_BT,
        GeoMatchConstraintValue_BV,
        GeoMatchConstraintValue_BW,
        GeoMatchConstraintValue_BY,
        GeoMatchConstraintValue_BZ,
        GeoMatchConstraintValue_CA,
        GeoMatchConstraintValue_CC,
        GeoMatchConstraintValue_CD,
        GeoMatchConstraintValue_CF,
        GeoMatchConstraintValue_CG,
        GeoMatchConstraintValue_CH,
        GeoMatchConstraintValue_CI,
        GeoMatchConstraintValue_CK,
        GeoMatchConstraintValue_CL,
        GeoMatchConstraintValue_CM,
        GeoMatchConstraintValue_CN,
        GeoMatchConstraintValue_CO,
        GeoMatchConstraintValue_CR,
        GeoMatchConstraintValue_CU,
        GeoMatchConstraintValue_CV,
        GeoMatchConstraintValue_CW,
        GeoMatchConstraintValue_CX,
        GeoMatchConstraintValue_CY,
        GeoMatchConstraintValue_CZ,
        GeoMatchConstraintValue_DE,
        GeoMatchConstraintValue_DJ,
        GeoMatchConstraintValue_DK,
        GeoMatchConstraintValue_DM,
        GeoMatchConstraintValue_DO,
        GeoMatchConstraintValue_DZ,
        GeoMatchConstraintValue_EC,
        GeoMatchConstraintValue_EE,
        GeoMatchConstraintValue_EG,
        GeoMatchConstraintValue_EH,
        GeoMatchConstraintValue_ER,
        GeoMatchConstraintValue_ES,
        GeoMatchConstraintValue_ET,
        GeoMatchConstraintValue_FI,
        GeoMatchConstraintValue_FJ,
        GeoMatchConstraintValue_FK,
        GeoMatchConstraintValue_FM,
        GeoMatchConstraintValue_FO,
        GeoMatchConstraintValue_FR,
        GeoMatchConstraintValue_GA,
        GeoMatchConstraintValue_GB,
        GeoMatchConstraintValue_GD,
        GeoMatchConstraintValue_GE,
        GeoMatchConstraintValue_GF,
        GeoMatchConstraintValue_GG,
        GeoMatchConstraintValue_GH,
        GeoMatchConstraintValue_GI,
        GeoMatchConstraintValue_GL,
        GeoMatchConstraintValue_GM,
        GeoMatchConstraintValue_GN,
        GeoMatchConstraintValue_GP,
        GeoMatchConstraintValue_GQ,
        GeoMatchConstraintValue_GR,
        GeoMatchConstraintValue_GS,
        GeoMatchConstraintValue_GT,
        GeoMatchConstraintValue_GU,
        GeoMatchConstraintValue_GW,
        GeoMatchConstraintValue_GY,
        GeoMatchConstraintValue_HK,
        GeoMatchConstraintValue_HM,
        GeoMatchConstraintValue_HN,
        GeoMatchConstraintValue_HR,
        GeoMatchConstraintValue_HT,
        GeoMatchConstraintValue_HU,
        GeoMatchConstraintValue_ID,
        GeoMatchConstraintValue_IE,
        GeoMatchConstraintValue_IL,
        GeoMatchConstraintValue_IM,
        GeoMatchConstraintValue_IN,
        GeoMatchConstraintValue_IO,
        GeoMatchConstraintValue_IQ,
        GeoMatchConstraintValue_IR,
        GeoMatchConstraintValue_IS,
        GeoMatchConstraintValue_IT,
        GeoMatchConstraintValue_JE,
        GeoMatchConstraintValue_JM,
        GeoMatchConstraintValue_JO,
        GeoMatchConstraintValue_JP,
        GeoMatchConstraintValue_KE,
        GeoMatchConstraintValue_KG,
        GeoMatchConstraintValue_KH,
        GeoMatchConstraintValue_KI,
        GeoMatchConstraintValue_KM,
        GeoMatchConstraintValue_KN,
        GeoMatchConstraintValue_KP,
        GeoMatchConstraintValue_KR,
        GeoMatchConstraintValue_KW,
        GeoMatchConstraintValue_KY,
        GeoMatchConstraintValue_KZ,
        GeoMatchConstraintValue_LA,
        GeoMatchConstraintValue_LB,
        GeoMatchConstraintValue_LC,
        GeoMatchConstraintValue_LI,
        GeoMatchConstraintValue_LK,
        GeoMatchConstraintValue_LR,
        GeoMatchConstraintValue_LS,
        GeoMatchConstraintValue_LT,
        GeoMatchConstraintValue_LU,
        GeoMatchConstraintValue_LV,
        GeoMatchConstraintValue_LY,
        GeoMatchConstraintValue_MA,
        GeoMatchConstraintValue_MC,
        GeoMatchConstraintValue_MD,
        GeoMatchConstraintValue_ME,
        GeoMatchConstraintValue_MF,
        GeoMatchConstraintValue_MG,
        GeoMatchConstraintValue_MH,
        GeoMatchConstraintValue_MK,
        GeoMatchConstraintValue_ML,
        GeoMatchConstraintValue_MM,
        GeoMatchConstraintValue_MN,
        GeoMatchConstraintValue_MO,
        GeoMatchConstraintValue_MP,
        GeoMatchConstraintValue_MQ,
        GeoMatchConstraintValue_MR,
        GeoMatchConstraintValue_MS,
        GeoMatchConstraintValue_MT,
        GeoMatchConstraintValue_MU,
        GeoMatchConstraintValue_MV,
        GeoMatchConstraintValue_MW,
        GeoMatchConstraintValue_MX,
        GeoMatchConstraintValue_MY,
        GeoMatchConstraintValue_MZ,
        GeoMatchConstraintValue_NA,
        GeoMatchConstraintValue_NC,
        GeoMatchConstraintValue_NE,
        GeoMatchConstraintValue_NF,
        GeoMatchConstraintValue_NG,
        GeoMatchConstraintValue_NI,
        GeoMatchConstraintValue_NL,
        GeoMatchConstraintValue_NO,
        GeoMatchConstraintValue_NP,
        GeoMatchConstraintValue_NR,
        GeoMatchConstraintValue_NU,
        GeoMatchConstraintValue_NZ,
        GeoMatchConstraintValue_OM,
        GeoMatchConstraintValue_PA,
        GeoMatchConstraintValue_PE,
        GeoMatchConstraintValue_PF,
        GeoMatchConstraintValue_PG,
        GeoMatchConstraintValue_PH,
        GeoMatchConstraintValue_PK,
        GeoMatchConstraintValue_PL,
        GeoMatchConstraintValue_PM,
        GeoMatchConstraintValue_PN,
        GeoMatchConstraintValue_PR,
        GeoMatchConstraintValue_PS,
        GeoMatchConstraintValue_PT,
        GeoMatchConstraintValue_PW,
        GeoMatchConstraintValue_PY,
        GeoMatchConstraintValue_QA,
        GeoMatchConstraintValue_RE,
        GeoMatchConstraintValue_RO,
        GeoMatchConstraintValue_RS,
        GeoMatchConstraintValue_RU,
        GeoMatchConstraintValue_RW,
        GeoMatchConstraintValue_SA,
        GeoMatchConstraintValue_SB,
        GeoMatchConstraintValue_SC,
        GeoMatchConstraintValue_SD,
        GeoMatchConstraintValue_SE,
        GeoMatchConstraintValue_SG,
        GeoMatchConstraintValue_SH,
        GeoMatchConstraintValue_SI,
        GeoMatchConstraintValue_SJ,
        GeoMatchConstraintValue_SK,
        GeoMatchConstraintValue_SL,
        GeoMatchConstraintValue_SM,
        GeoMatchConstraintValue_SN,
        GeoMatchConstraintValue_SO,
        GeoMatchConstraintValue_SR,
        GeoMatchConstraintValue_SS,
        GeoMatchConstraintValue_ST,
        GeoMatchConstraintValue_SV,
        GeoMatchConstraintValue_SX,
        GeoMatchConstraintValue_SY,
        GeoMatchConstraintValue_SZ,
        GeoMatchConstraintValue_TC,
        GeoMatchConstraintValue_TD,
        GeoMatchConstraintValue_TF,
        GeoMatchConstraintValue_TG,
        GeoMatchConstraintValue_TH,
        GeoMatchConstraintValue_TJ,
        GeoMatchConstraintValue_TK,
        GeoMatchConstraintValue_TL,
        GeoMatchConstraintValue_TM,
        GeoMatchConstraintValue_TN,
        GeoMatchConstraintValue_TO,
        GeoMatchConstraintValue_TR,
        GeoMatchConstraintValue_TT,
        GeoMatchConstraintValue_TV,
        GeoMatchConstraintValue_TW,
        GeoMatchConstraintValue_TZ,
        GeoMatchConstraintValue_UA,
        GeoMatchConstraintValue_UG,
        GeoMatchConstraintValue_UM,
        GeoMatchConstraintValue_US,
        GeoMatchConstraintValue_UY,
        GeoMatchConstraintValue_UZ,
        GeoMatchConstraintValue_VA,
        GeoMatchConstraintValue_VC,
        GeoMatchConstraintValue_VE,
        GeoMatchConstraintValue_VG,
        GeoMatchConstraintValue_VI,
        GeoMatchConstraintValue_VN,
        GeoMatchConstraintValue_VU,
        GeoMatchConstraintValue_WF,
        GeoMatchConstraintValue_WS,
        GeoMatchConstraintValue_YE,
        GeoMatchConstraintValue_YT,
        GeoMatchConstraintValue_ZA,
        GeoMatchConstraintValue_ZM,
        GeoMatchConstraintValue_ZW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GeoMatchConstraintValue = GeoMatchConstraintValue'
  { fromGeoMatchConstraintValue ::
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

pattern GeoMatchConstraintValue_AD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AD = GeoMatchConstraintValue' "AD"

pattern GeoMatchConstraintValue_AE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AE = GeoMatchConstraintValue' "AE"

pattern GeoMatchConstraintValue_AF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AF = GeoMatchConstraintValue' "AF"

pattern GeoMatchConstraintValue_AG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AG = GeoMatchConstraintValue' "AG"

pattern GeoMatchConstraintValue_AI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AI = GeoMatchConstraintValue' "AI"

pattern GeoMatchConstraintValue_AL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AL = GeoMatchConstraintValue' "AL"

pattern GeoMatchConstraintValue_AM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AM = GeoMatchConstraintValue' "AM"

pattern GeoMatchConstraintValue_AO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AO = GeoMatchConstraintValue' "AO"

pattern GeoMatchConstraintValue_AQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AQ = GeoMatchConstraintValue' "AQ"

pattern GeoMatchConstraintValue_AR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AR = GeoMatchConstraintValue' "AR"

pattern GeoMatchConstraintValue_AS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AS = GeoMatchConstraintValue' "AS"

pattern GeoMatchConstraintValue_AT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AT = GeoMatchConstraintValue' "AT"

pattern GeoMatchConstraintValue_AU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AU = GeoMatchConstraintValue' "AU"

pattern GeoMatchConstraintValue_AW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AW = GeoMatchConstraintValue' "AW"

pattern GeoMatchConstraintValue_AX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AX = GeoMatchConstraintValue' "AX"

pattern GeoMatchConstraintValue_AZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_AZ = GeoMatchConstraintValue' "AZ"

pattern GeoMatchConstraintValue_BA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BA = GeoMatchConstraintValue' "BA"

pattern GeoMatchConstraintValue_BB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BB = GeoMatchConstraintValue' "BB"

pattern GeoMatchConstraintValue_BD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BD = GeoMatchConstraintValue' "BD"

pattern GeoMatchConstraintValue_BE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BE = GeoMatchConstraintValue' "BE"

pattern GeoMatchConstraintValue_BF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BF = GeoMatchConstraintValue' "BF"

pattern GeoMatchConstraintValue_BG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BG = GeoMatchConstraintValue' "BG"

pattern GeoMatchConstraintValue_BH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BH = GeoMatchConstraintValue' "BH"

pattern GeoMatchConstraintValue_BI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BI = GeoMatchConstraintValue' "BI"

pattern GeoMatchConstraintValue_BJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BJ = GeoMatchConstraintValue' "BJ"

pattern GeoMatchConstraintValue_BL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BL = GeoMatchConstraintValue' "BL"

pattern GeoMatchConstraintValue_BM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BM = GeoMatchConstraintValue' "BM"

pattern GeoMatchConstraintValue_BN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BN = GeoMatchConstraintValue' "BN"

pattern GeoMatchConstraintValue_BO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BO = GeoMatchConstraintValue' "BO"

pattern GeoMatchConstraintValue_BQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BQ = GeoMatchConstraintValue' "BQ"

pattern GeoMatchConstraintValue_BR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BR = GeoMatchConstraintValue' "BR"

pattern GeoMatchConstraintValue_BS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BS = GeoMatchConstraintValue' "BS"

pattern GeoMatchConstraintValue_BT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BT = GeoMatchConstraintValue' "BT"

pattern GeoMatchConstraintValue_BV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BV = GeoMatchConstraintValue' "BV"

pattern GeoMatchConstraintValue_BW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BW = GeoMatchConstraintValue' "BW"

pattern GeoMatchConstraintValue_BY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BY = GeoMatchConstraintValue' "BY"

pattern GeoMatchConstraintValue_BZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_BZ = GeoMatchConstraintValue' "BZ"

pattern GeoMatchConstraintValue_CA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CA = GeoMatchConstraintValue' "CA"

pattern GeoMatchConstraintValue_CC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CC = GeoMatchConstraintValue' "CC"

pattern GeoMatchConstraintValue_CD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CD = GeoMatchConstraintValue' "CD"

pattern GeoMatchConstraintValue_CF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CF = GeoMatchConstraintValue' "CF"

pattern GeoMatchConstraintValue_CG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CG = GeoMatchConstraintValue' "CG"

pattern GeoMatchConstraintValue_CH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CH = GeoMatchConstraintValue' "CH"

pattern GeoMatchConstraintValue_CI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CI = GeoMatchConstraintValue' "CI"

pattern GeoMatchConstraintValue_CK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CK = GeoMatchConstraintValue' "CK"

pattern GeoMatchConstraintValue_CL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CL = GeoMatchConstraintValue' "CL"

pattern GeoMatchConstraintValue_CM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CM = GeoMatchConstraintValue' "CM"

pattern GeoMatchConstraintValue_CN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CN = GeoMatchConstraintValue' "CN"

pattern GeoMatchConstraintValue_CO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CO = GeoMatchConstraintValue' "CO"

pattern GeoMatchConstraintValue_CR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CR = GeoMatchConstraintValue' "CR"

pattern GeoMatchConstraintValue_CU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CU = GeoMatchConstraintValue' "CU"

pattern GeoMatchConstraintValue_CV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CV = GeoMatchConstraintValue' "CV"

pattern GeoMatchConstraintValue_CW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CW = GeoMatchConstraintValue' "CW"

pattern GeoMatchConstraintValue_CX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CX = GeoMatchConstraintValue' "CX"

pattern GeoMatchConstraintValue_CY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CY = GeoMatchConstraintValue' "CY"

pattern GeoMatchConstraintValue_CZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_CZ = GeoMatchConstraintValue' "CZ"

pattern GeoMatchConstraintValue_DE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_DE = GeoMatchConstraintValue' "DE"

pattern GeoMatchConstraintValue_DJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_DJ = GeoMatchConstraintValue' "DJ"

pattern GeoMatchConstraintValue_DK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_DK = GeoMatchConstraintValue' "DK"

pattern GeoMatchConstraintValue_DM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_DM = GeoMatchConstraintValue' "DM"

pattern GeoMatchConstraintValue_DO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_DO = GeoMatchConstraintValue' "DO"

pattern GeoMatchConstraintValue_DZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_DZ = GeoMatchConstraintValue' "DZ"

pattern GeoMatchConstraintValue_EC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_EC = GeoMatchConstraintValue' "EC"

pattern GeoMatchConstraintValue_EE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_EE = GeoMatchConstraintValue' "EE"

pattern GeoMatchConstraintValue_EG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_EG = GeoMatchConstraintValue' "EG"

pattern GeoMatchConstraintValue_EH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_EH = GeoMatchConstraintValue' "EH"

pattern GeoMatchConstraintValue_ER :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ER = GeoMatchConstraintValue' "ER"

pattern GeoMatchConstraintValue_ES :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ES = GeoMatchConstraintValue' "ES"

pattern GeoMatchConstraintValue_ET :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ET = GeoMatchConstraintValue' "ET"

pattern GeoMatchConstraintValue_FI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_FI = GeoMatchConstraintValue' "FI"

pattern GeoMatchConstraintValue_FJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_FJ = GeoMatchConstraintValue' "FJ"

pattern GeoMatchConstraintValue_FK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_FK = GeoMatchConstraintValue' "FK"

pattern GeoMatchConstraintValue_FM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_FM = GeoMatchConstraintValue' "FM"

pattern GeoMatchConstraintValue_FO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_FO = GeoMatchConstraintValue' "FO"

pattern GeoMatchConstraintValue_FR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_FR = GeoMatchConstraintValue' "FR"

pattern GeoMatchConstraintValue_GA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GA = GeoMatchConstraintValue' "GA"

pattern GeoMatchConstraintValue_GB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GB = GeoMatchConstraintValue' "GB"

pattern GeoMatchConstraintValue_GD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GD = GeoMatchConstraintValue' "GD"

pattern GeoMatchConstraintValue_GE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GE = GeoMatchConstraintValue' "GE"

pattern GeoMatchConstraintValue_GF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GF = GeoMatchConstraintValue' "GF"

pattern GeoMatchConstraintValue_GG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GG = GeoMatchConstraintValue' "GG"

pattern GeoMatchConstraintValue_GH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GH = GeoMatchConstraintValue' "GH"

pattern GeoMatchConstraintValue_GI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GI = GeoMatchConstraintValue' "GI"

pattern GeoMatchConstraintValue_GL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GL = GeoMatchConstraintValue' "GL"

pattern GeoMatchConstraintValue_GM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GM = GeoMatchConstraintValue' "GM"

pattern GeoMatchConstraintValue_GN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GN = GeoMatchConstraintValue' "GN"

pattern GeoMatchConstraintValue_GP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GP = GeoMatchConstraintValue' "GP"

pattern GeoMatchConstraintValue_GQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GQ = GeoMatchConstraintValue' "GQ"

pattern GeoMatchConstraintValue_GR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GR = GeoMatchConstraintValue' "GR"

pattern GeoMatchConstraintValue_GS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GS = GeoMatchConstraintValue' "GS"

pattern GeoMatchConstraintValue_GT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GT = GeoMatchConstraintValue' "GT"

pattern GeoMatchConstraintValue_GU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GU = GeoMatchConstraintValue' "GU"

pattern GeoMatchConstraintValue_GW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GW = GeoMatchConstraintValue' "GW"

pattern GeoMatchConstraintValue_GY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_GY = GeoMatchConstraintValue' "GY"

pattern GeoMatchConstraintValue_HK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_HK = GeoMatchConstraintValue' "HK"

pattern GeoMatchConstraintValue_HM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_HM = GeoMatchConstraintValue' "HM"

pattern GeoMatchConstraintValue_HN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_HN = GeoMatchConstraintValue' "HN"

pattern GeoMatchConstraintValue_HR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_HR = GeoMatchConstraintValue' "HR"

pattern GeoMatchConstraintValue_HT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_HT = GeoMatchConstraintValue' "HT"

pattern GeoMatchConstraintValue_HU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_HU = GeoMatchConstraintValue' "HU"

pattern GeoMatchConstraintValue_ID :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ID = GeoMatchConstraintValue' "ID"

pattern GeoMatchConstraintValue_IE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IE = GeoMatchConstraintValue' "IE"

pattern GeoMatchConstraintValue_IL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IL = GeoMatchConstraintValue' "IL"

pattern GeoMatchConstraintValue_IM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IM = GeoMatchConstraintValue' "IM"

pattern GeoMatchConstraintValue_IN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IN = GeoMatchConstraintValue' "IN"

pattern GeoMatchConstraintValue_IO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IO = GeoMatchConstraintValue' "IO"

pattern GeoMatchConstraintValue_IQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IQ = GeoMatchConstraintValue' "IQ"

pattern GeoMatchConstraintValue_IR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IR = GeoMatchConstraintValue' "IR"

pattern GeoMatchConstraintValue_IS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IS = GeoMatchConstraintValue' "IS"

pattern GeoMatchConstraintValue_IT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_IT = GeoMatchConstraintValue' "IT"

pattern GeoMatchConstraintValue_JE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_JE = GeoMatchConstraintValue' "JE"

pattern GeoMatchConstraintValue_JM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_JM = GeoMatchConstraintValue' "JM"

pattern GeoMatchConstraintValue_JO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_JO = GeoMatchConstraintValue' "JO"

pattern GeoMatchConstraintValue_JP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_JP = GeoMatchConstraintValue' "JP"

pattern GeoMatchConstraintValue_KE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KE = GeoMatchConstraintValue' "KE"

pattern GeoMatchConstraintValue_KG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KG = GeoMatchConstraintValue' "KG"

pattern GeoMatchConstraintValue_KH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KH = GeoMatchConstraintValue' "KH"

pattern GeoMatchConstraintValue_KI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KI = GeoMatchConstraintValue' "KI"

pattern GeoMatchConstraintValue_KM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KM = GeoMatchConstraintValue' "KM"

pattern GeoMatchConstraintValue_KN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KN = GeoMatchConstraintValue' "KN"

pattern GeoMatchConstraintValue_KP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KP = GeoMatchConstraintValue' "KP"

pattern GeoMatchConstraintValue_KR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KR = GeoMatchConstraintValue' "KR"

pattern GeoMatchConstraintValue_KW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KW = GeoMatchConstraintValue' "KW"

pattern GeoMatchConstraintValue_KY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KY = GeoMatchConstraintValue' "KY"

pattern GeoMatchConstraintValue_KZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_KZ = GeoMatchConstraintValue' "KZ"

pattern GeoMatchConstraintValue_LA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LA = GeoMatchConstraintValue' "LA"

pattern GeoMatchConstraintValue_LB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LB = GeoMatchConstraintValue' "LB"

pattern GeoMatchConstraintValue_LC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LC = GeoMatchConstraintValue' "LC"

pattern GeoMatchConstraintValue_LI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LI = GeoMatchConstraintValue' "LI"

pattern GeoMatchConstraintValue_LK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LK = GeoMatchConstraintValue' "LK"

pattern GeoMatchConstraintValue_LR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LR = GeoMatchConstraintValue' "LR"

pattern GeoMatchConstraintValue_LS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LS = GeoMatchConstraintValue' "LS"

pattern GeoMatchConstraintValue_LT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LT = GeoMatchConstraintValue' "LT"

pattern GeoMatchConstraintValue_LU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LU = GeoMatchConstraintValue' "LU"

pattern GeoMatchConstraintValue_LV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LV = GeoMatchConstraintValue' "LV"

pattern GeoMatchConstraintValue_LY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_LY = GeoMatchConstraintValue' "LY"

pattern GeoMatchConstraintValue_MA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MA = GeoMatchConstraintValue' "MA"

pattern GeoMatchConstraintValue_MC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MC = GeoMatchConstraintValue' "MC"

pattern GeoMatchConstraintValue_MD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MD = GeoMatchConstraintValue' "MD"

pattern GeoMatchConstraintValue_ME :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ME = GeoMatchConstraintValue' "ME"

pattern GeoMatchConstraintValue_MF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MF = GeoMatchConstraintValue' "MF"

pattern GeoMatchConstraintValue_MG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MG = GeoMatchConstraintValue' "MG"

pattern GeoMatchConstraintValue_MH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MH = GeoMatchConstraintValue' "MH"

pattern GeoMatchConstraintValue_MK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MK = GeoMatchConstraintValue' "MK"

pattern GeoMatchConstraintValue_ML :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ML = GeoMatchConstraintValue' "ML"

pattern GeoMatchConstraintValue_MM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MM = GeoMatchConstraintValue' "MM"

pattern GeoMatchConstraintValue_MN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MN = GeoMatchConstraintValue' "MN"

pattern GeoMatchConstraintValue_MO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MO = GeoMatchConstraintValue' "MO"

pattern GeoMatchConstraintValue_MP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MP = GeoMatchConstraintValue' "MP"

pattern GeoMatchConstraintValue_MQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MQ = GeoMatchConstraintValue' "MQ"

pattern GeoMatchConstraintValue_MR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MR = GeoMatchConstraintValue' "MR"

pattern GeoMatchConstraintValue_MS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MS = GeoMatchConstraintValue' "MS"

pattern GeoMatchConstraintValue_MT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MT = GeoMatchConstraintValue' "MT"

pattern GeoMatchConstraintValue_MU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MU = GeoMatchConstraintValue' "MU"

pattern GeoMatchConstraintValue_MV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MV = GeoMatchConstraintValue' "MV"

pattern GeoMatchConstraintValue_MW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MW = GeoMatchConstraintValue' "MW"

pattern GeoMatchConstraintValue_MX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MX = GeoMatchConstraintValue' "MX"

pattern GeoMatchConstraintValue_MY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MY = GeoMatchConstraintValue' "MY"

pattern GeoMatchConstraintValue_MZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_MZ = GeoMatchConstraintValue' "MZ"

pattern GeoMatchConstraintValue_NA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NA = GeoMatchConstraintValue' "NA"

pattern GeoMatchConstraintValue_NC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NC = GeoMatchConstraintValue' "NC"

pattern GeoMatchConstraintValue_NE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NE = GeoMatchConstraintValue' "NE"

pattern GeoMatchConstraintValue_NF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NF = GeoMatchConstraintValue' "NF"

pattern GeoMatchConstraintValue_NG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NG = GeoMatchConstraintValue' "NG"

pattern GeoMatchConstraintValue_NI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NI = GeoMatchConstraintValue' "NI"

pattern GeoMatchConstraintValue_NL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NL = GeoMatchConstraintValue' "NL"

pattern GeoMatchConstraintValue_NO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NO = GeoMatchConstraintValue' "NO"

pattern GeoMatchConstraintValue_NP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NP = GeoMatchConstraintValue' "NP"

pattern GeoMatchConstraintValue_NR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NR = GeoMatchConstraintValue' "NR"

pattern GeoMatchConstraintValue_NU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NU = GeoMatchConstraintValue' "NU"

pattern GeoMatchConstraintValue_NZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_NZ = GeoMatchConstraintValue' "NZ"

pattern GeoMatchConstraintValue_OM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_OM = GeoMatchConstraintValue' "OM"

pattern GeoMatchConstraintValue_PA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PA = GeoMatchConstraintValue' "PA"

pattern GeoMatchConstraintValue_PE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PE = GeoMatchConstraintValue' "PE"

pattern GeoMatchConstraintValue_PF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PF = GeoMatchConstraintValue' "PF"

pattern GeoMatchConstraintValue_PG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PG = GeoMatchConstraintValue' "PG"

pattern GeoMatchConstraintValue_PH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PH = GeoMatchConstraintValue' "PH"

pattern GeoMatchConstraintValue_PK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PK = GeoMatchConstraintValue' "PK"

pattern GeoMatchConstraintValue_PL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PL = GeoMatchConstraintValue' "PL"

pattern GeoMatchConstraintValue_PM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PM = GeoMatchConstraintValue' "PM"

pattern GeoMatchConstraintValue_PN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PN = GeoMatchConstraintValue' "PN"

pattern GeoMatchConstraintValue_PR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PR = GeoMatchConstraintValue' "PR"

pattern GeoMatchConstraintValue_PS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PS = GeoMatchConstraintValue' "PS"

pattern GeoMatchConstraintValue_PT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PT = GeoMatchConstraintValue' "PT"

pattern GeoMatchConstraintValue_PW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PW = GeoMatchConstraintValue' "PW"

pattern GeoMatchConstraintValue_PY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_PY = GeoMatchConstraintValue' "PY"

pattern GeoMatchConstraintValue_QA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_QA = GeoMatchConstraintValue' "QA"

pattern GeoMatchConstraintValue_RE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_RE = GeoMatchConstraintValue' "RE"

pattern GeoMatchConstraintValue_RO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_RO = GeoMatchConstraintValue' "RO"

pattern GeoMatchConstraintValue_RS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_RS = GeoMatchConstraintValue' "RS"

pattern GeoMatchConstraintValue_RU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_RU = GeoMatchConstraintValue' "RU"

pattern GeoMatchConstraintValue_RW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_RW = GeoMatchConstraintValue' "RW"

pattern GeoMatchConstraintValue_SA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SA = GeoMatchConstraintValue' "SA"

pattern GeoMatchConstraintValue_SB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SB = GeoMatchConstraintValue' "SB"

pattern GeoMatchConstraintValue_SC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SC = GeoMatchConstraintValue' "SC"

pattern GeoMatchConstraintValue_SD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SD = GeoMatchConstraintValue' "SD"

pattern GeoMatchConstraintValue_SE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SE = GeoMatchConstraintValue' "SE"

pattern GeoMatchConstraintValue_SG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SG = GeoMatchConstraintValue' "SG"

pattern GeoMatchConstraintValue_SH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SH = GeoMatchConstraintValue' "SH"

pattern GeoMatchConstraintValue_SI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SI = GeoMatchConstraintValue' "SI"

pattern GeoMatchConstraintValue_SJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SJ = GeoMatchConstraintValue' "SJ"

pattern GeoMatchConstraintValue_SK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SK = GeoMatchConstraintValue' "SK"

pattern GeoMatchConstraintValue_SL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SL = GeoMatchConstraintValue' "SL"

pattern GeoMatchConstraintValue_SM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SM = GeoMatchConstraintValue' "SM"

pattern GeoMatchConstraintValue_SN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SN = GeoMatchConstraintValue' "SN"

pattern GeoMatchConstraintValue_SO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SO = GeoMatchConstraintValue' "SO"

pattern GeoMatchConstraintValue_SR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SR = GeoMatchConstraintValue' "SR"

pattern GeoMatchConstraintValue_SS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SS = GeoMatchConstraintValue' "SS"

pattern GeoMatchConstraintValue_ST :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ST = GeoMatchConstraintValue' "ST"

pattern GeoMatchConstraintValue_SV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SV = GeoMatchConstraintValue' "SV"

pattern GeoMatchConstraintValue_SX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SX = GeoMatchConstraintValue' "SX"

pattern GeoMatchConstraintValue_SY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SY = GeoMatchConstraintValue' "SY"

pattern GeoMatchConstraintValue_SZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_SZ = GeoMatchConstraintValue' "SZ"

pattern GeoMatchConstraintValue_TC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TC = GeoMatchConstraintValue' "TC"

pattern GeoMatchConstraintValue_TD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TD = GeoMatchConstraintValue' "TD"

pattern GeoMatchConstraintValue_TF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TF = GeoMatchConstraintValue' "TF"

pattern GeoMatchConstraintValue_TG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TG = GeoMatchConstraintValue' "TG"

pattern GeoMatchConstraintValue_TH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TH = GeoMatchConstraintValue' "TH"

pattern GeoMatchConstraintValue_TJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TJ = GeoMatchConstraintValue' "TJ"

pattern GeoMatchConstraintValue_TK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TK = GeoMatchConstraintValue' "TK"

pattern GeoMatchConstraintValue_TL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TL = GeoMatchConstraintValue' "TL"

pattern GeoMatchConstraintValue_TM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TM = GeoMatchConstraintValue' "TM"

pattern GeoMatchConstraintValue_TN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TN = GeoMatchConstraintValue' "TN"

pattern GeoMatchConstraintValue_TO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TO = GeoMatchConstraintValue' "TO"

pattern GeoMatchConstraintValue_TR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TR = GeoMatchConstraintValue' "TR"

pattern GeoMatchConstraintValue_TT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TT = GeoMatchConstraintValue' "TT"

pattern GeoMatchConstraintValue_TV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TV = GeoMatchConstraintValue' "TV"

pattern GeoMatchConstraintValue_TW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TW = GeoMatchConstraintValue' "TW"

pattern GeoMatchConstraintValue_TZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_TZ = GeoMatchConstraintValue' "TZ"

pattern GeoMatchConstraintValue_UA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_UA = GeoMatchConstraintValue' "UA"

pattern GeoMatchConstraintValue_UG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_UG = GeoMatchConstraintValue' "UG"

pattern GeoMatchConstraintValue_UM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_UM = GeoMatchConstraintValue' "UM"

pattern GeoMatchConstraintValue_US :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_US = GeoMatchConstraintValue' "US"

pattern GeoMatchConstraintValue_UY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_UY = GeoMatchConstraintValue' "UY"

pattern GeoMatchConstraintValue_UZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_UZ = GeoMatchConstraintValue' "UZ"

pattern GeoMatchConstraintValue_VA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_VA = GeoMatchConstraintValue' "VA"

pattern GeoMatchConstraintValue_VC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_VC = GeoMatchConstraintValue' "VC"

pattern GeoMatchConstraintValue_VE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_VE = GeoMatchConstraintValue' "VE"

pattern GeoMatchConstraintValue_VG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_VG = GeoMatchConstraintValue' "VG"

pattern GeoMatchConstraintValue_VI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_VI = GeoMatchConstraintValue' "VI"

pattern GeoMatchConstraintValue_VN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_VN = GeoMatchConstraintValue' "VN"

pattern GeoMatchConstraintValue_VU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_VU = GeoMatchConstraintValue' "VU"

pattern GeoMatchConstraintValue_WF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_WF = GeoMatchConstraintValue' "WF"

pattern GeoMatchConstraintValue_WS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_WS = GeoMatchConstraintValue' "WS"

pattern GeoMatchConstraintValue_YE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_YE = GeoMatchConstraintValue' "YE"

pattern GeoMatchConstraintValue_YT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_YT = GeoMatchConstraintValue' "YT"

pattern GeoMatchConstraintValue_ZA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ZA = GeoMatchConstraintValue' "ZA"

pattern GeoMatchConstraintValue_ZM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ZM = GeoMatchConstraintValue' "ZM"

pattern GeoMatchConstraintValue_ZW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValue_ZW = GeoMatchConstraintValue' "ZW"

{-# COMPLETE
  GeoMatchConstraintValue_AD,
  GeoMatchConstraintValue_AE,
  GeoMatchConstraintValue_AF,
  GeoMatchConstraintValue_AG,
  GeoMatchConstraintValue_AI,
  GeoMatchConstraintValue_AL,
  GeoMatchConstraintValue_AM,
  GeoMatchConstraintValue_AO,
  GeoMatchConstraintValue_AQ,
  GeoMatchConstraintValue_AR,
  GeoMatchConstraintValue_AS,
  GeoMatchConstraintValue_AT,
  GeoMatchConstraintValue_AU,
  GeoMatchConstraintValue_AW,
  GeoMatchConstraintValue_AX,
  GeoMatchConstraintValue_AZ,
  GeoMatchConstraintValue_BA,
  GeoMatchConstraintValue_BB,
  GeoMatchConstraintValue_BD,
  GeoMatchConstraintValue_BE,
  GeoMatchConstraintValue_BF,
  GeoMatchConstraintValue_BG,
  GeoMatchConstraintValue_BH,
  GeoMatchConstraintValue_BI,
  GeoMatchConstraintValue_BJ,
  GeoMatchConstraintValue_BL,
  GeoMatchConstraintValue_BM,
  GeoMatchConstraintValue_BN,
  GeoMatchConstraintValue_BO,
  GeoMatchConstraintValue_BQ,
  GeoMatchConstraintValue_BR,
  GeoMatchConstraintValue_BS,
  GeoMatchConstraintValue_BT,
  GeoMatchConstraintValue_BV,
  GeoMatchConstraintValue_BW,
  GeoMatchConstraintValue_BY,
  GeoMatchConstraintValue_BZ,
  GeoMatchConstraintValue_CA,
  GeoMatchConstraintValue_CC,
  GeoMatchConstraintValue_CD,
  GeoMatchConstraintValue_CF,
  GeoMatchConstraintValue_CG,
  GeoMatchConstraintValue_CH,
  GeoMatchConstraintValue_CI,
  GeoMatchConstraintValue_CK,
  GeoMatchConstraintValue_CL,
  GeoMatchConstraintValue_CM,
  GeoMatchConstraintValue_CN,
  GeoMatchConstraintValue_CO,
  GeoMatchConstraintValue_CR,
  GeoMatchConstraintValue_CU,
  GeoMatchConstraintValue_CV,
  GeoMatchConstraintValue_CW,
  GeoMatchConstraintValue_CX,
  GeoMatchConstraintValue_CY,
  GeoMatchConstraintValue_CZ,
  GeoMatchConstraintValue_DE,
  GeoMatchConstraintValue_DJ,
  GeoMatchConstraintValue_DK,
  GeoMatchConstraintValue_DM,
  GeoMatchConstraintValue_DO,
  GeoMatchConstraintValue_DZ,
  GeoMatchConstraintValue_EC,
  GeoMatchConstraintValue_EE,
  GeoMatchConstraintValue_EG,
  GeoMatchConstraintValue_EH,
  GeoMatchConstraintValue_ER,
  GeoMatchConstraintValue_ES,
  GeoMatchConstraintValue_ET,
  GeoMatchConstraintValue_FI,
  GeoMatchConstraintValue_FJ,
  GeoMatchConstraintValue_FK,
  GeoMatchConstraintValue_FM,
  GeoMatchConstraintValue_FO,
  GeoMatchConstraintValue_FR,
  GeoMatchConstraintValue_GA,
  GeoMatchConstraintValue_GB,
  GeoMatchConstraintValue_GD,
  GeoMatchConstraintValue_GE,
  GeoMatchConstraintValue_GF,
  GeoMatchConstraintValue_GG,
  GeoMatchConstraintValue_GH,
  GeoMatchConstraintValue_GI,
  GeoMatchConstraintValue_GL,
  GeoMatchConstraintValue_GM,
  GeoMatchConstraintValue_GN,
  GeoMatchConstraintValue_GP,
  GeoMatchConstraintValue_GQ,
  GeoMatchConstraintValue_GR,
  GeoMatchConstraintValue_GS,
  GeoMatchConstraintValue_GT,
  GeoMatchConstraintValue_GU,
  GeoMatchConstraintValue_GW,
  GeoMatchConstraintValue_GY,
  GeoMatchConstraintValue_HK,
  GeoMatchConstraintValue_HM,
  GeoMatchConstraintValue_HN,
  GeoMatchConstraintValue_HR,
  GeoMatchConstraintValue_HT,
  GeoMatchConstraintValue_HU,
  GeoMatchConstraintValue_ID,
  GeoMatchConstraintValue_IE,
  GeoMatchConstraintValue_IL,
  GeoMatchConstraintValue_IM,
  GeoMatchConstraintValue_IN,
  GeoMatchConstraintValue_IO,
  GeoMatchConstraintValue_IQ,
  GeoMatchConstraintValue_IR,
  GeoMatchConstraintValue_IS,
  GeoMatchConstraintValue_IT,
  GeoMatchConstraintValue_JE,
  GeoMatchConstraintValue_JM,
  GeoMatchConstraintValue_JO,
  GeoMatchConstraintValue_JP,
  GeoMatchConstraintValue_KE,
  GeoMatchConstraintValue_KG,
  GeoMatchConstraintValue_KH,
  GeoMatchConstraintValue_KI,
  GeoMatchConstraintValue_KM,
  GeoMatchConstraintValue_KN,
  GeoMatchConstraintValue_KP,
  GeoMatchConstraintValue_KR,
  GeoMatchConstraintValue_KW,
  GeoMatchConstraintValue_KY,
  GeoMatchConstraintValue_KZ,
  GeoMatchConstraintValue_LA,
  GeoMatchConstraintValue_LB,
  GeoMatchConstraintValue_LC,
  GeoMatchConstraintValue_LI,
  GeoMatchConstraintValue_LK,
  GeoMatchConstraintValue_LR,
  GeoMatchConstraintValue_LS,
  GeoMatchConstraintValue_LT,
  GeoMatchConstraintValue_LU,
  GeoMatchConstraintValue_LV,
  GeoMatchConstraintValue_LY,
  GeoMatchConstraintValue_MA,
  GeoMatchConstraintValue_MC,
  GeoMatchConstraintValue_MD,
  GeoMatchConstraintValue_ME,
  GeoMatchConstraintValue_MF,
  GeoMatchConstraintValue_MG,
  GeoMatchConstraintValue_MH,
  GeoMatchConstraintValue_MK,
  GeoMatchConstraintValue_ML,
  GeoMatchConstraintValue_MM,
  GeoMatchConstraintValue_MN,
  GeoMatchConstraintValue_MO,
  GeoMatchConstraintValue_MP,
  GeoMatchConstraintValue_MQ,
  GeoMatchConstraintValue_MR,
  GeoMatchConstraintValue_MS,
  GeoMatchConstraintValue_MT,
  GeoMatchConstraintValue_MU,
  GeoMatchConstraintValue_MV,
  GeoMatchConstraintValue_MW,
  GeoMatchConstraintValue_MX,
  GeoMatchConstraintValue_MY,
  GeoMatchConstraintValue_MZ,
  GeoMatchConstraintValue_NA,
  GeoMatchConstraintValue_NC,
  GeoMatchConstraintValue_NE,
  GeoMatchConstraintValue_NF,
  GeoMatchConstraintValue_NG,
  GeoMatchConstraintValue_NI,
  GeoMatchConstraintValue_NL,
  GeoMatchConstraintValue_NO,
  GeoMatchConstraintValue_NP,
  GeoMatchConstraintValue_NR,
  GeoMatchConstraintValue_NU,
  GeoMatchConstraintValue_NZ,
  GeoMatchConstraintValue_OM,
  GeoMatchConstraintValue_PA,
  GeoMatchConstraintValue_PE,
  GeoMatchConstraintValue_PF,
  GeoMatchConstraintValue_PG,
  GeoMatchConstraintValue_PH,
  GeoMatchConstraintValue_PK,
  GeoMatchConstraintValue_PL,
  GeoMatchConstraintValue_PM,
  GeoMatchConstraintValue_PN,
  GeoMatchConstraintValue_PR,
  GeoMatchConstraintValue_PS,
  GeoMatchConstraintValue_PT,
  GeoMatchConstraintValue_PW,
  GeoMatchConstraintValue_PY,
  GeoMatchConstraintValue_QA,
  GeoMatchConstraintValue_RE,
  GeoMatchConstraintValue_RO,
  GeoMatchConstraintValue_RS,
  GeoMatchConstraintValue_RU,
  GeoMatchConstraintValue_RW,
  GeoMatchConstraintValue_SA,
  GeoMatchConstraintValue_SB,
  GeoMatchConstraintValue_SC,
  GeoMatchConstraintValue_SD,
  GeoMatchConstraintValue_SE,
  GeoMatchConstraintValue_SG,
  GeoMatchConstraintValue_SH,
  GeoMatchConstraintValue_SI,
  GeoMatchConstraintValue_SJ,
  GeoMatchConstraintValue_SK,
  GeoMatchConstraintValue_SL,
  GeoMatchConstraintValue_SM,
  GeoMatchConstraintValue_SN,
  GeoMatchConstraintValue_SO,
  GeoMatchConstraintValue_SR,
  GeoMatchConstraintValue_SS,
  GeoMatchConstraintValue_ST,
  GeoMatchConstraintValue_SV,
  GeoMatchConstraintValue_SX,
  GeoMatchConstraintValue_SY,
  GeoMatchConstraintValue_SZ,
  GeoMatchConstraintValue_TC,
  GeoMatchConstraintValue_TD,
  GeoMatchConstraintValue_TF,
  GeoMatchConstraintValue_TG,
  GeoMatchConstraintValue_TH,
  GeoMatchConstraintValue_TJ,
  GeoMatchConstraintValue_TK,
  GeoMatchConstraintValue_TL,
  GeoMatchConstraintValue_TM,
  GeoMatchConstraintValue_TN,
  GeoMatchConstraintValue_TO,
  GeoMatchConstraintValue_TR,
  GeoMatchConstraintValue_TT,
  GeoMatchConstraintValue_TV,
  GeoMatchConstraintValue_TW,
  GeoMatchConstraintValue_TZ,
  GeoMatchConstraintValue_UA,
  GeoMatchConstraintValue_UG,
  GeoMatchConstraintValue_UM,
  GeoMatchConstraintValue_US,
  GeoMatchConstraintValue_UY,
  GeoMatchConstraintValue_UZ,
  GeoMatchConstraintValue_VA,
  GeoMatchConstraintValue_VC,
  GeoMatchConstraintValue_VE,
  GeoMatchConstraintValue_VG,
  GeoMatchConstraintValue_VI,
  GeoMatchConstraintValue_VN,
  GeoMatchConstraintValue_VU,
  GeoMatchConstraintValue_WF,
  GeoMatchConstraintValue_WS,
  GeoMatchConstraintValue_YE,
  GeoMatchConstraintValue_YT,
  GeoMatchConstraintValue_ZA,
  GeoMatchConstraintValue_ZM,
  GeoMatchConstraintValue_ZW,
  GeoMatchConstraintValue'
  #-}

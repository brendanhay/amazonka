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
-- Module      : Amazonka.MediaConvert.Types.LanguageCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.LanguageCode
  ( LanguageCode
      ( ..,
        LanguageCode_AAR,
        LanguageCode_ABK,
        LanguageCode_AFR,
        LanguageCode_AKA,
        LanguageCode_AMH,
        LanguageCode_ARA,
        LanguageCode_ARG,
        LanguageCode_ASM,
        LanguageCode_AVA,
        LanguageCode_AVE,
        LanguageCode_AYM,
        LanguageCode_AZE,
        LanguageCode_BAK,
        LanguageCode_BAM,
        LanguageCode_BEL,
        LanguageCode_BEN,
        LanguageCode_BIH,
        LanguageCode_BIS,
        LanguageCode_BOD,
        LanguageCode_BOS,
        LanguageCode_BRE,
        LanguageCode_BUL,
        LanguageCode_CAT,
        LanguageCode_CES,
        LanguageCode_CHA,
        LanguageCode_CHE,
        LanguageCode_CHU,
        LanguageCode_CHV,
        LanguageCode_COR,
        LanguageCode_COS,
        LanguageCode_CRE,
        LanguageCode_CYM,
        LanguageCode_DAN,
        LanguageCode_DEU,
        LanguageCode_DIV,
        LanguageCode_DZO,
        LanguageCode_ELL,
        LanguageCode_ENG,
        LanguageCode_ENM,
        LanguageCode_EPO,
        LanguageCode_EST,
        LanguageCode_EUS,
        LanguageCode_EWE,
        LanguageCode_FAO,
        LanguageCode_FAS,
        LanguageCode_FIJ,
        LanguageCode_FIN,
        LanguageCode_FRA,
        LanguageCode_FRM,
        LanguageCode_FRY,
        LanguageCode_FUL,
        LanguageCode_GER,
        LanguageCode_GLA,
        LanguageCode_GLE,
        LanguageCode_GLG,
        LanguageCode_GLV,
        LanguageCode_GRN,
        LanguageCode_GUJ,
        LanguageCode_HAT,
        LanguageCode_HAU,
        LanguageCode_HEB,
        LanguageCode_HER,
        LanguageCode_HIN,
        LanguageCode_HMO,
        LanguageCode_HRV,
        LanguageCode_HUN,
        LanguageCode_HYE,
        LanguageCode_IBO,
        LanguageCode_IDO,
        LanguageCode_III,
        LanguageCode_IKU,
        LanguageCode_ILE,
        LanguageCode_INA,
        LanguageCode_IND,
        LanguageCode_IPK,
        LanguageCode_ISL,
        LanguageCode_ITA,
        LanguageCode_JAV,
        LanguageCode_JPN,
        LanguageCode_KAL,
        LanguageCode_KAN,
        LanguageCode_KAS,
        LanguageCode_KAT,
        LanguageCode_KAU,
        LanguageCode_KAZ,
        LanguageCode_KHM,
        LanguageCode_KIK,
        LanguageCode_KIN,
        LanguageCode_KIR,
        LanguageCode_KOM,
        LanguageCode_KON,
        LanguageCode_KOR,
        LanguageCode_KUA,
        LanguageCode_KUR,
        LanguageCode_LAO,
        LanguageCode_LAT,
        LanguageCode_LAV,
        LanguageCode_LIM,
        LanguageCode_LIN,
        LanguageCode_LIT,
        LanguageCode_LTZ,
        LanguageCode_LUB,
        LanguageCode_LUG,
        LanguageCode_MAH,
        LanguageCode_MAL,
        LanguageCode_MAR,
        LanguageCode_MKD,
        LanguageCode_MLG,
        LanguageCode_MLT,
        LanguageCode_MON,
        LanguageCode_MRI,
        LanguageCode_MSA,
        LanguageCode_MYA,
        LanguageCode_NAU,
        LanguageCode_NAV,
        LanguageCode_NBL,
        LanguageCode_NDE,
        LanguageCode_NDO,
        LanguageCode_NEP,
        LanguageCode_NLD,
        LanguageCode_NNO,
        LanguageCode_NOB,
        LanguageCode_NOR,
        LanguageCode_NYA,
        LanguageCode_OCI,
        LanguageCode_OJI,
        LanguageCode_ORI,
        LanguageCode_ORJ,
        LanguageCode_ORM,
        LanguageCode_OSS,
        LanguageCode_PAN,
        LanguageCode_PLI,
        LanguageCode_POL,
        LanguageCode_POR,
        LanguageCode_PUS,
        LanguageCode_QAA,
        LanguageCode_QPC,
        LanguageCode_QUE,
        LanguageCode_ROH,
        LanguageCode_RON,
        LanguageCode_RUN,
        LanguageCode_RUS,
        LanguageCode_SAG,
        LanguageCode_SAN,
        LanguageCode_SIN,
        LanguageCode_SLK,
        LanguageCode_SLV,
        LanguageCode_SME,
        LanguageCode_SMO,
        LanguageCode_SNA,
        LanguageCode_SND,
        LanguageCode_SOM,
        LanguageCode_SOT,
        LanguageCode_SPA,
        LanguageCode_SQI,
        LanguageCode_SRB,
        LanguageCode_SRD,
        LanguageCode_SRP,
        LanguageCode_SSW,
        LanguageCode_SUN,
        LanguageCode_SWA,
        LanguageCode_SWE,
        LanguageCode_TAH,
        LanguageCode_TAM,
        LanguageCode_TAT,
        LanguageCode_TEL,
        LanguageCode_TGK,
        LanguageCode_TGL,
        LanguageCode_THA,
        LanguageCode_TIR,
        LanguageCode_TNG,
        LanguageCode_TON,
        LanguageCode_TSN,
        LanguageCode_TSO,
        LanguageCode_TUK,
        LanguageCode_TUR,
        LanguageCode_TWI,
        LanguageCode_UIG,
        LanguageCode_UKR,
        LanguageCode_URD,
        LanguageCode_UZB,
        LanguageCode_VEN,
        LanguageCode_VIE,
        LanguageCode_VOL,
        LanguageCode_WLN,
        LanguageCode_WOL,
        LanguageCode_XHO,
        LanguageCode_YID,
        LanguageCode_YOR,
        LanguageCode_ZHA,
        LanguageCode_ZHO,
        LanguageCode_ZUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the language, using the ISO 639-2 three-letter code listed at
-- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
newtype LanguageCode = LanguageCode'
  { fromLanguageCode ::
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

pattern LanguageCode_AAR :: LanguageCode
pattern LanguageCode_AAR = LanguageCode' "AAR"

pattern LanguageCode_ABK :: LanguageCode
pattern LanguageCode_ABK = LanguageCode' "ABK"

pattern LanguageCode_AFR :: LanguageCode
pattern LanguageCode_AFR = LanguageCode' "AFR"

pattern LanguageCode_AKA :: LanguageCode
pattern LanguageCode_AKA = LanguageCode' "AKA"

pattern LanguageCode_AMH :: LanguageCode
pattern LanguageCode_AMH = LanguageCode' "AMH"

pattern LanguageCode_ARA :: LanguageCode
pattern LanguageCode_ARA = LanguageCode' "ARA"

pattern LanguageCode_ARG :: LanguageCode
pattern LanguageCode_ARG = LanguageCode' "ARG"

pattern LanguageCode_ASM :: LanguageCode
pattern LanguageCode_ASM = LanguageCode' "ASM"

pattern LanguageCode_AVA :: LanguageCode
pattern LanguageCode_AVA = LanguageCode' "AVA"

pattern LanguageCode_AVE :: LanguageCode
pattern LanguageCode_AVE = LanguageCode' "AVE"

pattern LanguageCode_AYM :: LanguageCode
pattern LanguageCode_AYM = LanguageCode' "AYM"

pattern LanguageCode_AZE :: LanguageCode
pattern LanguageCode_AZE = LanguageCode' "AZE"

pattern LanguageCode_BAK :: LanguageCode
pattern LanguageCode_BAK = LanguageCode' "BAK"

pattern LanguageCode_BAM :: LanguageCode
pattern LanguageCode_BAM = LanguageCode' "BAM"

pattern LanguageCode_BEL :: LanguageCode
pattern LanguageCode_BEL = LanguageCode' "BEL"

pattern LanguageCode_BEN :: LanguageCode
pattern LanguageCode_BEN = LanguageCode' "BEN"

pattern LanguageCode_BIH :: LanguageCode
pattern LanguageCode_BIH = LanguageCode' "BIH"

pattern LanguageCode_BIS :: LanguageCode
pattern LanguageCode_BIS = LanguageCode' "BIS"

pattern LanguageCode_BOD :: LanguageCode
pattern LanguageCode_BOD = LanguageCode' "BOD"

pattern LanguageCode_BOS :: LanguageCode
pattern LanguageCode_BOS = LanguageCode' "BOS"

pattern LanguageCode_BRE :: LanguageCode
pattern LanguageCode_BRE = LanguageCode' "BRE"

pattern LanguageCode_BUL :: LanguageCode
pattern LanguageCode_BUL = LanguageCode' "BUL"

pattern LanguageCode_CAT :: LanguageCode
pattern LanguageCode_CAT = LanguageCode' "CAT"

pattern LanguageCode_CES :: LanguageCode
pattern LanguageCode_CES = LanguageCode' "CES"

pattern LanguageCode_CHA :: LanguageCode
pattern LanguageCode_CHA = LanguageCode' "CHA"

pattern LanguageCode_CHE :: LanguageCode
pattern LanguageCode_CHE = LanguageCode' "CHE"

pattern LanguageCode_CHU :: LanguageCode
pattern LanguageCode_CHU = LanguageCode' "CHU"

pattern LanguageCode_CHV :: LanguageCode
pattern LanguageCode_CHV = LanguageCode' "CHV"

pattern LanguageCode_COR :: LanguageCode
pattern LanguageCode_COR = LanguageCode' "COR"

pattern LanguageCode_COS :: LanguageCode
pattern LanguageCode_COS = LanguageCode' "COS"

pattern LanguageCode_CRE :: LanguageCode
pattern LanguageCode_CRE = LanguageCode' "CRE"

pattern LanguageCode_CYM :: LanguageCode
pattern LanguageCode_CYM = LanguageCode' "CYM"

pattern LanguageCode_DAN :: LanguageCode
pattern LanguageCode_DAN = LanguageCode' "DAN"

pattern LanguageCode_DEU :: LanguageCode
pattern LanguageCode_DEU = LanguageCode' "DEU"

pattern LanguageCode_DIV :: LanguageCode
pattern LanguageCode_DIV = LanguageCode' "DIV"

pattern LanguageCode_DZO :: LanguageCode
pattern LanguageCode_DZO = LanguageCode' "DZO"

pattern LanguageCode_ELL :: LanguageCode
pattern LanguageCode_ELL = LanguageCode' "ELL"

pattern LanguageCode_ENG :: LanguageCode
pattern LanguageCode_ENG = LanguageCode' "ENG"

pattern LanguageCode_ENM :: LanguageCode
pattern LanguageCode_ENM = LanguageCode' "ENM"

pattern LanguageCode_EPO :: LanguageCode
pattern LanguageCode_EPO = LanguageCode' "EPO"

pattern LanguageCode_EST :: LanguageCode
pattern LanguageCode_EST = LanguageCode' "EST"

pattern LanguageCode_EUS :: LanguageCode
pattern LanguageCode_EUS = LanguageCode' "EUS"

pattern LanguageCode_EWE :: LanguageCode
pattern LanguageCode_EWE = LanguageCode' "EWE"

pattern LanguageCode_FAO :: LanguageCode
pattern LanguageCode_FAO = LanguageCode' "FAO"

pattern LanguageCode_FAS :: LanguageCode
pattern LanguageCode_FAS = LanguageCode' "FAS"

pattern LanguageCode_FIJ :: LanguageCode
pattern LanguageCode_FIJ = LanguageCode' "FIJ"

pattern LanguageCode_FIN :: LanguageCode
pattern LanguageCode_FIN = LanguageCode' "FIN"

pattern LanguageCode_FRA :: LanguageCode
pattern LanguageCode_FRA = LanguageCode' "FRA"

pattern LanguageCode_FRM :: LanguageCode
pattern LanguageCode_FRM = LanguageCode' "FRM"

pattern LanguageCode_FRY :: LanguageCode
pattern LanguageCode_FRY = LanguageCode' "FRY"

pattern LanguageCode_FUL :: LanguageCode
pattern LanguageCode_FUL = LanguageCode' "FUL"

pattern LanguageCode_GER :: LanguageCode
pattern LanguageCode_GER = LanguageCode' "GER"

pattern LanguageCode_GLA :: LanguageCode
pattern LanguageCode_GLA = LanguageCode' "GLA"

pattern LanguageCode_GLE :: LanguageCode
pattern LanguageCode_GLE = LanguageCode' "GLE"

pattern LanguageCode_GLG :: LanguageCode
pattern LanguageCode_GLG = LanguageCode' "GLG"

pattern LanguageCode_GLV :: LanguageCode
pattern LanguageCode_GLV = LanguageCode' "GLV"

pattern LanguageCode_GRN :: LanguageCode
pattern LanguageCode_GRN = LanguageCode' "GRN"

pattern LanguageCode_GUJ :: LanguageCode
pattern LanguageCode_GUJ = LanguageCode' "GUJ"

pattern LanguageCode_HAT :: LanguageCode
pattern LanguageCode_HAT = LanguageCode' "HAT"

pattern LanguageCode_HAU :: LanguageCode
pattern LanguageCode_HAU = LanguageCode' "HAU"

pattern LanguageCode_HEB :: LanguageCode
pattern LanguageCode_HEB = LanguageCode' "HEB"

pattern LanguageCode_HER :: LanguageCode
pattern LanguageCode_HER = LanguageCode' "HER"

pattern LanguageCode_HIN :: LanguageCode
pattern LanguageCode_HIN = LanguageCode' "HIN"

pattern LanguageCode_HMO :: LanguageCode
pattern LanguageCode_HMO = LanguageCode' "HMO"

pattern LanguageCode_HRV :: LanguageCode
pattern LanguageCode_HRV = LanguageCode' "HRV"

pattern LanguageCode_HUN :: LanguageCode
pattern LanguageCode_HUN = LanguageCode' "HUN"

pattern LanguageCode_HYE :: LanguageCode
pattern LanguageCode_HYE = LanguageCode' "HYE"

pattern LanguageCode_IBO :: LanguageCode
pattern LanguageCode_IBO = LanguageCode' "IBO"

pattern LanguageCode_IDO :: LanguageCode
pattern LanguageCode_IDO = LanguageCode' "IDO"

pattern LanguageCode_III :: LanguageCode
pattern LanguageCode_III = LanguageCode' "III"

pattern LanguageCode_IKU :: LanguageCode
pattern LanguageCode_IKU = LanguageCode' "IKU"

pattern LanguageCode_ILE :: LanguageCode
pattern LanguageCode_ILE = LanguageCode' "ILE"

pattern LanguageCode_INA :: LanguageCode
pattern LanguageCode_INA = LanguageCode' "INA"

pattern LanguageCode_IND :: LanguageCode
pattern LanguageCode_IND = LanguageCode' "IND"

pattern LanguageCode_IPK :: LanguageCode
pattern LanguageCode_IPK = LanguageCode' "IPK"

pattern LanguageCode_ISL :: LanguageCode
pattern LanguageCode_ISL = LanguageCode' "ISL"

pattern LanguageCode_ITA :: LanguageCode
pattern LanguageCode_ITA = LanguageCode' "ITA"

pattern LanguageCode_JAV :: LanguageCode
pattern LanguageCode_JAV = LanguageCode' "JAV"

pattern LanguageCode_JPN :: LanguageCode
pattern LanguageCode_JPN = LanguageCode' "JPN"

pattern LanguageCode_KAL :: LanguageCode
pattern LanguageCode_KAL = LanguageCode' "KAL"

pattern LanguageCode_KAN :: LanguageCode
pattern LanguageCode_KAN = LanguageCode' "KAN"

pattern LanguageCode_KAS :: LanguageCode
pattern LanguageCode_KAS = LanguageCode' "KAS"

pattern LanguageCode_KAT :: LanguageCode
pattern LanguageCode_KAT = LanguageCode' "KAT"

pattern LanguageCode_KAU :: LanguageCode
pattern LanguageCode_KAU = LanguageCode' "KAU"

pattern LanguageCode_KAZ :: LanguageCode
pattern LanguageCode_KAZ = LanguageCode' "KAZ"

pattern LanguageCode_KHM :: LanguageCode
pattern LanguageCode_KHM = LanguageCode' "KHM"

pattern LanguageCode_KIK :: LanguageCode
pattern LanguageCode_KIK = LanguageCode' "KIK"

pattern LanguageCode_KIN :: LanguageCode
pattern LanguageCode_KIN = LanguageCode' "KIN"

pattern LanguageCode_KIR :: LanguageCode
pattern LanguageCode_KIR = LanguageCode' "KIR"

pattern LanguageCode_KOM :: LanguageCode
pattern LanguageCode_KOM = LanguageCode' "KOM"

pattern LanguageCode_KON :: LanguageCode
pattern LanguageCode_KON = LanguageCode' "KON"

pattern LanguageCode_KOR :: LanguageCode
pattern LanguageCode_KOR = LanguageCode' "KOR"

pattern LanguageCode_KUA :: LanguageCode
pattern LanguageCode_KUA = LanguageCode' "KUA"

pattern LanguageCode_KUR :: LanguageCode
pattern LanguageCode_KUR = LanguageCode' "KUR"

pattern LanguageCode_LAO :: LanguageCode
pattern LanguageCode_LAO = LanguageCode' "LAO"

pattern LanguageCode_LAT :: LanguageCode
pattern LanguageCode_LAT = LanguageCode' "LAT"

pattern LanguageCode_LAV :: LanguageCode
pattern LanguageCode_LAV = LanguageCode' "LAV"

pattern LanguageCode_LIM :: LanguageCode
pattern LanguageCode_LIM = LanguageCode' "LIM"

pattern LanguageCode_LIN :: LanguageCode
pattern LanguageCode_LIN = LanguageCode' "LIN"

pattern LanguageCode_LIT :: LanguageCode
pattern LanguageCode_LIT = LanguageCode' "LIT"

pattern LanguageCode_LTZ :: LanguageCode
pattern LanguageCode_LTZ = LanguageCode' "LTZ"

pattern LanguageCode_LUB :: LanguageCode
pattern LanguageCode_LUB = LanguageCode' "LUB"

pattern LanguageCode_LUG :: LanguageCode
pattern LanguageCode_LUG = LanguageCode' "LUG"

pattern LanguageCode_MAH :: LanguageCode
pattern LanguageCode_MAH = LanguageCode' "MAH"

pattern LanguageCode_MAL :: LanguageCode
pattern LanguageCode_MAL = LanguageCode' "MAL"

pattern LanguageCode_MAR :: LanguageCode
pattern LanguageCode_MAR = LanguageCode' "MAR"

pattern LanguageCode_MKD :: LanguageCode
pattern LanguageCode_MKD = LanguageCode' "MKD"

pattern LanguageCode_MLG :: LanguageCode
pattern LanguageCode_MLG = LanguageCode' "MLG"

pattern LanguageCode_MLT :: LanguageCode
pattern LanguageCode_MLT = LanguageCode' "MLT"

pattern LanguageCode_MON :: LanguageCode
pattern LanguageCode_MON = LanguageCode' "MON"

pattern LanguageCode_MRI :: LanguageCode
pattern LanguageCode_MRI = LanguageCode' "MRI"

pattern LanguageCode_MSA :: LanguageCode
pattern LanguageCode_MSA = LanguageCode' "MSA"

pattern LanguageCode_MYA :: LanguageCode
pattern LanguageCode_MYA = LanguageCode' "MYA"

pattern LanguageCode_NAU :: LanguageCode
pattern LanguageCode_NAU = LanguageCode' "NAU"

pattern LanguageCode_NAV :: LanguageCode
pattern LanguageCode_NAV = LanguageCode' "NAV"

pattern LanguageCode_NBL :: LanguageCode
pattern LanguageCode_NBL = LanguageCode' "NBL"

pattern LanguageCode_NDE :: LanguageCode
pattern LanguageCode_NDE = LanguageCode' "NDE"

pattern LanguageCode_NDO :: LanguageCode
pattern LanguageCode_NDO = LanguageCode' "NDO"

pattern LanguageCode_NEP :: LanguageCode
pattern LanguageCode_NEP = LanguageCode' "NEP"

pattern LanguageCode_NLD :: LanguageCode
pattern LanguageCode_NLD = LanguageCode' "NLD"

pattern LanguageCode_NNO :: LanguageCode
pattern LanguageCode_NNO = LanguageCode' "NNO"

pattern LanguageCode_NOB :: LanguageCode
pattern LanguageCode_NOB = LanguageCode' "NOB"

pattern LanguageCode_NOR :: LanguageCode
pattern LanguageCode_NOR = LanguageCode' "NOR"

pattern LanguageCode_NYA :: LanguageCode
pattern LanguageCode_NYA = LanguageCode' "NYA"

pattern LanguageCode_OCI :: LanguageCode
pattern LanguageCode_OCI = LanguageCode' "OCI"

pattern LanguageCode_OJI :: LanguageCode
pattern LanguageCode_OJI = LanguageCode' "OJI"

pattern LanguageCode_ORI :: LanguageCode
pattern LanguageCode_ORI = LanguageCode' "ORI"

pattern LanguageCode_ORJ :: LanguageCode
pattern LanguageCode_ORJ = LanguageCode' "ORJ"

pattern LanguageCode_ORM :: LanguageCode
pattern LanguageCode_ORM = LanguageCode' "ORM"

pattern LanguageCode_OSS :: LanguageCode
pattern LanguageCode_OSS = LanguageCode' "OSS"

pattern LanguageCode_PAN :: LanguageCode
pattern LanguageCode_PAN = LanguageCode' "PAN"

pattern LanguageCode_PLI :: LanguageCode
pattern LanguageCode_PLI = LanguageCode' "PLI"

pattern LanguageCode_POL :: LanguageCode
pattern LanguageCode_POL = LanguageCode' "POL"

pattern LanguageCode_POR :: LanguageCode
pattern LanguageCode_POR = LanguageCode' "POR"

pattern LanguageCode_PUS :: LanguageCode
pattern LanguageCode_PUS = LanguageCode' "PUS"

pattern LanguageCode_QAA :: LanguageCode
pattern LanguageCode_QAA = LanguageCode' "QAA"

pattern LanguageCode_QPC :: LanguageCode
pattern LanguageCode_QPC = LanguageCode' "QPC"

pattern LanguageCode_QUE :: LanguageCode
pattern LanguageCode_QUE = LanguageCode' "QUE"

pattern LanguageCode_ROH :: LanguageCode
pattern LanguageCode_ROH = LanguageCode' "ROH"

pattern LanguageCode_RON :: LanguageCode
pattern LanguageCode_RON = LanguageCode' "RON"

pattern LanguageCode_RUN :: LanguageCode
pattern LanguageCode_RUN = LanguageCode' "RUN"

pattern LanguageCode_RUS :: LanguageCode
pattern LanguageCode_RUS = LanguageCode' "RUS"

pattern LanguageCode_SAG :: LanguageCode
pattern LanguageCode_SAG = LanguageCode' "SAG"

pattern LanguageCode_SAN :: LanguageCode
pattern LanguageCode_SAN = LanguageCode' "SAN"

pattern LanguageCode_SIN :: LanguageCode
pattern LanguageCode_SIN = LanguageCode' "SIN"

pattern LanguageCode_SLK :: LanguageCode
pattern LanguageCode_SLK = LanguageCode' "SLK"

pattern LanguageCode_SLV :: LanguageCode
pattern LanguageCode_SLV = LanguageCode' "SLV"

pattern LanguageCode_SME :: LanguageCode
pattern LanguageCode_SME = LanguageCode' "SME"

pattern LanguageCode_SMO :: LanguageCode
pattern LanguageCode_SMO = LanguageCode' "SMO"

pattern LanguageCode_SNA :: LanguageCode
pattern LanguageCode_SNA = LanguageCode' "SNA"

pattern LanguageCode_SND :: LanguageCode
pattern LanguageCode_SND = LanguageCode' "SND"

pattern LanguageCode_SOM :: LanguageCode
pattern LanguageCode_SOM = LanguageCode' "SOM"

pattern LanguageCode_SOT :: LanguageCode
pattern LanguageCode_SOT = LanguageCode' "SOT"

pattern LanguageCode_SPA :: LanguageCode
pattern LanguageCode_SPA = LanguageCode' "SPA"

pattern LanguageCode_SQI :: LanguageCode
pattern LanguageCode_SQI = LanguageCode' "SQI"

pattern LanguageCode_SRB :: LanguageCode
pattern LanguageCode_SRB = LanguageCode' "SRB"

pattern LanguageCode_SRD :: LanguageCode
pattern LanguageCode_SRD = LanguageCode' "SRD"

pattern LanguageCode_SRP :: LanguageCode
pattern LanguageCode_SRP = LanguageCode' "SRP"

pattern LanguageCode_SSW :: LanguageCode
pattern LanguageCode_SSW = LanguageCode' "SSW"

pattern LanguageCode_SUN :: LanguageCode
pattern LanguageCode_SUN = LanguageCode' "SUN"

pattern LanguageCode_SWA :: LanguageCode
pattern LanguageCode_SWA = LanguageCode' "SWA"

pattern LanguageCode_SWE :: LanguageCode
pattern LanguageCode_SWE = LanguageCode' "SWE"

pattern LanguageCode_TAH :: LanguageCode
pattern LanguageCode_TAH = LanguageCode' "TAH"

pattern LanguageCode_TAM :: LanguageCode
pattern LanguageCode_TAM = LanguageCode' "TAM"

pattern LanguageCode_TAT :: LanguageCode
pattern LanguageCode_TAT = LanguageCode' "TAT"

pattern LanguageCode_TEL :: LanguageCode
pattern LanguageCode_TEL = LanguageCode' "TEL"

pattern LanguageCode_TGK :: LanguageCode
pattern LanguageCode_TGK = LanguageCode' "TGK"

pattern LanguageCode_TGL :: LanguageCode
pattern LanguageCode_TGL = LanguageCode' "TGL"

pattern LanguageCode_THA :: LanguageCode
pattern LanguageCode_THA = LanguageCode' "THA"

pattern LanguageCode_TIR :: LanguageCode
pattern LanguageCode_TIR = LanguageCode' "TIR"

pattern LanguageCode_TNG :: LanguageCode
pattern LanguageCode_TNG = LanguageCode' "TNG"

pattern LanguageCode_TON :: LanguageCode
pattern LanguageCode_TON = LanguageCode' "TON"

pattern LanguageCode_TSN :: LanguageCode
pattern LanguageCode_TSN = LanguageCode' "TSN"

pattern LanguageCode_TSO :: LanguageCode
pattern LanguageCode_TSO = LanguageCode' "TSO"

pattern LanguageCode_TUK :: LanguageCode
pattern LanguageCode_TUK = LanguageCode' "TUK"

pattern LanguageCode_TUR :: LanguageCode
pattern LanguageCode_TUR = LanguageCode' "TUR"

pattern LanguageCode_TWI :: LanguageCode
pattern LanguageCode_TWI = LanguageCode' "TWI"

pattern LanguageCode_UIG :: LanguageCode
pattern LanguageCode_UIG = LanguageCode' "UIG"

pattern LanguageCode_UKR :: LanguageCode
pattern LanguageCode_UKR = LanguageCode' "UKR"

pattern LanguageCode_URD :: LanguageCode
pattern LanguageCode_URD = LanguageCode' "URD"

pattern LanguageCode_UZB :: LanguageCode
pattern LanguageCode_UZB = LanguageCode' "UZB"

pattern LanguageCode_VEN :: LanguageCode
pattern LanguageCode_VEN = LanguageCode' "VEN"

pattern LanguageCode_VIE :: LanguageCode
pattern LanguageCode_VIE = LanguageCode' "VIE"

pattern LanguageCode_VOL :: LanguageCode
pattern LanguageCode_VOL = LanguageCode' "VOL"

pattern LanguageCode_WLN :: LanguageCode
pattern LanguageCode_WLN = LanguageCode' "WLN"

pattern LanguageCode_WOL :: LanguageCode
pattern LanguageCode_WOL = LanguageCode' "WOL"

pattern LanguageCode_XHO :: LanguageCode
pattern LanguageCode_XHO = LanguageCode' "XHO"

pattern LanguageCode_YID :: LanguageCode
pattern LanguageCode_YID = LanguageCode' "YID"

pattern LanguageCode_YOR :: LanguageCode
pattern LanguageCode_YOR = LanguageCode' "YOR"

pattern LanguageCode_ZHA :: LanguageCode
pattern LanguageCode_ZHA = LanguageCode' "ZHA"

pattern LanguageCode_ZHO :: LanguageCode
pattern LanguageCode_ZHO = LanguageCode' "ZHO"

pattern LanguageCode_ZUL :: LanguageCode
pattern LanguageCode_ZUL = LanguageCode' "ZUL"

{-# COMPLETE
  LanguageCode_AAR,
  LanguageCode_ABK,
  LanguageCode_AFR,
  LanguageCode_AKA,
  LanguageCode_AMH,
  LanguageCode_ARA,
  LanguageCode_ARG,
  LanguageCode_ASM,
  LanguageCode_AVA,
  LanguageCode_AVE,
  LanguageCode_AYM,
  LanguageCode_AZE,
  LanguageCode_BAK,
  LanguageCode_BAM,
  LanguageCode_BEL,
  LanguageCode_BEN,
  LanguageCode_BIH,
  LanguageCode_BIS,
  LanguageCode_BOD,
  LanguageCode_BOS,
  LanguageCode_BRE,
  LanguageCode_BUL,
  LanguageCode_CAT,
  LanguageCode_CES,
  LanguageCode_CHA,
  LanguageCode_CHE,
  LanguageCode_CHU,
  LanguageCode_CHV,
  LanguageCode_COR,
  LanguageCode_COS,
  LanguageCode_CRE,
  LanguageCode_CYM,
  LanguageCode_DAN,
  LanguageCode_DEU,
  LanguageCode_DIV,
  LanguageCode_DZO,
  LanguageCode_ELL,
  LanguageCode_ENG,
  LanguageCode_ENM,
  LanguageCode_EPO,
  LanguageCode_EST,
  LanguageCode_EUS,
  LanguageCode_EWE,
  LanguageCode_FAO,
  LanguageCode_FAS,
  LanguageCode_FIJ,
  LanguageCode_FIN,
  LanguageCode_FRA,
  LanguageCode_FRM,
  LanguageCode_FRY,
  LanguageCode_FUL,
  LanguageCode_GER,
  LanguageCode_GLA,
  LanguageCode_GLE,
  LanguageCode_GLG,
  LanguageCode_GLV,
  LanguageCode_GRN,
  LanguageCode_GUJ,
  LanguageCode_HAT,
  LanguageCode_HAU,
  LanguageCode_HEB,
  LanguageCode_HER,
  LanguageCode_HIN,
  LanguageCode_HMO,
  LanguageCode_HRV,
  LanguageCode_HUN,
  LanguageCode_HYE,
  LanguageCode_IBO,
  LanguageCode_IDO,
  LanguageCode_III,
  LanguageCode_IKU,
  LanguageCode_ILE,
  LanguageCode_INA,
  LanguageCode_IND,
  LanguageCode_IPK,
  LanguageCode_ISL,
  LanguageCode_ITA,
  LanguageCode_JAV,
  LanguageCode_JPN,
  LanguageCode_KAL,
  LanguageCode_KAN,
  LanguageCode_KAS,
  LanguageCode_KAT,
  LanguageCode_KAU,
  LanguageCode_KAZ,
  LanguageCode_KHM,
  LanguageCode_KIK,
  LanguageCode_KIN,
  LanguageCode_KIR,
  LanguageCode_KOM,
  LanguageCode_KON,
  LanguageCode_KOR,
  LanguageCode_KUA,
  LanguageCode_KUR,
  LanguageCode_LAO,
  LanguageCode_LAT,
  LanguageCode_LAV,
  LanguageCode_LIM,
  LanguageCode_LIN,
  LanguageCode_LIT,
  LanguageCode_LTZ,
  LanguageCode_LUB,
  LanguageCode_LUG,
  LanguageCode_MAH,
  LanguageCode_MAL,
  LanguageCode_MAR,
  LanguageCode_MKD,
  LanguageCode_MLG,
  LanguageCode_MLT,
  LanguageCode_MON,
  LanguageCode_MRI,
  LanguageCode_MSA,
  LanguageCode_MYA,
  LanguageCode_NAU,
  LanguageCode_NAV,
  LanguageCode_NBL,
  LanguageCode_NDE,
  LanguageCode_NDO,
  LanguageCode_NEP,
  LanguageCode_NLD,
  LanguageCode_NNO,
  LanguageCode_NOB,
  LanguageCode_NOR,
  LanguageCode_NYA,
  LanguageCode_OCI,
  LanguageCode_OJI,
  LanguageCode_ORI,
  LanguageCode_ORJ,
  LanguageCode_ORM,
  LanguageCode_OSS,
  LanguageCode_PAN,
  LanguageCode_PLI,
  LanguageCode_POL,
  LanguageCode_POR,
  LanguageCode_PUS,
  LanguageCode_QAA,
  LanguageCode_QPC,
  LanguageCode_QUE,
  LanguageCode_ROH,
  LanguageCode_RON,
  LanguageCode_RUN,
  LanguageCode_RUS,
  LanguageCode_SAG,
  LanguageCode_SAN,
  LanguageCode_SIN,
  LanguageCode_SLK,
  LanguageCode_SLV,
  LanguageCode_SME,
  LanguageCode_SMO,
  LanguageCode_SNA,
  LanguageCode_SND,
  LanguageCode_SOM,
  LanguageCode_SOT,
  LanguageCode_SPA,
  LanguageCode_SQI,
  LanguageCode_SRB,
  LanguageCode_SRD,
  LanguageCode_SRP,
  LanguageCode_SSW,
  LanguageCode_SUN,
  LanguageCode_SWA,
  LanguageCode_SWE,
  LanguageCode_TAH,
  LanguageCode_TAM,
  LanguageCode_TAT,
  LanguageCode_TEL,
  LanguageCode_TGK,
  LanguageCode_TGL,
  LanguageCode_THA,
  LanguageCode_TIR,
  LanguageCode_TNG,
  LanguageCode_TON,
  LanguageCode_TSN,
  LanguageCode_TSO,
  LanguageCode_TUK,
  LanguageCode_TUR,
  LanguageCode_TWI,
  LanguageCode_UIG,
  LanguageCode_UKR,
  LanguageCode_URD,
  LanguageCode_UZB,
  LanguageCode_VEN,
  LanguageCode_VIE,
  LanguageCode_VOL,
  LanguageCode_WLN,
  LanguageCode_WOL,
  LanguageCode_XHO,
  LanguageCode_YID,
  LanguageCode_YOR,
  LanguageCode_ZHA,
  LanguageCode_ZHO,
  LanguageCode_ZUL,
  LanguageCode'
  #-}

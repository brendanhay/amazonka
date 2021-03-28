{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProductionVariantInstanceType
  ( ProductionVariantInstanceType
    ( ProductionVariantInstanceType'
    , ProductionVariantInstanceTypeMl_T2_Medium
    , ProductionVariantInstanceTypeMl_T2_Large
    , ProductionVariantInstanceTypeMl_T2_Xlarge
    , ProductionVariantInstanceTypeMl_T2_2xlarge
    , ProductionVariantInstanceTypeMl_M4_Xlarge
    , ProductionVariantInstanceTypeMl_M4_2xlarge
    , ProductionVariantInstanceTypeMl_M4_4xlarge
    , ProductionVariantInstanceTypeMl_M4_10xlarge
    , ProductionVariantInstanceTypeMl_M4_16xlarge
    , ProductionVariantInstanceTypeMl_M5_Large
    , ProductionVariantInstanceTypeMl_M5_Xlarge
    , ProductionVariantInstanceTypeMl_M5_2xlarge
    , ProductionVariantInstanceTypeMl_M5_4xlarge
    , ProductionVariantInstanceTypeMl_M5_12xlarge
    , ProductionVariantInstanceTypeMl_M5_24xlarge
    , ProductionVariantInstanceTypeMl_M5d_Large
    , ProductionVariantInstanceTypeMl_M5d_Xlarge
    , ProductionVariantInstanceTypeMl_M5d_2xlarge
    , ProductionVariantInstanceTypeMl_M5d_4xlarge
    , ProductionVariantInstanceTypeMl_M5d_12xlarge
    , ProductionVariantInstanceTypeMl_M5d_24xlarge
    , ProductionVariantInstanceTypeMl_C4_Large
    , ProductionVariantInstanceTypeMl_C4_Xlarge
    , ProductionVariantInstanceTypeMl_C4_2xlarge
    , ProductionVariantInstanceTypeMl_C4_4xlarge
    , ProductionVariantInstanceTypeMl_C4_8xlarge
    , ProductionVariantInstanceTypeMl_P2_Xlarge
    , ProductionVariantInstanceTypeMl_P2_8xlarge
    , ProductionVariantInstanceTypeMl_P2_16xlarge
    , ProductionVariantInstanceTypeMl_P3_2xlarge
    , ProductionVariantInstanceTypeMl_P3_8xlarge
    , ProductionVariantInstanceTypeMl_P3_16xlarge
    , ProductionVariantInstanceTypeMl_C5_Large
    , ProductionVariantInstanceTypeMl_C5_Xlarge
    , ProductionVariantInstanceTypeMl_C5_2xlarge
    , ProductionVariantInstanceTypeMl_C5_4xlarge
    , ProductionVariantInstanceTypeMl_C5_9xlarge
    , ProductionVariantInstanceTypeMl_C5_18xlarge
    , ProductionVariantInstanceTypeMl_C5d_Large
    , ProductionVariantInstanceTypeMl_C5d_Xlarge
    , ProductionVariantInstanceTypeMl_C5d_2xlarge
    , ProductionVariantInstanceTypeMl_C5d_4xlarge
    , ProductionVariantInstanceTypeMl_C5d_9xlarge
    , ProductionVariantInstanceTypeMl_C5d_18xlarge
    , ProductionVariantInstanceTypeMl_G4dn_Xlarge
    , ProductionVariantInstanceTypeMl_G4dn_2xlarge
    , ProductionVariantInstanceTypeMl_G4dn_4xlarge
    , ProductionVariantInstanceTypeMl_G4dn_8xlarge
    , ProductionVariantInstanceTypeMl_G4dn_12xlarge
    , ProductionVariantInstanceTypeMl_G4dn_16xlarge
    , ProductionVariantInstanceTypeMl_R5_Large
    , ProductionVariantInstanceTypeMl_R5_Xlarge
    , ProductionVariantInstanceTypeMl_R5_2xlarge
    , ProductionVariantInstanceTypeMl_R5_4xlarge
    , ProductionVariantInstanceTypeMl_R5_12xlarge
    , ProductionVariantInstanceTypeMl_R5_24xlarge
    , ProductionVariantInstanceTypeMl_R5d_Large
    , ProductionVariantInstanceTypeMl_R5d_Xlarge
    , ProductionVariantInstanceTypeMl_R5d_2xlarge
    , ProductionVariantInstanceTypeMl_R5d_4xlarge
    , ProductionVariantInstanceTypeMl_R5d_12xlarge
    , ProductionVariantInstanceTypeMl_R5d_24xlarge
    , ProductionVariantInstanceTypeMl_INF1_Xlarge
    , ProductionVariantInstanceTypeMl_INF1_2xlarge
    , ProductionVariantInstanceTypeMl_INF1_6xlarge
    , ProductionVariantInstanceTypeMl_INF1_24xlarge
    , fromProductionVariantInstanceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProductionVariantInstanceType = ProductionVariantInstanceType'{fromProductionVariantInstanceType
                                                                       :: Core.Text}
                                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                          Core.Generic)
                                          deriving newtype (Core.IsString, Core.Hashable,
                                                            Core.NFData, Core.ToJSONKey,
                                                            Core.FromJSONKey, Core.ToJSON,
                                                            Core.FromJSON, Core.ToXML, Core.FromXML,
                                                            Core.ToText, Core.FromText,
                                                            Core.ToByteString, Core.ToQuery,
                                                            Core.ToHeader)

pattern ProductionVariantInstanceTypeMl_T2_Medium :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_T2_Medium = ProductionVariantInstanceType' "ml.t2.medium"

pattern ProductionVariantInstanceTypeMl_T2_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_T2_Large = ProductionVariantInstanceType' "ml.t2.large"

pattern ProductionVariantInstanceTypeMl_T2_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_T2_Xlarge = ProductionVariantInstanceType' "ml.t2.xlarge"

pattern ProductionVariantInstanceTypeMl_T2_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_T2_2xlarge = ProductionVariantInstanceType' "ml.t2.2xlarge"

pattern ProductionVariantInstanceTypeMl_M4_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M4_Xlarge = ProductionVariantInstanceType' "ml.m4.xlarge"

pattern ProductionVariantInstanceTypeMl_M4_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M4_2xlarge = ProductionVariantInstanceType' "ml.m4.2xlarge"

pattern ProductionVariantInstanceTypeMl_M4_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M4_4xlarge = ProductionVariantInstanceType' "ml.m4.4xlarge"

pattern ProductionVariantInstanceTypeMl_M4_10xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M4_10xlarge = ProductionVariantInstanceType' "ml.m4.10xlarge"

pattern ProductionVariantInstanceTypeMl_M4_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M4_16xlarge = ProductionVariantInstanceType' "ml.m4.16xlarge"

pattern ProductionVariantInstanceTypeMl_M5_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5_Large = ProductionVariantInstanceType' "ml.m5.large"

pattern ProductionVariantInstanceTypeMl_M5_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5_Xlarge = ProductionVariantInstanceType' "ml.m5.xlarge"

pattern ProductionVariantInstanceTypeMl_M5_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5_2xlarge = ProductionVariantInstanceType' "ml.m5.2xlarge"

pattern ProductionVariantInstanceTypeMl_M5_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5_4xlarge = ProductionVariantInstanceType' "ml.m5.4xlarge"

pattern ProductionVariantInstanceTypeMl_M5_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5_12xlarge = ProductionVariantInstanceType' "ml.m5.12xlarge"

pattern ProductionVariantInstanceTypeMl_M5_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5_24xlarge = ProductionVariantInstanceType' "ml.m5.24xlarge"

pattern ProductionVariantInstanceTypeMl_M5d_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5d_Large = ProductionVariantInstanceType' "ml.m5d.large"

pattern ProductionVariantInstanceTypeMl_M5d_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5d_Xlarge = ProductionVariantInstanceType' "ml.m5d.xlarge"

pattern ProductionVariantInstanceTypeMl_M5d_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5d_2xlarge = ProductionVariantInstanceType' "ml.m5d.2xlarge"

pattern ProductionVariantInstanceTypeMl_M5d_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5d_4xlarge = ProductionVariantInstanceType' "ml.m5d.4xlarge"

pattern ProductionVariantInstanceTypeMl_M5d_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5d_12xlarge = ProductionVariantInstanceType' "ml.m5d.12xlarge"

pattern ProductionVariantInstanceTypeMl_M5d_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_M5d_24xlarge = ProductionVariantInstanceType' "ml.m5d.24xlarge"

pattern ProductionVariantInstanceTypeMl_C4_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C4_Large = ProductionVariantInstanceType' "ml.c4.large"

pattern ProductionVariantInstanceTypeMl_C4_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C4_Xlarge = ProductionVariantInstanceType' "ml.c4.xlarge"

pattern ProductionVariantInstanceTypeMl_C4_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C4_2xlarge = ProductionVariantInstanceType' "ml.c4.2xlarge"

pattern ProductionVariantInstanceTypeMl_C4_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C4_4xlarge = ProductionVariantInstanceType' "ml.c4.4xlarge"

pattern ProductionVariantInstanceTypeMl_C4_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C4_8xlarge = ProductionVariantInstanceType' "ml.c4.8xlarge"

pattern ProductionVariantInstanceTypeMl_P2_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_P2_Xlarge = ProductionVariantInstanceType' "ml.p2.xlarge"

pattern ProductionVariantInstanceTypeMl_P2_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_P2_8xlarge = ProductionVariantInstanceType' "ml.p2.8xlarge"

pattern ProductionVariantInstanceTypeMl_P2_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_P2_16xlarge = ProductionVariantInstanceType' "ml.p2.16xlarge"

pattern ProductionVariantInstanceTypeMl_P3_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_P3_2xlarge = ProductionVariantInstanceType' "ml.p3.2xlarge"

pattern ProductionVariantInstanceTypeMl_P3_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_P3_8xlarge = ProductionVariantInstanceType' "ml.p3.8xlarge"

pattern ProductionVariantInstanceTypeMl_P3_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_P3_16xlarge = ProductionVariantInstanceType' "ml.p3.16xlarge"

pattern ProductionVariantInstanceTypeMl_C5_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5_Large = ProductionVariantInstanceType' "ml.c5.large"

pattern ProductionVariantInstanceTypeMl_C5_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5_Xlarge = ProductionVariantInstanceType' "ml.c5.xlarge"

pattern ProductionVariantInstanceTypeMl_C5_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5_2xlarge = ProductionVariantInstanceType' "ml.c5.2xlarge"

pattern ProductionVariantInstanceTypeMl_C5_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5_4xlarge = ProductionVariantInstanceType' "ml.c5.4xlarge"

pattern ProductionVariantInstanceTypeMl_C5_9xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5_9xlarge = ProductionVariantInstanceType' "ml.c5.9xlarge"

pattern ProductionVariantInstanceTypeMl_C5_18xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5_18xlarge = ProductionVariantInstanceType' "ml.c5.18xlarge"

pattern ProductionVariantInstanceTypeMl_C5d_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5d_Large = ProductionVariantInstanceType' "ml.c5d.large"

pattern ProductionVariantInstanceTypeMl_C5d_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5d_Xlarge = ProductionVariantInstanceType' "ml.c5d.xlarge"

pattern ProductionVariantInstanceTypeMl_C5d_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5d_2xlarge = ProductionVariantInstanceType' "ml.c5d.2xlarge"

pattern ProductionVariantInstanceTypeMl_C5d_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5d_4xlarge = ProductionVariantInstanceType' "ml.c5d.4xlarge"

pattern ProductionVariantInstanceTypeMl_C5d_9xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5d_9xlarge = ProductionVariantInstanceType' "ml.c5d.9xlarge"

pattern ProductionVariantInstanceTypeMl_C5d_18xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_C5d_18xlarge = ProductionVariantInstanceType' "ml.c5d.18xlarge"

pattern ProductionVariantInstanceTypeMl_G4dn_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_G4dn_Xlarge = ProductionVariantInstanceType' "ml.g4dn.xlarge"

pattern ProductionVariantInstanceTypeMl_G4dn_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_G4dn_2xlarge = ProductionVariantInstanceType' "ml.g4dn.2xlarge"

pattern ProductionVariantInstanceTypeMl_G4dn_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_G4dn_4xlarge = ProductionVariantInstanceType' "ml.g4dn.4xlarge"

pattern ProductionVariantInstanceTypeMl_G4dn_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_G4dn_8xlarge = ProductionVariantInstanceType' "ml.g4dn.8xlarge"

pattern ProductionVariantInstanceTypeMl_G4dn_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_G4dn_12xlarge = ProductionVariantInstanceType' "ml.g4dn.12xlarge"

pattern ProductionVariantInstanceTypeMl_G4dn_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_G4dn_16xlarge = ProductionVariantInstanceType' "ml.g4dn.16xlarge"

pattern ProductionVariantInstanceTypeMl_R5_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5_Large = ProductionVariantInstanceType' "ml.r5.large"

pattern ProductionVariantInstanceTypeMl_R5_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5_Xlarge = ProductionVariantInstanceType' "ml.r5.xlarge"

pattern ProductionVariantInstanceTypeMl_R5_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5_2xlarge = ProductionVariantInstanceType' "ml.r5.2xlarge"

pattern ProductionVariantInstanceTypeMl_R5_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5_4xlarge = ProductionVariantInstanceType' "ml.r5.4xlarge"

pattern ProductionVariantInstanceTypeMl_R5_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5_12xlarge = ProductionVariantInstanceType' "ml.r5.12xlarge"

pattern ProductionVariantInstanceTypeMl_R5_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5_24xlarge = ProductionVariantInstanceType' "ml.r5.24xlarge"

pattern ProductionVariantInstanceTypeMl_R5d_Large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5d_Large = ProductionVariantInstanceType' "ml.r5d.large"

pattern ProductionVariantInstanceTypeMl_R5d_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5d_Xlarge = ProductionVariantInstanceType' "ml.r5d.xlarge"

pattern ProductionVariantInstanceTypeMl_R5d_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5d_2xlarge = ProductionVariantInstanceType' "ml.r5d.2xlarge"

pattern ProductionVariantInstanceTypeMl_R5d_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5d_4xlarge = ProductionVariantInstanceType' "ml.r5d.4xlarge"

pattern ProductionVariantInstanceTypeMl_R5d_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5d_12xlarge = ProductionVariantInstanceType' "ml.r5d.12xlarge"

pattern ProductionVariantInstanceTypeMl_R5d_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_R5d_24xlarge = ProductionVariantInstanceType' "ml.r5d.24xlarge"

pattern ProductionVariantInstanceTypeMl_INF1_Xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_INF1_Xlarge = ProductionVariantInstanceType' "ml.inf1.xlarge"

pattern ProductionVariantInstanceTypeMl_INF1_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_INF1_2xlarge = ProductionVariantInstanceType' "ml.inf1.2xlarge"

pattern ProductionVariantInstanceTypeMl_INF1_6xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_INF1_6xlarge = ProductionVariantInstanceType' "ml.inf1.6xlarge"

pattern ProductionVariantInstanceTypeMl_INF1_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceTypeMl_INF1_24xlarge = ProductionVariantInstanceType' "ml.inf1.24xlarge"

{-# COMPLETE 
  ProductionVariantInstanceTypeMl_T2_Medium,

  ProductionVariantInstanceTypeMl_T2_Large,

  ProductionVariantInstanceTypeMl_T2_Xlarge,

  ProductionVariantInstanceTypeMl_T2_2xlarge,

  ProductionVariantInstanceTypeMl_M4_Xlarge,

  ProductionVariantInstanceTypeMl_M4_2xlarge,

  ProductionVariantInstanceTypeMl_M4_4xlarge,

  ProductionVariantInstanceTypeMl_M4_10xlarge,

  ProductionVariantInstanceTypeMl_M4_16xlarge,

  ProductionVariantInstanceTypeMl_M5_Large,

  ProductionVariantInstanceTypeMl_M5_Xlarge,

  ProductionVariantInstanceTypeMl_M5_2xlarge,

  ProductionVariantInstanceTypeMl_M5_4xlarge,

  ProductionVariantInstanceTypeMl_M5_12xlarge,

  ProductionVariantInstanceTypeMl_M5_24xlarge,

  ProductionVariantInstanceTypeMl_M5d_Large,

  ProductionVariantInstanceTypeMl_M5d_Xlarge,

  ProductionVariantInstanceTypeMl_M5d_2xlarge,

  ProductionVariantInstanceTypeMl_M5d_4xlarge,

  ProductionVariantInstanceTypeMl_M5d_12xlarge,

  ProductionVariantInstanceTypeMl_M5d_24xlarge,

  ProductionVariantInstanceTypeMl_C4_Large,

  ProductionVariantInstanceTypeMl_C4_Xlarge,

  ProductionVariantInstanceTypeMl_C4_2xlarge,

  ProductionVariantInstanceTypeMl_C4_4xlarge,

  ProductionVariantInstanceTypeMl_C4_8xlarge,

  ProductionVariantInstanceTypeMl_P2_Xlarge,

  ProductionVariantInstanceTypeMl_P2_8xlarge,

  ProductionVariantInstanceTypeMl_P2_16xlarge,

  ProductionVariantInstanceTypeMl_P3_2xlarge,

  ProductionVariantInstanceTypeMl_P3_8xlarge,

  ProductionVariantInstanceTypeMl_P3_16xlarge,

  ProductionVariantInstanceTypeMl_C5_Large,

  ProductionVariantInstanceTypeMl_C5_Xlarge,

  ProductionVariantInstanceTypeMl_C5_2xlarge,

  ProductionVariantInstanceTypeMl_C5_4xlarge,

  ProductionVariantInstanceTypeMl_C5_9xlarge,

  ProductionVariantInstanceTypeMl_C5_18xlarge,

  ProductionVariantInstanceTypeMl_C5d_Large,

  ProductionVariantInstanceTypeMl_C5d_Xlarge,

  ProductionVariantInstanceTypeMl_C5d_2xlarge,

  ProductionVariantInstanceTypeMl_C5d_4xlarge,

  ProductionVariantInstanceTypeMl_C5d_9xlarge,

  ProductionVariantInstanceTypeMl_C5d_18xlarge,

  ProductionVariantInstanceTypeMl_G4dn_Xlarge,

  ProductionVariantInstanceTypeMl_G4dn_2xlarge,

  ProductionVariantInstanceTypeMl_G4dn_4xlarge,

  ProductionVariantInstanceTypeMl_G4dn_8xlarge,

  ProductionVariantInstanceTypeMl_G4dn_12xlarge,

  ProductionVariantInstanceTypeMl_G4dn_16xlarge,

  ProductionVariantInstanceTypeMl_R5_Large,

  ProductionVariantInstanceTypeMl_R5_Xlarge,

  ProductionVariantInstanceTypeMl_R5_2xlarge,

  ProductionVariantInstanceTypeMl_R5_4xlarge,

  ProductionVariantInstanceTypeMl_R5_12xlarge,

  ProductionVariantInstanceTypeMl_R5_24xlarge,

  ProductionVariantInstanceTypeMl_R5d_Large,

  ProductionVariantInstanceTypeMl_R5d_Xlarge,

  ProductionVariantInstanceTypeMl_R5d_2xlarge,

  ProductionVariantInstanceTypeMl_R5d_4xlarge,

  ProductionVariantInstanceTypeMl_R5d_12xlarge,

  ProductionVariantInstanceTypeMl_R5d_24xlarge,

  ProductionVariantInstanceTypeMl_INF1_Xlarge,

  ProductionVariantInstanceTypeMl_INF1_2xlarge,

  ProductionVariantInstanceTypeMl_INF1_6xlarge,

  ProductionVariantInstanceTypeMl_INF1_24xlarge,
  ProductionVariantInstanceType'
  #-}

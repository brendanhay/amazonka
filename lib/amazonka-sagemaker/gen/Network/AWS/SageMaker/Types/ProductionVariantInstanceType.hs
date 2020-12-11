-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantInstanceType
  ( ProductionVariantInstanceType
      ( ProductionVariantInstanceType',
        PVITMl_C4_2XLarge,
        PVITMl_C4_4XLarge,
        PVITMl_C4_8XLarge,
        PVITMl_C4_Large,
        PVITMl_C4_XLarge,
        PVITMl_C5_18XLarge,
        PVITMl_C5_2XLarge,
        PVITMl_C5_4XLarge,
        PVITMl_C5_9XLarge,
        PVITMl_C5_Large,
        PVITMl_C5_XLarge,
        PVITMl_C5d_18XLarge,
        PVITMl_C5d_2XLarge,
        PVITMl_C5d_4XLarge,
        PVITMl_C5d_9XLarge,
        PVITMl_C5d_Large,
        PVITMl_C5d_XLarge,
        PVITMl_G4dn_12XLarge,
        PVITMl_G4dn_16XLarge,
        PVITMl_G4dn_2XLarge,
        PVITMl_G4dn_4XLarge,
        PVITMl_G4dn_8XLarge,
        PVITMl_G4dn_XLarge,
        PVITMl_INF1_24XLarge,
        PVITMl_INF1_2XLarge,
        PVITMl_INF1_6XLarge,
        PVITMl_INF1_XLarge,
        PVITMl_M4_10XLarge,
        PVITMl_M4_16XLarge,
        PVITMl_M4_2XLarge,
        PVITMl_M4_4XLarge,
        PVITMl_M4_XLarge,
        PVITMl_M5_12XLarge,
        PVITMl_M5_24XLarge,
        PVITMl_M5_2XLarge,
        PVITMl_M5_4XLarge,
        PVITMl_M5_Large,
        PVITMl_M5_XLarge,
        PVITMl_M5d_12XLarge,
        PVITMl_M5d_24XLarge,
        PVITMl_M5d_2XLarge,
        PVITMl_M5d_4XLarge,
        PVITMl_M5d_Large,
        PVITMl_M5d_XLarge,
        PVITMl_P2_16XLarge,
        PVITMl_P2_8XLarge,
        PVITMl_P2_XLarge,
        PVITMl_P3_16XLarge,
        PVITMl_P3_2XLarge,
        PVITMl_P3_8XLarge,
        PVITMl_R5_12XLarge,
        PVITMl_R5_24XLarge,
        PVITMl_R5_2XLarge,
        PVITMl_R5_4XLarge,
        PVITMl_R5_Large,
        PVITMl_R5_XLarge,
        PVITMl_R5d_12XLarge,
        PVITMl_R5d_24XLarge,
        PVITMl_R5d_2XLarge,
        PVITMl_R5d_4XLarge,
        PVITMl_R5d_Large,
        PVITMl_R5d_XLarge,
        PVITMl_T2_2XLarge,
        PVITMl_T2_Large,
        PVITMl_T2_Medium,
        PVITMl_T2_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProductionVariantInstanceType = ProductionVariantInstanceType' Lude.Text
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

pattern PVITMl_C4_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_2XLarge = ProductionVariantInstanceType' "ml.c4.2xlarge"

pattern PVITMl_C4_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_4XLarge = ProductionVariantInstanceType' "ml.c4.4xlarge"

pattern PVITMl_C4_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_8XLarge = ProductionVariantInstanceType' "ml.c4.8xlarge"

pattern PVITMl_C4_Large :: ProductionVariantInstanceType
pattern PVITMl_C4_Large = ProductionVariantInstanceType' "ml.c4.large"

pattern PVITMl_C4_XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_XLarge = ProductionVariantInstanceType' "ml.c4.xlarge"

pattern PVITMl_C5_18XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_18XLarge = ProductionVariantInstanceType' "ml.c5.18xlarge"

pattern PVITMl_C5_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_2XLarge = ProductionVariantInstanceType' "ml.c5.2xlarge"

pattern PVITMl_C5_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_4XLarge = ProductionVariantInstanceType' "ml.c5.4xlarge"

pattern PVITMl_C5_9XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_9XLarge = ProductionVariantInstanceType' "ml.c5.9xlarge"

pattern PVITMl_C5_Large :: ProductionVariantInstanceType
pattern PVITMl_C5_Large = ProductionVariantInstanceType' "ml.c5.large"

pattern PVITMl_C5_XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_XLarge = ProductionVariantInstanceType' "ml.c5.xlarge"

pattern PVITMl_C5d_18XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_18XLarge = ProductionVariantInstanceType' "ml.c5d.18xlarge"

pattern PVITMl_C5d_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_2XLarge = ProductionVariantInstanceType' "ml.c5d.2xlarge"

pattern PVITMl_C5d_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_4XLarge = ProductionVariantInstanceType' "ml.c5d.4xlarge"

pattern PVITMl_C5d_9XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_9XLarge = ProductionVariantInstanceType' "ml.c5d.9xlarge"

pattern PVITMl_C5d_Large :: ProductionVariantInstanceType
pattern PVITMl_C5d_Large = ProductionVariantInstanceType' "ml.c5d.large"

pattern PVITMl_C5d_XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_XLarge = ProductionVariantInstanceType' "ml.c5d.xlarge"

pattern PVITMl_G4dn_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_12XLarge = ProductionVariantInstanceType' "ml.g4dn.12xlarge"

pattern PVITMl_G4dn_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_16XLarge = ProductionVariantInstanceType' "ml.g4dn.16xlarge"

pattern PVITMl_G4dn_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_2XLarge = ProductionVariantInstanceType' "ml.g4dn.2xlarge"

pattern PVITMl_G4dn_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_4XLarge = ProductionVariantInstanceType' "ml.g4dn.4xlarge"

pattern PVITMl_G4dn_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_8XLarge = ProductionVariantInstanceType' "ml.g4dn.8xlarge"

pattern PVITMl_G4dn_XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_XLarge = ProductionVariantInstanceType' "ml.g4dn.xlarge"

pattern PVITMl_INF1_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_24XLarge = ProductionVariantInstanceType' "ml.inf1.24xlarge"

pattern PVITMl_INF1_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_2XLarge = ProductionVariantInstanceType' "ml.inf1.2xlarge"

pattern PVITMl_INF1_6XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_6XLarge = ProductionVariantInstanceType' "ml.inf1.6xlarge"

pattern PVITMl_INF1_XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_XLarge = ProductionVariantInstanceType' "ml.inf1.xlarge"

pattern PVITMl_M4_10XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_10XLarge = ProductionVariantInstanceType' "ml.m4.10xlarge"

pattern PVITMl_M4_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_16XLarge = ProductionVariantInstanceType' "ml.m4.16xlarge"

pattern PVITMl_M4_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_2XLarge = ProductionVariantInstanceType' "ml.m4.2xlarge"

pattern PVITMl_M4_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_4XLarge = ProductionVariantInstanceType' "ml.m4.4xlarge"

pattern PVITMl_M4_XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_XLarge = ProductionVariantInstanceType' "ml.m4.xlarge"

pattern PVITMl_M5_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_12XLarge = ProductionVariantInstanceType' "ml.m5.12xlarge"

pattern PVITMl_M5_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_24XLarge = ProductionVariantInstanceType' "ml.m5.24xlarge"

pattern PVITMl_M5_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_2XLarge = ProductionVariantInstanceType' "ml.m5.2xlarge"

pattern PVITMl_M5_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_4XLarge = ProductionVariantInstanceType' "ml.m5.4xlarge"

pattern PVITMl_M5_Large :: ProductionVariantInstanceType
pattern PVITMl_M5_Large = ProductionVariantInstanceType' "ml.m5.large"

pattern PVITMl_M5_XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_XLarge = ProductionVariantInstanceType' "ml.m5.xlarge"

pattern PVITMl_M5d_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_12XLarge = ProductionVariantInstanceType' "ml.m5d.12xlarge"

pattern PVITMl_M5d_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_24XLarge = ProductionVariantInstanceType' "ml.m5d.24xlarge"

pattern PVITMl_M5d_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_2XLarge = ProductionVariantInstanceType' "ml.m5d.2xlarge"

pattern PVITMl_M5d_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_4XLarge = ProductionVariantInstanceType' "ml.m5d.4xlarge"

pattern PVITMl_M5d_Large :: ProductionVariantInstanceType
pattern PVITMl_M5d_Large = ProductionVariantInstanceType' "ml.m5d.large"

pattern PVITMl_M5d_XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_XLarge = ProductionVariantInstanceType' "ml.m5d.xlarge"

pattern PVITMl_P2_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_P2_16XLarge = ProductionVariantInstanceType' "ml.p2.16xlarge"

pattern PVITMl_P2_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_P2_8XLarge = ProductionVariantInstanceType' "ml.p2.8xlarge"

pattern PVITMl_P2_XLarge :: ProductionVariantInstanceType
pattern PVITMl_P2_XLarge = ProductionVariantInstanceType' "ml.p2.xlarge"

pattern PVITMl_P3_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_P3_16XLarge = ProductionVariantInstanceType' "ml.p3.16xlarge"

pattern PVITMl_P3_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_P3_2XLarge = ProductionVariantInstanceType' "ml.p3.2xlarge"

pattern PVITMl_P3_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_P3_8XLarge = ProductionVariantInstanceType' "ml.p3.8xlarge"

pattern PVITMl_R5_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_12XLarge = ProductionVariantInstanceType' "ml.r5.12xlarge"

pattern PVITMl_R5_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_24XLarge = ProductionVariantInstanceType' "ml.r5.24xlarge"

pattern PVITMl_R5_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_2XLarge = ProductionVariantInstanceType' "ml.r5.2xlarge"

pattern PVITMl_R5_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_4XLarge = ProductionVariantInstanceType' "ml.r5.4xlarge"

pattern PVITMl_R5_Large :: ProductionVariantInstanceType
pattern PVITMl_R5_Large = ProductionVariantInstanceType' "ml.r5.large"

pattern PVITMl_R5_XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_XLarge = ProductionVariantInstanceType' "ml.r5.xlarge"

pattern PVITMl_R5d_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_12XLarge = ProductionVariantInstanceType' "ml.r5d.12xlarge"

pattern PVITMl_R5d_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_24XLarge = ProductionVariantInstanceType' "ml.r5d.24xlarge"

pattern PVITMl_R5d_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_2XLarge = ProductionVariantInstanceType' "ml.r5d.2xlarge"

pattern PVITMl_R5d_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_4XLarge = ProductionVariantInstanceType' "ml.r5d.4xlarge"

pattern PVITMl_R5d_Large :: ProductionVariantInstanceType
pattern PVITMl_R5d_Large = ProductionVariantInstanceType' "ml.r5d.large"

pattern PVITMl_R5d_XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_XLarge = ProductionVariantInstanceType' "ml.r5d.xlarge"

pattern PVITMl_T2_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_T2_2XLarge = ProductionVariantInstanceType' "ml.t2.2xlarge"

pattern PVITMl_T2_Large :: ProductionVariantInstanceType
pattern PVITMl_T2_Large = ProductionVariantInstanceType' "ml.t2.large"

pattern PVITMl_T2_Medium :: ProductionVariantInstanceType
pattern PVITMl_T2_Medium = ProductionVariantInstanceType' "ml.t2.medium"

pattern PVITMl_T2_XLarge :: ProductionVariantInstanceType
pattern PVITMl_T2_XLarge = ProductionVariantInstanceType' "ml.t2.xlarge"

{-# COMPLETE
  PVITMl_C4_2XLarge,
  PVITMl_C4_4XLarge,
  PVITMl_C4_8XLarge,
  PVITMl_C4_Large,
  PVITMl_C4_XLarge,
  PVITMl_C5_18XLarge,
  PVITMl_C5_2XLarge,
  PVITMl_C5_4XLarge,
  PVITMl_C5_9XLarge,
  PVITMl_C5_Large,
  PVITMl_C5_XLarge,
  PVITMl_C5d_18XLarge,
  PVITMl_C5d_2XLarge,
  PVITMl_C5d_4XLarge,
  PVITMl_C5d_9XLarge,
  PVITMl_C5d_Large,
  PVITMl_C5d_XLarge,
  PVITMl_G4dn_12XLarge,
  PVITMl_G4dn_16XLarge,
  PVITMl_G4dn_2XLarge,
  PVITMl_G4dn_4XLarge,
  PVITMl_G4dn_8XLarge,
  PVITMl_G4dn_XLarge,
  PVITMl_INF1_24XLarge,
  PVITMl_INF1_2XLarge,
  PVITMl_INF1_6XLarge,
  PVITMl_INF1_XLarge,
  PVITMl_M4_10XLarge,
  PVITMl_M4_16XLarge,
  PVITMl_M4_2XLarge,
  PVITMl_M4_4XLarge,
  PVITMl_M4_XLarge,
  PVITMl_M5_12XLarge,
  PVITMl_M5_24XLarge,
  PVITMl_M5_2XLarge,
  PVITMl_M5_4XLarge,
  PVITMl_M5_Large,
  PVITMl_M5_XLarge,
  PVITMl_M5d_12XLarge,
  PVITMl_M5d_24XLarge,
  PVITMl_M5d_2XLarge,
  PVITMl_M5d_4XLarge,
  PVITMl_M5d_Large,
  PVITMl_M5d_XLarge,
  PVITMl_P2_16XLarge,
  PVITMl_P2_8XLarge,
  PVITMl_P2_XLarge,
  PVITMl_P3_16XLarge,
  PVITMl_P3_2XLarge,
  PVITMl_P3_8XLarge,
  PVITMl_R5_12XLarge,
  PVITMl_R5_24XLarge,
  PVITMl_R5_2XLarge,
  PVITMl_R5_4XLarge,
  PVITMl_R5_Large,
  PVITMl_R5_XLarge,
  PVITMl_R5d_12XLarge,
  PVITMl_R5d_24XLarge,
  PVITMl_R5d_2XLarge,
  PVITMl_R5d_4XLarge,
  PVITMl_R5d_Large,
  PVITMl_R5d_XLarge,
  PVITMl_T2_2XLarge,
  PVITMl_T2_Large,
  PVITMl_T2_Medium,
  PVITMl_T2_XLarge,
  ProductionVariantInstanceType'
  #-}

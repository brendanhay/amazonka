-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InstanceType
  ( InstanceType
      ( InstanceType',
        ITMl_C4_2XLarge,
        ITMl_C4_4XLarge,
        ITMl_C4_8XLarge,
        ITMl_C4_XLarge,
        ITMl_C5_18XLarge,
        ITMl_C5_2XLarge,
        ITMl_C5_4XLarge,
        ITMl_C5_9XLarge,
        ITMl_C5_XLarge,
        ITMl_C5d_18XLarge,
        ITMl_C5d_2XLarge,
        ITMl_C5d_4XLarge,
        ITMl_C5d_9XLarge,
        ITMl_C5d_XLarge,
        ITMl_M4_10XLarge,
        ITMl_M4_16XLarge,
        ITMl_M4_2XLarge,
        ITMl_M4_4XLarge,
        ITMl_M4_XLarge,
        ITMl_M5_12XLarge,
        ITMl_M5_24XLarge,
        ITMl_M5_2XLarge,
        ITMl_M5_4XLarge,
        ITMl_M5_XLarge,
        ITMl_P2_16XLarge,
        ITMl_P2_8XLarge,
        ITMl_P2_XLarge,
        ITMl_P3_16XLarge,
        ITMl_P3_2XLarge,
        ITMl_P3_8XLarge,
        ITMl_T2_2XLarge,
        ITMl_T2_Large,
        ITMl_T2_Medium,
        ITMl_T2_XLarge,
        ITMl_T3_2XLarge,
        ITMl_T3_Large,
        ITMl_T3_Medium,
        ITMl_T3_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceType = InstanceType' Lude.Text
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

pattern ITMl_C4_2XLarge :: InstanceType
pattern ITMl_C4_2XLarge = InstanceType' "ml.c4.2xlarge"

pattern ITMl_C4_4XLarge :: InstanceType
pattern ITMl_C4_4XLarge = InstanceType' "ml.c4.4xlarge"

pattern ITMl_C4_8XLarge :: InstanceType
pattern ITMl_C4_8XLarge = InstanceType' "ml.c4.8xlarge"

pattern ITMl_C4_XLarge :: InstanceType
pattern ITMl_C4_XLarge = InstanceType' "ml.c4.xlarge"

pattern ITMl_C5_18XLarge :: InstanceType
pattern ITMl_C5_18XLarge = InstanceType' "ml.c5.18xlarge"

pattern ITMl_C5_2XLarge :: InstanceType
pattern ITMl_C5_2XLarge = InstanceType' "ml.c5.2xlarge"

pattern ITMl_C5_4XLarge :: InstanceType
pattern ITMl_C5_4XLarge = InstanceType' "ml.c5.4xlarge"

pattern ITMl_C5_9XLarge :: InstanceType
pattern ITMl_C5_9XLarge = InstanceType' "ml.c5.9xlarge"

pattern ITMl_C5_XLarge :: InstanceType
pattern ITMl_C5_XLarge = InstanceType' "ml.c5.xlarge"

pattern ITMl_C5d_18XLarge :: InstanceType
pattern ITMl_C5d_18XLarge = InstanceType' "ml.c5d.18xlarge"

pattern ITMl_C5d_2XLarge :: InstanceType
pattern ITMl_C5d_2XLarge = InstanceType' "ml.c5d.2xlarge"

pattern ITMl_C5d_4XLarge :: InstanceType
pattern ITMl_C5d_4XLarge = InstanceType' "ml.c5d.4xlarge"

pattern ITMl_C5d_9XLarge :: InstanceType
pattern ITMl_C5d_9XLarge = InstanceType' "ml.c5d.9xlarge"

pattern ITMl_C5d_XLarge :: InstanceType
pattern ITMl_C5d_XLarge = InstanceType' "ml.c5d.xlarge"

pattern ITMl_M4_10XLarge :: InstanceType
pattern ITMl_M4_10XLarge = InstanceType' "ml.m4.10xlarge"

pattern ITMl_M4_16XLarge :: InstanceType
pattern ITMl_M4_16XLarge = InstanceType' "ml.m4.16xlarge"

pattern ITMl_M4_2XLarge :: InstanceType
pattern ITMl_M4_2XLarge = InstanceType' "ml.m4.2xlarge"

pattern ITMl_M4_4XLarge :: InstanceType
pattern ITMl_M4_4XLarge = InstanceType' "ml.m4.4xlarge"

pattern ITMl_M4_XLarge :: InstanceType
pattern ITMl_M4_XLarge = InstanceType' "ml.m4.xlarge"

pattern ITMl_M5_12XLarge :: InstanceType
pattern ITMl_M5_12XLarge = InstanceType' "ml.m5.12xlarge"

pattern ITMl_M5_24XLarge :: InstanceType
pattern ITMl_M5_24XLarge = InstanceType' "ml.m5.24xlarge"

pattern ITMl_M5_2XLarge :: InstanceType
pattern ITMl_M5_2XLarge = InstanceType' "ml.m5.2xlarge"

pattern ITMl_M5_4XLarge :: InstanceType
pattern ITMl_M5_4XLarge = InstanceType' "ml.m5.4xlarge"

pattern ITMl_M5_XLarge :: InstanceType
pattern ITMl_M5_XLarge = InstanceType' "ml.m5.xlarge"

pattern ITMl_P2_16XLarge :: InstanceType
pattern ITMl_P2_16XLarge = InstanceType' "ml.p2.16xlarge"

pattern ITMl_P2_8XLarge :: InstanceType
pattern ITMl_P2_8XLarge = InstanceType' "ml.p2.8xlarge"

pattern ITMl_P2_XLarge :: InstanceType
pattern ITMl_P2_XLarge = InstanceType' "ml.p2.xlarge"

pattern ITMl_P3_16XLarge :: InstanceType
pattern ITMl_P3_16XLarge = InstanceType' "ml.p3.16xlarge"

pattern ITMl_P3_2XLarge :: InstanceType
pattern ITMl_P3_2XLarge = InstanceType' "ml.p3.2xlarge"

pattern ITMl_P3_8XLarge :: InstanceType
pattern ITMl_P3_8XLarge = InstanceType' "ml.p3.8xlarge"

pattern ITMl_T2_2XLarge :: InstanceType
pattern ITMl_T2_2XLarge = InstanceType' "ml.t2.2xlarge"

pattern ITMl_T2_Large :: InstanceType
pattern ITMl_T2_Large = InstanceType' "ml.t2.large"

pattern ITMl_T2_Medium :: InstanceType
pattern ITMl_T2_Medium = InstanceType' "ml.t2.medium"

pattern ITMl_T2_XLarge :: InstanceType
pattern ITMl_T2_XLarge = InstanceType' "ml.t2.xlarge"

pattern ITMl_T3_2XLarge :: InstanceType
pattern ITMl_T3_2XLarge = InstanceType' "ml.t3.2xlarge"

pattern ITMl_T3_Large :: InstanceType
pattern ITMl_T3_Large = InstanceType' "ml.t3.large"

pattern ITMl_T3_Medium :: InstanceType
pattern ITMl_T3_Medium = InstanceType' "ml.t3.medium"

pattern ITMl_T3_XLarge :: InstanceType
pattern ITMl_T3_XLarge = InstanceType' "ml.t3.xlarge"

{-# COMPLETE
  ITMl_C4_2XLarge,
  ITMl_C4_4XLarge,
  ITMl_C4_8XLarge,
  ITMl_C4_XLarge,
  ITMl_C5_18XLarge,
  ITMl_C5_2XLarge,
  ITMl_C5_4XLarge,
  ITMl_C5_9XLarge,
  ITMl_C5_XLarge,
  ITMl_C5d_18XLarge,
  ITMl_C5d_2XLarge,
  ITMl_C5d_4XLarge,
  ITMl_C5d_9XLarge,
  ITMl_C5d_XLarge,
  ITMl_M4_10XLarge,
  ITMl_M4_16XLarge,
  ITMl_M4_2XLarge,
  ITMl_M4_4XLarge,
  ITMl_M4_XLarge,
  ITMl_M5_12XLarge,
  ITMl_M5_24XLarge,
  ITMl_M5_2XLarge,
  ITMl_M5_4XLarge,
  ITMl_M5_XLarge,
  ITMl_P2_16XLarge,
  ITMl_P2_8XLarge,
  ITMl_P2_XLarge,
  ITMl_P3_16XLarge,
  ITMl_P3_2XLarge,
  ITMl_P3_8XLarge,
  ITMl_T2_2XLarge,
  ITMl_T2_Large,
  ITMl_T2_Medium,
  ITMl_T2_XLarge,
  ITMl_T3_2XLarge,
  ITMl_T3_Large,
  ITMl_T3_Medium,
  ITMl_T3_XLarge,
  InstanceType'
  #-}

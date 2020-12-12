{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingInstanceType
  ( ProcessingInstanceType
      ( ProcessingInstanceType',
        PITMl_C4_2XLarge,
        PITMl_C4_4XLarge,
        PITMl_C4_8XLarge,
        PITMl_C4_XLarge,
        PITMl_C5_18XLarge,
        PITMl_C5_2XLarge,
        PITMl_C5_4XLarge,
        PITMl_C5_9XLarge,
        PITMl_C5_XLarge,
        PITMl_M4_10XLarge,
        PITMl_M4_16XLarge,
        PITMl_M4_2XLarge,
        PITMl_M4_4XLarge,
        PITMl_M4_XLarge,
        PITMl_M5_12XLarge,
        PITMl_M5_24XLarge,
        PITMl_M5_2XLarge,
        PITMl_M5_4XLarge,
        PITMl_M5_Large,
        PITMl_M5_XLarge,
        PITMl_P2_16XLarge,
        PITMl_P2_8XLarge,
        PITMl_P2_XLarge,
        PITMl_P3_16XLarge,
        PITMl_P3_2XLarge,
        PITMl_P3_8XLarge,
        PITMl_R5_12XLarge,
        PITMl_R5_16XLarge,
        PITMl_R5_24XLarge,
        PITMl_R5_2XLarge,
        PITMl_R5_4XLarge,
        PITMl_R5_8XLarge,
        PITMl_R5_Large,
        PITMl_R5_XLarge,
        PITMl_T3_2XLarge,
        PITMl_T3_Large,
        PITMl_T3_Medium,
        PITMl_T3_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProcessingInstanceType = ProcessingInstanceType' Lude.Text
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

pattern PITMl_C4_2XLarge :: ProcessingInstanceType
pattern PITMl_C4_2XLarge = ProcessingInstanceType' "ml.c4.2xlarge"

pattern PITMl_C4_4XLarge :: ProcessingInstanceType
pattern PITMl_C4_4XLarge = ProcessingInstanceType' "ml.c4.4xlarge"

pattern PITMl_C4_8XLarge :: ProcessingInstanceType
pattern PITMl_C4_8XLarge = ProcessingInstanceType' "ml.c4.8xlarge"

pattern PITMl_C4_XLarge :: ProcessingInstanceType
pattern PITMl_C4_XLarge = ProcessingInstanceType' "ml.c4.xlarge"

pattern PITMl_C5_18XLarge :: ProcessingInstanceType
pattern PITMl_C5_18XLarge = ProcessingInstanceType' "ml.c5.18xlarge"

pattern PITMl_C5_2XLarge :: ProcessingInstanceType
pattern PITMl_C5_2XLarge = ProcessingInstanceType' "ml.c5.2xlarge"

pattern PITMl_C5_4XLarge :: ProcessingInstanceType
pattern PITMl_C5_4XLarge = ProcessingInstanceType' "ml.c5.4xlarge"

pattern PITMl_C5_9XLarge :: ProcessingInstanceType
pattern PITMl_C5_9XLarge = ProcessingInstanceType' "ml.c5.9xlarge"

pattern PITMl_C5_XLarge :: ProcessingInstanceType
pattern PITMl_C5_XLarge = ProcessingInstanceType' "ml.c5.xlarge"

pattern PITMl_M4_10XLarge :: ProcessingInstanceType
pattern PITMl_M4_10XLarge = ProcessingInstanceType' "ml.m4.10xlarge"

pattern PITMl_M4_16XLarge :: ProcessingInstanceType
pattern PITMl_M4_16XLarge = ProcessingInstanceType' "ml.m4.16xlarge"

pattern PITMl_M4_2XLarge :: ProcessingInstanceType
pattern PITMl_M4_2XLarge = ProcessingInstanceType' "ml.m4.2xlarge"

pattern PITMl_M4_4XLarge :: ProcessingInstanceType
pattern PITMl_M4_4XLarge = ProcessingInstanceType' "ml.m4.4xlarge"

pattern PITMl_M4_XLarge :: ProcessingInstanceType
pattern PITMl_M4_XLarge = ProcessingInstanceType' "ml.m4.xlarge"

pattern PITMl_M5_12XLarge :: ProcessingInstanceType
pattern PITMl_M5_12XLarge = ProcessingInstanceType' "ml.m5.12xlarge"

pattern PITMl_M5_24XLarge :: ProcessingInstanceType
pattern PITMl_M5_24XLarge = ProcessingInstanceType' "ml.m5.24xlarge"

pattern PITMl_M5_2XLarge :: ProcessingInstanceType
pattern PITMl_M5_2XLarge = ProcessingInstanceType' "ml.m5.2xlarge"

pattern PITMl_M5_4XLarge :: ProcessingInstanceType
pattern PITMl_M5_4XLarge = ProcessingInstanceType' "ml.m5.4xlarge"

pattern PITMl_M5_Large :: ProcessingInstanceType
pattern PITMl_M5_Large = ProcessingInstanceType' "ml.m5.large"

pattern PITMl_M5_XLarge :: ProcessingInstanceType
pattern PITMl_M5_XLarge = ProcessingInstanceType' "ml.m5.xlarge"

pattern PITMl_P2_16XLarge :: ProcessingInstanceType
pattern PITMl_P2_16XLarge = ProcessingInstanceType' "ml.p2.16xlarge"

pattern PITMl_P2_8XLarge :: ProcessingInstanceType
pattern PITMl_P2_8XLarge = ProcessingInstanceType' "ml.p2.8xlarge"

pattern PITMl_P2_XLarge :: ProcessingInstanceType
pattern PITMl_P2_XLarge = ProcessingInstanceType' "ml.p2.xlarge"

pattern PITMl_P3_16XLarge :: ProcessingInstanceType
pattern PITMl_P3_16XLarge = ProcessingInstanceType' "ml.p3.16xlarge"

pattern PITMl_P3_2XLarge :: ProcessingInstanceType
pattern PITMl_P3_2XLarge = ProcessingInstanceType' "ml.p3.2xlarge"

pattern PITMl_P3_8XLarge :: ProcessingInstanceType
pattern PITMl_P3_8XLarge = ProcessingInstanceType' "ml.p3.8xlarge"

pattern PITMl_R5_12XLarge :: ProcessingInstanceType
pattern PITMl_R5_12XLarge = ProcessingInstanceType' "ml.r5.12xlarge"

pattern PITMl_R5_16XLarge :: ProcessingInstanceType
pattern PITMl_R5_16XLarge = ProcessingInstanceType' "ml.r5.16xlarge"

pattern PITMl_R5_24XLarge :: ProcessingInstanceType
pattern PITMl_R5_24XLarge = ProcessingInstanceType' "ml.r5.24xlarge"

pattern PITMl_R5_2XLarge :: ProcessingInstanceType
pattern PITMl_R5_2XLarge = ProcessingInstanceType' "ml.r5.2xlarge"

pattern PITMl_R5_4XLarge :: ProcessingInstanceType
pattern PITMl_R5_4XLarge = ProcessingInstanceType' "ml.r5.4xlarge"

pattern PITMl_R5_8XLarge :: ProcessingInstanceType
pattern PITMl_R5_8XLarge = ProcessingInstanceType' "ml.r5.8xlarge"

pattern PITMl_R5_Large :: ProcessingInstanceType
pattern PITMl_R5_Large = ProcessingInstanceType' "ml.r5.large"

pattern PITMl_R5_XLarge :: ProcessingInstanceType
pattern PITMl_R5_XLarge = ProcessingInstanceType' "ml.r5.xlarge"

pattern PITMl_T3_2XLarge :: ProcessingInstanceType
pattern PITMl_T3_2XLarge = ProcessingInstanceType' "ml.t3.2xlarge"

pattern PITMl_T3_Large :: ProcessingInstanceType
pattern PITMl_T3_Large = ProcessingInstanceType' "ml.t3.large"

pattern PITMl_T3_Medium :: ProcessingInstanceType
pattern PITMl_T3_Medium = ProcessingInstanceType' "ml.t3.medium"

pattern PITMl_T3_XLarge :: ProcessingInstanceType
pattern PITMl_T3_XLarge = ProcessingInstanceType' "ml.t3.xlarge"

{-# COMPLETE
  PITMl_C4_2XLarge,
  PITMl_C4_4XLarge,
  PITMl_C4_8XLarge,
  PITMl_C4_XLarge,
  PITMl_C5_18XLarge,
  PITMl_C5_2XLarge,
  PITMl_C5_4XLarge,
  PITMl_C5_9XLarge,
  PITMl_C5_XLarge,
  PITMl_M4_10XLarge,
  PITMl_M4_16XLarge,
  PITMl_M4_2XLarge,
  PITMl_M4_4XLarge,
  PITMl_M4_XLarge,
  PITMl_M5_12XLarge,
  PITMl_M5_24XLarge,
  PITMl_M5_2XLarge,
  PITMl_M5_4XLarge,
  PITMl_M5_Large,
  PITMl_M5_XLarge,
  PITMl_P2_16XLarge,
  PITMl_P2_8XLarge,
  PITMl_P2_XLarge,
  PITMl_P3_16XLarge,
  PITMl_P3_2XLarge,
  PITMl_P3_8XLarge,
  PITMl_R5_12XLarge,
  PITMl_R5_16XLarge,
  PITMl_R5_24XLarge,
  PITMl_R5_2XLarge,
  PITMl_R5_4XLarge,
  PITMl_R5_8XLarge,
  PITMl_R5_Large,
  PITMl_R5_XLarge,
  PITMl_T3_2XLarge,
  PITMl_T3_Large,
  PITMl_T3_Medium,
  PITMl_T3_XLarge,
  ProcessingInstanceType'
  #-}

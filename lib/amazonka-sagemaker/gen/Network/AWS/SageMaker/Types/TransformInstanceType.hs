{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformInstanceType
  ( TransformInstanceType
      ( TransformInstanceType',
        TMl_M4_XLarge,
        TMl_M4_2XLarge,
        TMl_M4_4XLarge,
        TMl_M4_10XLarge,
        TMl_M4_16XLarge,
        TMl_C4_XLarge,
        TMl_C4_2XLarge,
        TMl_C4_4XLarge,
        TMl_C4_8XLarge,
        TMl_P2_XLarge,
        TMl_P2_8XLarge,
        TMl_P2_16XLarge,
        TMl_P3_2XLarge,
        TMl_P3_8XLarge,
        TMl_P3_16XLarge,
        TMl_C5_XLarge,
        TMl_C5_2XLarge,
        TMl_C5_4XLarge,
        TMl_C5_9XLarge,
        TMl_C5_18XLarge,
        TMl_M5_Large,
        TMl_M5_XLarge,
        TMl_M5_2XLarge,
        TMl_M5_4XLarge,
        TMl_M5_12XLarge,
        TMl_M5_24XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransformInstanceType = TransformInstanceType' Lude.Text
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

pattern TMl_M4_XLarge :: TransformInstanceType
pattern TMl_M4_XLarge = TransformInstanceType' "ml.m4.xlarge"

pattern TMl_M4_2XLarge :: TransformInstanceType
pattern TMl_M4_2XLarge = TransformInstanceType' "ml.m4.2xlarge"

pattern TMl_M4_4XLarge :: TransformInstanceType
pattern TMl_M4_4XLarge = TransformInstanceType' "ml.m4.4xlarge"

pattern TMl_M4_10XLarge :: TransformInstanceType
pattern TMl_M4_10XLarge = TransformInstanceType' "ml.m4.10xlarge"

pattern TMl_M4_16XLarge :: TransformInstanceType
pattern TMl_M4_16XLarge = TransformInstanceType' "ml.m4.16xlarge"

pattern TMl_C4_XLarge :: TransformInstanceType
pattern TMl_C4_XLarge = TransformInstanceType' "ml.c4.xlarge"

pattern TMl_C4_2XLarge :: TransformInstanceType
pattern TMl_C4_2XLarge = TransformInstanceType' "ml.c4.2xlarge"

pattern TMl_C4_4XLarge :: TransformInstanceType
pattern TMl_C4_4XLarge = TransformInstanceType' "ml.c4.4xlarge"

pattern TMl_C4_8XLarge :: TransformInstanceType
pattern TMl_C4_8XLarge = TransformInstanceType' "ml.c4.8xlarge"

pattern TMl_P2_XLarge :: TransformInstanceType
pattern TMl_P2_XLarge = TransformInstanceType' "ml.p2.xlarge"

pattern TMl_P2_8XLarge :: TransformInstanceType
pattern TMl_P2_8XLarge = TransformInstanceType' "ml.p2.8xlarge"

pattern TMl_P2_16XLarge :: TransformInstanceType
pattern TMl_P2_16XLarge = TransformInstanceType' "ml.p2.16xlarge"

pattern TMl_P3_2XLarge :: TransformInstanceType
pattern TMl_P3_2XLarge = TransformInstanceType' "ml.p3.2xlarge"

pattern TMl_P3_8XLarge :: TransformInstanceType
pattern TMl_P3_8XLarge = TransformInstanceType' "ml.p3.8xlarge"

pattern TMl_P3_16XLarge :: TransformInstanceType
pattern TMl_P3_16XLarge = TransformInstanceType' "ml.p3.16xlarge"

pattern TMl_C5_XLarge :: TransformInstanceType
pattern TMl_C5_XLarge = TransformInstanceType' "ml.c5.xlarge"

pattern TMl_C5_2XLarge :: TransformInstanceType
pattern TMl_C5_2XLarge = TransformInstanceType' "ml.c5.2xlarge"

pattern TMl_C5_4XLarge :: TransformInstanceType
pattern TMl_C5_4XLarge = TransformInstanceType' "ml.c5.4xlarge"

pattern TMl_C5_9XLarge :: TransformInstanceType
pattern TMl_C5_9XLarge = TransformInstanceType' "ml.c5.9xlarge"

pattern TMl_C5_18XLarge :: TransformInstanceType
pattern TMl_C5_18XLarge = TransformInstanceType' "ml.c5.18xlarge"

pattern TMl_M5_Large :: TransformInstanceType
pattern TMl_M5_Large = TransformInstanceType' "ml.m5.large"

pattern TMl_M5_XLarge :: TransformInstanceType
pattern TMl_M5_XLarge = TransformInstanceType' "ml.m5.xlarge"

pattern TMl_M5_2XLarge :: TransformInstanceType
pattern TMl_M5_2XLarge = TransformInstanceType' "ml.m5.2xlarge"

pattern TMl_M5_4XLarge :: TransformInstanceType
pattern TMl_M5_4XLarge = TransformInstanceType' "ml.m5.4xlarge"

pattern TMl_M5_12XLarge :: TransformInstanceType
pattern TMl_M5_12XLarge = TransformInstanceType' "ml.m5.12xlarge"

pattern TMl_M5_24XLarge :: TransformInstanceType
pattern TMl_M5_24XLarge = TransformInstanceType' "ml.m5.24xlarge"

{-# COMPLETE
  TMl_M4_XLarge,
  TMl_M4_2XLarge,
  TMl_M4_4XLarge,
  TMl_M4_10XLarge,
  TMl_M4_16XLarge,
  TMl_C4_XLarge,
  TMl_C4_2XLarge,
  TMl_C4_4XLarge,
  TMl_C4_8XLarge,
  TMl_P2_XLarge,
  TMl_P2_8XLarge,
  TMl_P2_16XLarge,
  TMl_P3_2XLarge,
  TMl_P3_8XLarge,
  TMl_P3_16XLarge,
  TMl_C5_XLarge,
  TMl_C5_2XLarge,
  TMl_C5_4XLarge,
  TMl_C5_9XLarge,
  TMl_C5_18XLarge,
  TMl_M5_Large,
  TMl_M5_XLarge,
  TMl_M5_2XLarge,
  TMl_M5_4XLarge,
  TMl_M5_12XLarge,
  TMl_M5_24XLarge,
  TransformInstanceType'
  #-}

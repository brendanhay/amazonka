{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingInstanceType where

import Network.AWS.Prelude

data ProcessingInstanceType
  = PITMl_C4_2XLarge
  | PITMl_C4_4XLarge
  | PITMl_C4_8XLarge
  | PITMl_C4_XLarge
  | PITMl_C5_18XLarge
  | PITMl_C5_2XLarge
  | PITMl_C5_4XLarge
  | PITMl_C5_9XLarge
  | PITMl_C5_XLarge
  | PITMl_M4_10XLarge
  | PITMl_M4_16XLarge
  | PITMl_M4_2XLarge
  | PITMl_M4_4XLarge
  | PITMl_M4_XLarge
  | PITMl_M5_12XLarge
  | PITMl_M5_24XLarge
  | PITMl_M5_2XLarge
  | PITMl_M5_4XLarge
  | PITMl_M5_Large
  | PITMl_M5_XLarge
  | PITMl_P2_16XLarge
  | PITMl_P2_8XLarge
  | PITMl_P2_XLarge
  | PITMl_P3_16XLarge
  | PITMl_P3_2XLarge
  | PITMl_P3_8XLarge
  | PITMl_R5_12XLarge
  | PITMl_R5_16XLarge
  | PITMl_R5_24XLarge
  | PITMl_R5_2XLarge
  | PITMl_R5_4XLarge
  | PITMl_R5_8XLarge
  | PITMl_R5_Large
  | PITMl_R5_XLarge
  | PITMl_T3_2XLarge
  | PITMl_T3_Large
  | PITMl_T3_Medium
  | PITMl_T3_XLarge
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ProcessingInstanceType where
  parser =
    takeLowerText >>= \case
      "ml.c4.2xlarge" -> pure PITMl_C4_2XLarge
      "ml.c4.4xlarge" -> pure PITMl_C4_4XLarge
      "ml.c4.8xlarge" -> pure PITMl_C4_8XLarge
      "ml.c4.xlarge" -> pure PITMl_C4_XLarge
      "ml.c5.18xlarge" -> pure PITMl_C5_18XLarge
      "ml.c5.2xlarge" -> pure PITMl_C5_2XLarge
      "ml.c5.4xlarge" -> pure PITMl_C5_4XLarge
      "ml.c5.9xlarge" -> pure PITMl_C5_9XLarge
      "ml.c5.xlarge" -> pure PITMl_C5_XLarge
      "ml.m4.10xlarge" -> pure PITMl_M4_10XLarge
      "ml.m4.16xlarge" -> pure PITMl_M4_16XLarge
      "ml.m4.2xlarge" -> pure PITMl_M4_2XLarge
      "ml.m4.4xlarge" -> pure PITMl_M4_4XLarge
      "ml.m4.xlarge" -> pure PITMl_M4_XLarge
      "ml.m5.12xlarge" -> pure PITMl_M5_12XLarge
      "ml.m5.24xlarge" -> pure PITMl_M5_24XLarge
      "ml.m5.2xlarge" -> pure PITMl_M5_2XLarge
      "ml.m5.4xlarge" -> pure PITMl_M5_4XLarge
      "ml.m5.large" -> pure PITMl_M5_Large
      "ml.m5.xlarge" -> pure PITMl_M5_XLarge
      "ml.p2.16xlarge" -> pure PITMl_P2_16XLarge
      "ml.p2.8xlarge" -> pure PITMl_P2_8XLarge
      "ml.p2.xlarge" -> pure PITMl_P2_XLarge
      "ml.p3.16xlarge" -> pure PITMl_P3_16XLarge
      "ml.p3.2xlarge" -> pure PITMl_P3_2XLarge
      "ml.p3.8xlarge" -> pure PITMl_P3_8XLarge
      "ml.r5.12xlarge" -> pure PITMl_R5_12XLarge
      "ml.r5.16xlarge" -> pure PITMl_R5_16XLarge
      "ml.r5.24xlarge" -> pure PITMl_R5_24XLarge
      "ml.r5.2xlarge" -> pure PITMl_R5_2XLarge
      "ml.r5.4xlarge" -> pure PITMl_R5_4XLarge
      "ml.r5.8xlarge" -> pure PITMl_R5_8XLarge
      "ml.r5.large" -> pure PITMl_R5_Large
      "ml.r5.xlarge" -> pure PITMl_R5_XLarge
      "ml.t3.2xlarge" -> pure PITMl_T3_2XLarge
      "ml.t3.large" -> pure PITMl_T3_Large
      "ml.t3.medium" -> pure PITMl_T3_Medium
      "ml.t3.xlarge" -> pure PITMl_T3_XLarge
      e ->
        fromTextError $
          "Failure parsing ProcessingInstanceType from value: '" <> e
            <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.r5.12xlarge, ml.r5.16xlarge, ml.r5.24xlarge, ml.r5.2xlarge, ml.r5.4xlarge, ml.r5.8xlarge, ml.r5.large, ml.r5.xlarge, ml.t3.2xlarge, ml.t3.large, ml.t3.medium, ml.t3.xlarge"

instance ToText ProcessingInstanceType where
  toText = \case
    PITMl_C4_2XLarge -> "ml.c4.2xlarge"
    PITMl_C4_4XLarge -> "ml.c4.4xlarge"
    PITMl_C4_8XLarge -> "ml.c4.8xlarge"
    PITMl_C4_XLarge -> "ml.c4.xlarge"
    PITMl_C5_18XLarge -> "ml.c5.18xlarge"
    PITMl_C5_2XLarge -> "ml.c5.2xlarge"
    PITMl_C5_4XLarge -> "ml.c5.4xlarge"
    PITMl_C5_9XLarge -> "ml.c5.9xlarge"
    PITMl_C5_XLarge -> "ml.c5.xlarge"
    PITMl_M4_10XLarge -> "ml.m4.10xlarge"
    PITMl_M4_16XLarge -> "ml.m4.16xlarge"
    PITMl_M4_2XLarge -> "ml.m4.2xlarge"
    PITMl_M4_4XLarge -> "ml.m4.4xlarge"
    PITMl_M4_XLarge -> "ml.m4.xlarge"
    PITMl_M5_12XLarge -> "ml.m5.12xlarge"
    PITMl_M5_24XLarge -> "ml.m5.24xlarge"
    PITMl_M5_2XLarge -> "ml.m5.2xlarge"
    PITMl_M5_4XLarge -> "ml.m5.4xlarge"
    PITMl_M5_Large -> "ml.m5.large"
    PITMl_M5_XLarge -> "ml.m5.xlarge"
    PITMl_P2_16XLarge -> "ml.p2.16xlarge"
    PITMl_P2_8XLarge -> "ml.p2.8xlarge"
    PITMl_P2_XLarge -> "ml.p2.xlarge"
    PITMl_P3_16XLarge -> "ml.p3.16xlarge"
    PITMl_P3_2XLarge -> "ml.p3.2xlarge"
    PITMl_P3_8XLarge -> "ml.p3.8xlarge"
    PITMl_R5_12XLarge -> "ml.r5.12xlarge"
    PITMl_R5_16XLarge -> "ml.r5.16xlarge"
    PITMl_R5_24XLarge -> "ml.r5.24xlarge"
    PITMl_R5_2XLarge -> "ml.r5.2xlarge"
    PITMl_R5_4XLarge -> "ml.r5.4xlarge"
    PITMl_R5_8XLarge -> "ml.r5.8xlarge"
    PITMl_R5_Large -> "ml.r5.large"
    PITMl_R5_XLarge -> "ml.r5.xlarge"
    PITMl_T3_2XLarge -> "ml.t3.2xlarge"
    PITMl_T3_Large -> "ml.t3.large"
    PITMl_T3_Medium -> "ml.t3.medium"
    PITMl_T3_XLarge -> "ml.t3.xlarge"

instance Hashable ProcessingInstanceType

instance NFData ProcessingInstanceType

instance ToByteString ProcessingInstanceType

instance ToQuery ProcessingInstanceType

instance ToHeader ProcessingInstanceType

instance ToJSON ProcessingInstanceType where
  toJSON = toJSONText

instance FromJSON ProcessingInstanceType where
  parseJSON = parseJSONText "ProcessingInstanceType"

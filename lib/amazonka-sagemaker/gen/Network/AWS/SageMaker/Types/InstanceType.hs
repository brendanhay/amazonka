{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InstanceType where

import Network.AWS.Prelude

data InstanceType
  = ITMl_C4_2XLarge
  | ITMl_C4_4XLarge
  | ITMl_C4_8XLarge
  | ITMl_C4_XLarge
  | ITMl_C5_18XLarge
  | ITMl_C5_2XLarge
  | ITMl_C5_4XLarge
  | ITMl_C5_9XLarge
  | ITMl_C5_XLarge
  | ITMl_C5d_18XLarge
  | ITMl_C5d_2XLarge
  | ITMl_C5d_4XLarge
  | ITMl_C5d_9XLarge
  | ITMl_C5d_XLarge
  | ITMl_M4_10XLarge
  | ITMl_M4_16XLarge
  | ITMl_M4_2XLarge
  | ITMl_M4_4XLarge
  | ITMl_M4_XLarge
  | ITMl_M5_12XLarge
  | ITMl_M5_24XLarge
  | ITMl_M5_2XLarge
  | ITMl_M5_4XLarge
  | ITMl_M5_XLarge
  | ITMl_P2_16XLarge
  | ITMl_P2_8XLarge
  | ITMl_P2_XLarge
  | ITMl_P3_16XLarge
  | ITMl_P3_2XLarge
  | ITMl_P3_8XLarge
  | ITMl_T2_2XLarge
  | ITMl_T2_Large
  | ITMl_T2_Medium
  | ITMl_T2_XLarge
  | ITMl_T3_2XLarge
  | ITMl_T3_Large
  | ITMl_T3_Medium
  | ITMl_T3_XLarge
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

instance FromText InstanceType where
  parser =
    takeLowerText >>= \case
      "ml.c4.2xlarge" -> pure ITMl_C4_2XLarge
      "ml.c4.4xlarge" -> pure ITMl_C4_4XLarge
      "ml.c4.8xlarge" -> pure ITMl_C4_8XLarge
      "ml.c4.xlarge" -> pure ITMl_C4_XLarge
      "ml.c5.18xlarge" -> pure ITMl_C5_18XLarge
      "ml.c5.2xlarge" -> pure ITMl_C5_2XLarge
      "ml.c5.4xlarge" -> pure ITMl_C5_4XLarge
      "ml.c5.9xlarge" -> pure ITMl_C5_9XLarge
      "ml.c5.xlarge" -> pure ITMl_C5_XLarge
      "ml.c5d.18xlarge" -> pure ITMl_C5d_18XLarge
      "ml.c5d.2xlarge" -> pure ITMl_C5d_2XLarge
      "ml.c5d.4xlarge" -> pure ITMl_C5d_4XLarge
      "ml.c5d.9xlarge" -> pure ITMl_C5d_9XLarge
      "ml.c5d.xlarge" -> pure ITMl_C5d_XLarge
      "ml.m4.10xlarge" -> pure ITMl_M4_10XLarge
      "ml.m4.16xlarge" -> pure ITMl_M4_16XLarge
      "ml.m4.2xlarge" -> pure ITMl_M4_2XLarge
      "ml.m4.4xlarge" -> pure ITMl_M4_4XLarge
      "ml.m4.xlarge" -> pure ITMl_M4_XLarge
      "ml.m5.12xlarge" -> pure ITMl_M5_12XLarge
      "ml.m5.24xlarge" -> pure ITMl_M5_24XLarge
      "ml.m5.2xlarge" -> pure ITMl_M5_2XLarge
      "ml.m5.4xlarge" -> pure ITMl_M5_4XLarge
      "ml.m5.xlarge" -> pure ITMl_M5_XLarge
      "ml.p2.16xlarge" -> pure ITMl_P2_16XLarge
      "ml.p2.8xlarge" -> pure ITMl_P2_8XLarge
      "ml.p2.xlarge" -> pure ITMl_P2_XLarge
      "ml.p3.16xlarge" -> pure ITMl_P3_16XLarge
      "ml.p3.2xlarge" -> pure ITMl_P3_2XLarge
      "ml.p3.8xlarge" -> pure ITMl_P3_8XLarge
      "ml.t2.2xlarge" -> pure ITMl_T2_2XLarge
      "ml.t2.large" -> pure ITMl_T2_Large
      "ml.t2.medium" -> pure ITMl_T2_Medium
      "ml.t2.xlarge" -> pure ITMl_T2_XLarge
      "ml.t3.2xlarge" -> pure ITMl_T3_2XLarge
      "ml.t3.large" -> pure ITMl_T3_Large
      "ml.t3.medium" -> pure ITMl_T3_Medium
      "ml.t3.xlarge" -> pure ITMl_T3_XLarge
      e ->
        fromTextError $
          "Failure parsing InstanceType from value: '" <> e
            <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.c5d.18xlarge, ml.c5d.2xlarge, ml.c5d.4xlarge, ml.c5d.9xlarge, ml.c5d.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.t2.2xlarge, ml.t2.large, ml.t2.medium, ml.t2.xlarge, ml.t3.2xlarge, ml.t3.large, ml.t3.medium, ml.t3.xlarge"

instance ToText InstanceType where
  toText = \case
    ITMl_C4_2XLarge -> "ml.c4.2xlarge"
    ITMl_C4_4XLarge -> "ml.c4.4xlarge"
    ITMl_C4_8XLarge -> "ml.c4.8xlarge"
    ITMl_C4_XLarge -> "ml.c4.xlarge"
    ITMl_C5_18XLarge -> "ml.c5.18xlarge"
    ITMl_C5_2XLarge -> "ml.c5.2xlarge"
    ITMl_C5_4XLarge -> "ml.c5.4xlarge"
    ITMl_C5_9XLarge -> "ml.c5.9xlarge"
    ITMl_C5_XLarge -> "ml.c5.xlarge"
    ITMl_C5d_18XLarge -> "ml.c5d.18xlarge"
    ITMl_C5d_2XLarge -> "ml.c5d.2xlarge"
    ITMl_C5d_4XLarge -> "ml.c5d.4xlarge"
    ITMl_C5d_9XLarge -> "ml.c5d.9xlarge"
    ITMl_C5d_XLarge -> "ml.c5d.xlarge"
    ITMl_M4_10XLarge -> "ml.m4.10xlarge"
    ITMl_M4_16XLarge -> "ml.m4.16xlarge"
    ITMl_M4_2XLarge -> "ml.m4.2xlarge"
    ITMl_M4_4XLarge -> "ml.m4.4xlarge"
    ITMl_M4_XLarge -> "ml.m4.xlarge"
    ITMl_M5_12XLarge -> "ml.m5.12xlarge"
    ITMl_M5_24XLarge -> "ml.m5.24xlarge"
    ITMl_M5_2XLarge -> "ml.m5.2xlarge"
    ITMl_M5_4XLarge -> "ml.m5.4xlarge"
    ITMl_M5_XLarge -> "ml.m5.xlarge"
    ITMl_P2_16XLarge -> "ml.p2.16xlarge"
    ITMl_P2_8XLarge -> "ml.p2.8xlarge"
    ITMl_P2_XLarge -> "ml.p2.xlarge"
    ITMl_P3_16XLarge -> "ml.p3.16xlarge"
    ITMl_P3_2XLarge -> "ml.p3.2xlarge"
    ITMl_P3_8XLarge -> "ml.p3.8xlarge"
    ITMl_T2_2XLarge -> "ml.t2.2xlarge"
    ITMl_T2_Large -> "ml.t2.large"
    ITMl_T2_Medium -> "ml.t2.medium"
    ITMl_T2_XLarge -> "ml.t2.xlarge"
    ITMl_T3_2XLarge -> "ml.t3.2xlarge"
    ITMl_T3_Large -> "ml.t3.large"
    ITMl_T3_Medium -> "ml.t3.medium"
    ITMl_T3_XLarge -> "ml.t3.xlarge"

instance Hashable InstanceType

instance NFData InstanceType

instance ToByteString InstanceType

instance ToQuery InstanceType

instance ToHeader InstanceType

instance ToJSON InstanceType where
  toJSON = toJSONText

instance FromJSON InstanceType where
  parseJSON = parseJSONText "InstanceType"

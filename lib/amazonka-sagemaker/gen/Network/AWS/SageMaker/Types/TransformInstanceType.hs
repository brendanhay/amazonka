{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformInstanceType where

import Network.AWS.Prelude

data TransformInstanceType
  = TMl_C4_2XLarge
  | TMl_C4_4XLarge
  | TMl_C4_8XLarge
  | TMl_C4_XLarge
  | TMl_C5_18XLarge
  | TMl_C5_2XLarge
  | TMl_C5_4XLarge
  | TMl_C5_9XLarge
  | TMl_C5_XLarge
  | TMl_M4_10XLarge
  | TMl_M4_16XLarge
  | TMl_M4_2XLarge
  | TMl_M4_4XLarge
  | TMl_M4_XLarge
  | TMl_M5_12XLarge
  | TMl_M5_24XLarge
  | TMl_M5_2XLarge
  | TMl_M5_4XLarge
  | TMl_M5_Large
  | TMl_M5_XLarge
  | TMl_P2_16XLarge
  | TMl_P2_8XLarge
  | TMl_P2_XLarge
  | TMl_P3_16XLarge
  | TMl_P3_2XLarge
  | TMl_P3_8XLarge
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

instance FromText TransformInstanceType where
  parser =
    takeLowerText >>= \case
      "ml.c4.2xlarge" -> pure TMl_C4_2XLarge
      "ml.c4.4xlarge" -> pure TMl_C4_4XLarge
      "ml.c4.8xlarge" -> pure TMl_C4_8XLarge
      "ml.c4.xlarge" -> pure TMl_C4_XLarge
      "ml.c5.18xlarge" -> pure TMl_C5_18XLarge
      "ml.c5.2xlarge" -> pure TMl_C5_2XLarge
      "ml.c5.4xlarge" -> pure TMl_C5_4XLarge
      "ml.c5.9xlarge" -> pure TMl_C5_9XLarge
      "ml.c5.xlarge" -> pure TMl_C5_XLarge
      "ml.m4.10xlarge" -> pure TMl_M4_10XLarge
      "ml.m4.16xlarge" -> pure TMl_M4_16XLarge
      "ml.m4.2xlarge" -> pure TMl_M4_2XLarge
      "ml.m4.4xlarge" -> pure TMl_M4_4XLarge
      "ml.m4.xlarge" -> pure TMl_M4_XLarge
      "ml.m5.12xlarge" -> pure TMl_M5_12XLarge
      "ml.m5.24xlarge" -> pure TMl_M5_24XLarge
      "ml.m5.2xlarge" -> pure TMl_M5_2XLarge
      "ml.m5.4xlarge" -> pure TMl_M5_4XLarge
      "ml.m5.large" -> pure TMl_M5_Large
      "ml.m5.xlarge" -> pure TMl_M5_XLarge
      "ml.p2.16xlarge" -> pure TMl_P2_16XLarge
      "ml.p2.8xlarge" -> pure TMl_P2_8XLarge
      "ml.p2.xlarge" -> pure TMl_P2_XLarge
      "ml.p3.16xlarge" -> pure TMl_P3_16XLarge
      "ml.p3.2xlarge" -> pure TMl_P3_2XLarge
      "ml.p3.8xlarge" -> pure TMl_P3_8XLarge
      e ->
        fromTextError $
          "Failure parsing TransformInstanceType from value: '" <> e
            <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge"

instance ToText TransformInstanceType where
  toText = \case
    TMl_C4_2XLarge -> "ml.c4.2xlarge"
    TMl_C4_4XLarge -> "ml.c4.4xlarge"
    TMl_C4_8XLarge -> "ml.c4.8xlarge"
    TMl_C4_XLarge -> "ml.c4.xlarge"
    TMl_C5_18XLarge -> "ml.c5.18xlarge"
    TMl_C5_2XLarge -> "ml.c5.2xlarge"
    TMl_C5_4XLarge -> "ml.c5.4xlarge"
    TMl_C5_9XLarge -> "ml.c5.9xlarge"
    TMl_C5_XLarge -> "ml.c5.xlarge"
    TMl_M4_10XLarge -> "ml.m4.10xlarge"
    TMl_M4_16XLarge -> "ml.m4.16xlarge"
    TMl_M4_2XLarge -> "ml.m4.2xlarge"
    TMl_M4_4XLarge -> "ml.m4.4xlarge"
    TMl_M4_XLarge -> "ml.m4.xlarge"
    TMl_M5_12XLarge -> "ml.m5.12xlarge"
    TMl_M5_24XLarge -> "ml.m5.24xlarge"
    TMl_M5_2XLarge -> "ml.m5.2xlarge"
    TMl_M5_4XLarge -> "ml.m5.4xlarge"
    TMl_M5_Large -> "ml.m5.large"
    TMl_M5_XLarge -> "ml.m5.xlarge"
    TMl_P2_16XLarge -> "ml.p2.16xlarge"
    TMl_P2_8XLarge -> "ml.p2.8xlarge"
    TMl_P2_XLarge -> "ml.p2.xlarge"
    TMl_P3_16XLarge -> "ml.p3.16xlarge"
    TMl_P3_2XLarge -> "ml.p3.2xlarge"
    TMl_P3_8XLarge -> "ml.p3.8xlarge"

instance Hashable TransformInstanceType

instance NFData TransformInstanceType

instance ToByteString TransformInstanceType

instance ToQuery TransformInstanceType

instance ToHeader TransformInstanceType

instance ToJSON TransformInstanceType where
  toJSON = toJSONText

instance FromJSON TransformInstanceType where
  parseJSON = parseJSONText "TransformInstanceType"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceType where

import Network.AWS.Prelude

data EC2InstanceType
  = EITC3_2XLarge
  | EITC3_4XLarge
  | EITC3_8XLarge
  | EITC3_Large
  | EITC3_XLarge
  | EITC4_2XLarge
  | EITC4_4XLarge
  | EITC4_8XLarge
  | EITC4_Large
  | EITC4_XLarge
  | EITC5_12XLarge
  | EITC5_18XLarge
  | EITC5_24XLarge
  | EITC5_2XLarge
  | EITC5_4XLarge
  | EITC5_9XLarge
  | EITC5_Large
  | EITC5_XLarge
  | EITC5a_12XLarge
  | EITC5a_16XLarge
  | EITC5a_24XLarge
  | EITC5a_2XLarge
  | EITC5a_4XLarge
  | EITC5a_8XLarge
  | EITC5a_Large
  | EITC5a_XLarge
  | EITM3_2XLarge
  | EITM3_Large
  | EITM3_Medium
  | EITM3_XLarge
  | EITM4_10XLarge
  | EITM4_2XLarge
  | EITM4_4XLarge
  | EITM4_Large
  | EITM4_XLarge
  | EITM5_12XLarge
  | EITM5_16XLarge
  | EITM5_24XLarge
  | EITM5_2XLarge
  | EITM5_4XLarge
  | EITM5_8XLarge
  | EITM5_Large
  | EITM5_XLarge
  | EITM5a_12XLarge
  | EITM5a_16XLarge
  | EITM5a_24XLarge
  | EITM5a_2XLarge
  | EITM5a_4XLarge
  | EITM5a_8XLarge
  | EITM5a_Large
  | EITM5a_XLarge
  | EITR3_2XLarge
  | EITR3_4XLarge
  | EITR3_8XLarge
  | EITR3_Large
  | EITR3_XLarge
  | EITR4_16XLarge
  | EITR4_2XLarge
  | EITR4_4XLarge
  | EITR4_8XLarge
  | EITR4_Large
  | EITR4_XLarge
  | EITR5_12XLarge
  | EITR5_16XLarge
  | EITR5_24XLarge
  | EITR5_2XLarge
  | EITR5_4XLarge
  | EITR5_8XLarge
  | EITR5_Large
  | EITR5_XLarge
  | EITR5a_12XLarge
  | EITR5a_16XLarge
  | EITR5a_24XLarge
  | EITR5a_2XLarge
  | EITR5a_4XLarge
  | EITR5a_8XLarge
  | EITR5a_Large
  | EITR5a_XLarge
  | EITT2_Large
  | EITT2_Medium
  | EITT2_Micro
  | EITT2_Small
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

instance FromText EC2InstanceType where
  parser =
    takeLowerText >>= \case
      "c3.2xlarge" -> pure EITC3_2XLarge
      "c3.4xlarge" -> pure EITC3_4XLarge
      "c3.8xlarge" -> pure EITC3_8XLarge
      "c3.large" -> pure EITC3_Large
      "c3.xlarge" -> pure EITC3_XLarge
      "c4.2xlarge" -> pure EITC4_2XLarge
      "c4.4xlarge" -> pure EITC4_4XLarge
      "c4.8xlarge" -> pure EITC4_8XLarge
      "c4.large" -> pure EITC4_Large
      "c4.xlarge" -> pure EITC4_XLarge
      "c5.12xlarge" -> pure EITC5_12XLarge
      "c5.18xlarge" -> pure EITC5_18XLarge
      "c5.24xlarge" -> pure EITC5_24XLarge
      "c5.2xlarge" -> pure EITC5_2XLarge
      "c5.4xlarge" -> pure EITC5_4XLarge
      "c5.9xlarge" -> pure EITC5_9XLarge
      "c5.large" -> pure EITC5_Large
      "c5.xlarge" -> pure EITC5_XLarge
      "c5a.12xlarge" -> pure EITC5a_12XLarge
      "c5a.16xlarge" -> pure EITC5a_16XLarge
      "c5a.24xlarge" -> pure EITC5a_24XLarge
      "c5a.2xlarge" -> pure EITC5a_2XLarge
      "c5a.4xlarge" -> pure EITC5a_4XLarge
      "c5a.8xlarge" -> pure EITC5a_8XLarge
      "c5a.large" -> pure EITC5a_Large
      "c5a.xlarge" -> pure EITC5a_XLarge
      "m3.2xlarge" -> pure EITM3_2XLarge
      "m3.large" -> pure EITM3_Large
      "m3.medium" -> pure EITM3_Medium
      "m3.xlarge" -> pure EITM3_XLarge
      "m4.10xlarge" -> pure EITM4_10XLarge
      "m4.2xlarge" -> pure EITM4_2XLarge
      "m4.4xlarge" -> pure EITM4_4XLarge
      "m4.large" -> pure EITM4_Large
      "m4.xlarge" -> pure EITM4_XLarge
      "m5.12xlarge" -> pure EITM5_12XLarge
      "m5.16xlarge" -> pure EITM5_16XLarge
      "m5.24xlarge" -> pure EITM5_24XLarge
      "m5.2xlarge" -> pure EITM5_2XLarge
      "m5.4xlarge" -> pure EITM5_4XLarge
      "m5.8xlarge" -> pure EITM5_8XLarge
      "m5.large" -> pure EITM5_Large
      "m5.xlarge" -> pure EITM5_XLarge
      "m5a.12xlarge" -> pure EITM5a_12XLarge
      "m5a.16xlarge" -> pure EITM5a_16XLarge
      "m5a.24xlarge" -> pure EITM5a_24XLarge
      "m5a.2xlarge" -> pure EITM5a_2XLarge
      "m5a.4xlarge" -> pure EITM5a_4XLarge
      "m5a.8xlarge" -> pure EITM5a_8XLarge
      "m5a.large" -> pure EITM5a_Large
      "m5a.xlarge" -> pure EITM5a_XLarge
      "r3.2xlarge" -> pure EITR3_2XLarge
      "r3.4xlarge" -> pure EITR3_4XLarge
      "r3.8xlarge" -> pure EITR3_8XLarge
      "r3.large" -> pure EITR3_Large
      "r3.xlarge" -> pure EITR3_XLarge
      "r4.16xlarge" -> pure EITR4_16XLarge
      "r4.2xlarge" -> pure EITR4_2XLarge
      "r4.4xlarge" -> pure EITR4_4XLarge
      "r4.8xlarge" -> pure EITR4_8XLarge
      "r4.large" -> pure EITR4_Large
      "r4.xlarge" -> pure EITR4_XLarge
      "r5.12xlarge" -> pure EITR5_12XLarge
      "r5.16xlarge" -> pure EITR5_16XLarge
      "r5.24xlarge" -> pure EITR5_24XLarge
      "r5.2xlarge" -> pure EITR5_2XLarge
      "r5.4xlarge" -> pure EITR5_4XLarge
      "r5.8xlarge" -> pure EITR5_8XLarge
      "r5.large" -> pure EITR5_Large
      "r5.xlarge" -> pure EITR5_XLarge
      "r5a.12xlarge" -> pure EITR5a_12XLarge
      "r5a.16xlarge" -> pure EITR5a_16XLarge
      "r5a.24xlarge" -> pure EITR5a_24XLarge
      "r5a.2xlarge" -> pure EITR5a_2XLarge
      "r5a.4xlarge" -> pure EITR5a_4XLarge
      "r5a.8xlarge" -> pure EITR5a_8XLarge
      "r5a.large" -> pure EITR5a_Large
      "r5a.xlarge" -> pure EITR5a_XLarge
      "t2.large" -> pure EITT2_Large
      "t2.medium" -> pure EITT2_Medium
      "t2.micro" -> pure EITT2_Micro
      "t2.small" -> pure EITT2_Small
      e ->
        fromTextError $
          "Failure parsing EC2InstanceType from value: '" <> e
            <> "'. Accepted values: c3.2xlarge, c3.4xlarge, c3.8xlarge, c3.large, c3.xlarge, c4.2xlarge, c4.4xlarge, c4.8xlarge, c4.large, c4.xlarge, c5.12xlarge, c5.18xlarge, c5.24xlarge, c5.2xlarge, c5.4xlarge, c5.9xlarge, c5.large, c5.xlarge, c5a.12xlarge, c5a.16xlarge, c5a.24xlarge, c5a.2xlarge, c5a.4xlarge, c5a.8xlarge, c5a.large, c5a.xlarge, m3.2xlarge, m3.large, m3.medium, m3.xlarge, m4.10xlarge, m4.2xlarge, m4.4xlarge, m4.large, m4.xlarge, m5.12xlarge, m5.16xlarge, m5.24xlarge, m5.2xlarge, m5.4xlarge, m5.8xlarge, m5.large, m5.xlarge, m5a.12xlarge, m5a.16xlarge, m5a.24xlarge, m5a.2xlarge, m5a.4xlarge, m5a.8xlarge, m5a.large, m5a.xlarge, r3.2xlarge, r3.4xlarge, r3.8xlarge, r3.large, r3.xlarge, r4.16xlarge, r4.2xlarge, r4.4xlarge, r4.8xlarge, r4.large, r4.xlarge, r5.12xlarge, r5.16xlarge, r5.24xlarge, r5.2xlarge, r5.4xlarge, r5.8xlarge, r5.large, r5.xlarge, r5a.12xlarge, r5a.16xlarge, r5a.24xlarge, r5a.2xlarge, r5a.4xlarge, r5a.8xlarge, r5a.large, r5a.xlarge, t2.large, t2.medium, t2.micro, t2.small"

instance ToText EC2InstanceType where
  toText = \case
    EITC3_2XLarge -> "c3.2xlarge"
    EITC3_4XLarge -> "c3.4xlarge"
    EITC3_8XLarge -> "c3.8xlarge"
    EITC3_Large -> "c3.large"
    EITC3_XLarge -> "c3.xlarge"
    EITC4_2XLarge -> "c4.2xlarge"
    EITC4_4XLarge -> "c4.4xlarge"
    EITC4_8XLarge -> "c4.8xlarge"
    EITC4_Large -> "c4.large"
    EITC4_XLarge -> "c4.xlarge"
    EITC5_12XLarge -> "c5.12xlarge"
    EITC5_18XLarge -> "c5.18xlarge"
    EITC5_24XLarge -> "c5.24xlarge"
    EITC5_2XLarge -> "c5.2xlarge"
    EITC5_4XLarge -> "c5.4xlarge"
    EITC5_9XLarge -> "c5.9xlarge"
    EITC5_Large -> "c5.large"
    EITC5_XLarge -> "c5.xlarge"
    EITC5a_12XLarge -> "c5a.12xlarge"
    EITC5a_16XLarge -> "c5a.16xlarge"
    EITC5a_24XLarge -> "c5a.24xlarge"
    EITC5a_2XLarge -> "c5a.2xlarge"
    EITC5a_4XLarge -> "c5a.4xlarge"
    EITC5a_8XLarge -> "c5a.8xlarge"
    EITC5a_Large -> "c5a.large"
    EITC5a_XLarge -> "c5a.xlarge"
    EITM3_2XLarge -> "m3.2xlarge"
    EITM3_Large -> "m3.large"
    EITM3_Medium -> "m3.medium"
    EITM3_XLarge -> "m3.xlarge"
    EITM4_10XLarge -> "m4.10xlarge"
    EITM4_2XLarge -> "m4.2xlarge"
    EITM4_4XLarge -> "m4.4xlarge"
    EITM4_Large -> "m4.large"
    EITM4_XLarge -> "m4.xlarge"
    EITM5_12XLarge -> "m5.12xlarge"
    EITM5_16XLarge -> "m5.16xlarge"
    EITM5_24XLarge -> "m5.24xlarge"
    EITM5_2XLarge -> "m5.2xlarge"
    EITM5_4XLarge -> "m5.4xlarge"
    EITM5_8XLarge -> "m5.8xlarge"
    EITM5_Large -> "m5.large"
    EITM5_XLarge -> "m5.xlarge"
    EITM5a_12XLarge -> "m5a.12xlarge"
    EITM5a_16XLarge -> "m5a.16xlarge"
    EITM5a_24XLarge -> "m5a.24xlarge"
    EITM5a_2XLarge -> "m5a.2xlarge"
    EITM5a_4XLarge -> "m5a.4xlarge"
    EITM5a_8XLarge -> "m5a.8xlarge"
    EITM5a_Large -> "m5a.large"
    EITM5a_XLarge -> "m5a.xlarge"
    EITR3_2XLarge -> "r3.2xlarge"
    EITR3_4XLarge -> "r3.4xlarge"
    EITR3_8XLarge -> "r3.8xlarge"
    EITR3_Large -> "r3.large"
    EITR3_XLarge -> "r3.xlarge"
    EITR4_16XLarge -> "r4.16xlarge"
    EITR4_2XLarge -> "r4.2xlarge"
    EITR4_4XLarge -> "r4.4xlarge"
    EITR4_8XLarge -> "r4.8xlarge"
    EITR4_Large -> "r4.large"
    EITR4_XLarge -> "r4.xlarge"
    EITR5_12XLarge -> "r5.12xlarge"
    EITR5_16XLarge -> "r5.16xlarge"
    EITR5_24XLarge -> "r5.24xlarge"
    EITR5_2XLarge -> "r5.2xlarge"
    EITR5_4XLarge -> "r5.4xlarge"
    EITR5_8XLarge -> "r5.8xlarge"
    EITR5_Large -> "r5.large"
    EITR5_XLarge -> "r5.xlarge"
    EITR5a_12XLarge -> "r5a.12xlarge"
    EITR5a_16XLarge -> "r5a.16xlarge"
    EITR5a_24XLarge -> "r5a.24xlarge"
    EITR5a_2XLarge -> "r5a.2xlarge"
    EITR5a_4XLarge -> "r5a.4xlarge"
    EITR5a_8XLarge -> "r5a.8xlarge"
    EITR5a_Large -> "r5a.large"
    EITR5a_XLarge -> "r5a.xlarge"
    EITT2_Large -> "t2.large"
    EITT2_Medium -> "t2.medium"
    EITT2_Micro -> "t2.micro"
    EITT2_Small -> "t2.small"

instance Hashable EC2InstanceType

instance NFData EC2InstanceType

instance ToByteString EC2InstanceType

instance ToQuery EC2InstanceType

instance ToHeader EC2InstanceType

instance ToJSON EC2InstanceType where
  toJSON = toJSONText

instance FromJSON EC2InstanceType where
  parseJSON = parseJSONText "EC2InstanceType"

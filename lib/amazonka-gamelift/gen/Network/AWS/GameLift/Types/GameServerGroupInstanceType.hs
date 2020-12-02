{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupInstanceType where

import Network.AWS.Prelude

data GameServerGroupInstanceType
  = C4_2XLarge
  | C4_4XLarge
  | C4_8XLarge
  | C4_Large
  | C4_XLarge
  | C5_12XLarge
  | C5_18XLarge
  | C5_24XLarge
  | C5_2XLarge
  | C5_4XLarge
  | C5_9XLarge
  | C5_Large
  | C5_XLarge
  | M4_10XLarge
  | M4_2XLarge
  | M4_4XLarge
  | M4_Large
  | M4_XLarge
  | M5_12XLarge
  | M5_16XLarge
  | M5_24XLarge
  | M5_2XLarge
  | M5_4XLarge
  | M5_8XLarge
  | M5_Large
  | M5_XLarge
  | R4_16XLarge
  | R4_2XLarge
  | R4_4XLarge
  | R4_8XLarge
  | R4_Large
  | R4_XLarge
  | R5_12XLarge
  | R5_16XLarge
  | R5_24XLarge
  | R5_2XLarge
  | R5_4XLarge
  | R5_8XLarge
  | R5_Large
  | R5_XLarge
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

instance FromText GameServerGroupInstanceType where
  parser =
    takeLowerText >>= \case
      "c4.2xlarge" -> pure C4_2XLarge
      "c4.4xlarge" -> pure C4_4XLarge
      "c4.8xlarge" -> pure C4_8XLarge
      "c4.large" -> pure C4_Large
      "c4.xlarge" -> pure C4_XLarge
      "c5.12xlarge" -> pure C5_12XLarge
      "c5.18xlarge" -> pure C5_18XLarge
      "c5.24xlarge" -> pure C5_24XLarge
      "c5.2xlarge" -> pure C5_2XLarge
      "c5.4xlarge" -> pure C5_4XLarge
      "c5.9xlarge" -> pure C5_9XLarge
      "c5.large" -> pure C5_Large
      "c5.xlarge" -> pure C5_XLarge
      "m4.10xlarge" -> pure M4_10XLarge
      "m4.2xlarge" -> pure M4_2XLarge
      "m4.4xlarge" -> pure M4_4XLarge
      "m4.large" -> pure M4_Large
      "m4.xlarge" -> pure M4_XLarge
      "m5.12xlarge" -> pure M5_12XLarge
      "m5.16xlarge" -> pure M5_16XLarge
      "m5.24xlarge" -> pure M5_24XLarge
      "m5.2xlarge" -> pure M5_2XLarge
      "m5.4xlarge" -> pure M5_4XLarge
      "m5.8xlarge" -> pure M5_8XLarge
      "m5.large" -> pure M5_Large
      "m5.xlarge" -> pure M5_XLarge
      "r4.16xlarge" -> pure R4_16XLarge
      "r4.2xlarge" -> pure R4_2XLarge
      "r4.4xlarge" -> pure R4_4XLarge
      "r4.8xlarge" -> pure R4_8XLarge
      "r4.large" -> pure R4_Large
      "r4.xlarge" -> pure R4_XLarge
      "r5.12xlarge" -> pure R5_12XLarge
      "r5.16xlarge" -> pure R5_16XLarge
      "r5.24xlarge" -> pure R5_24XLarge
      "r5.2xlarge" -> pure R5_2XLarge
      "r5.4xlarge" -> pure R5_4XLarge
      "r5.8xlarge" -> pure R5_8XLarge
      "r5.large" -> pure R5_Large
      "r5.xlarge" -> pure R5_XLarge
      e ->
        fromTextError $
          "Failure parsing GameServerGroupInstanceType from value: '" <> e
            <> "'. Accepted values: c4.2xlarge, c4.4xlarge, c4.8xlarge, c4.large, c4.xlarge, c5.12xlarge, c5.18xlarge, c5.24xlarge, c5.2xlarge, c5.4xlarge, c5.9xlarge, c5.large, c5.xlarge, m4.10xlarge, m4.2xlarge, m4.4xlarge, m4.large, m4.xlarge, m5.12xlarge, m5.16xlarge, m5.24xlarge, m5.2xlarge, m5.4xlarge, m5.8xlarge, m5.large, m5.xlarge, r4.16xlarge, r4.2xlarge, r4.4xlarge, r4.8xlarge, r4.large, r4.xlarge, r5.12xlarge, r5.16xlarge, r5.24xlarge, r5.2xlarge, r5.4xlarge, r5.8xlarge, r5.large, r5.xlarge"

instance ToText GameServerGroupInstanceType where
  toText = \case
    C4_2XLarge -> "c4.2xlarge"
    C4_4XLarge -> "c4.4xlarge"
    C4_8XLarge -> "c4.8xlarge"
    C4_Large -> "c4.large"
    C4_XLarge -> "c4.xlarge"
    C5_12XLarge -> "c5.12xlarge"
    C5_18XLarge -> "c5.18xlarge"
    C5_24XLarge -> "c5.24xlarge"
    C5_2XLarge -> "c5.2xlarge"
    C5_4XLarge -> "c5.4xlarge"
    C5_9XLarge -> "c5.9xlarge"
    C5_Large -> "c5.large"
    C5_XLarge -> "c5.xlarge"
    M4_10XLarge -> "m4.10xlarge"
    M4_2XLarge -> "m4.2xlarge"
    M4_4XLarge -> "m4.4xlarge"
    M4_Large -> "m4.large"
    M4_XLarge -> "m4.xlarge"
    M5_12XLarge -> "m5.12xlarge"
    M5_16XLarge -> "m5.16xlarge"
    M5_24XLarge -> "m5.24xlarge"
    M5_2XLarge -> "m5.2xlarge"
    M5_4XLarge -> "m5.4xlarge"
    M5_8XLarge -> "m5.8xlarge"
    M5_Large -> "m5.large"
    M5_XLarge -> "m5.xlarge"
    R4_16XLarge -> "r4.16xlarge"
    R4_2XLarge -> "r4.2xlarge"
    R4_4XLarge -> "r4.4xlarge"
    R4_8XLarge -> "r4.8xlarge"
    R4_Large -> "r4.large"
    R4_XLarge -> "r4.xlarge"
    R5_12XLarge -> "r5.12xlarge"
    R5_16XLarge -> "r5.16xlarge"
    R5_24XLarge -> "r5.24xlarge"
    R5_2XLarge -> "r5.2xlarge"
    R5_4XLarge -> "r5.4xlarge"
    R5_8XLarge -> "r5.8xlarge"
    R5_Large -> "r5.large"
    R5_XLarge -> "r5.xlarge"

instance Hashable GameServerGroupInstanceType

instance NFData GameServerGroupInstanceType

instance ToByteString GameServerGroupInstanceType

instance ToQuery GameServerGroupInstanceType

instance ToHeader GameServerGroupInstanceType

instance ToJSON GameServerGroupInstanceType where
  toJSON = toJSONText

instance FromJSON GameServerGroupInstanceType where
  parseJSON = parseJSONText "GameServerGroupInstanceType"

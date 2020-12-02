{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.LaunchType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.LaunchType where

import Network.AWS.Prelude

data LaunchType
  = EC2
  | Fargate
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

instance FromText LaunchType where
  parser =
    takeLowerText >>= \case
      "ec2" -> pure EC2
      "fargate" -> pure Fargate
      e ->
        fromTextError $
          "Failure parsing LaunchType from value: '" <> e
            <> "'. Accepted values: ec2, fargate"

instance ToText LaunchType where
  toText = \case
    EC2 -> "EC2"
    Fargate -> "FARGATE"

instance Hashable LaunchType

instance NFData LaunchType

instance ToByteString LaunchType

instance ToQuery LaunchType

instance ToHeader LaunchType

instance ToJSON LaunchType where
  toJSON = toJSONText

instance FromJSON LaunchType where
  parseJSON = parseJSONText "LaunchType"

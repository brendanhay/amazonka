{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DestinationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DestinationType where

import Network.AWS.Prelude

data DestinationType = S3
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

instance FromText DestinationType where
  parser =
    takeLowerText >>= \case
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing DestinationType from value: '" <> e
            <> "'. Accepted values: s3"

instance ToText DestinationType where
  toText = \case
    S3 -> "S3"

instance Hashable DestinationType

instance NFData DestinationType

instance ToByteString DestinationType

instance ToQuery DestinationType

instance ToHeader DestinationType

instance ToJSON DestinationType where
  toJSON = toJSONText

instance FromJSON DestinationType where
  parseJSON = parseJSONText "DestinationType"

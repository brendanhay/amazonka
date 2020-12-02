{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ConsumerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ConsumerStatus where

import Network.AWS.Prelude

data ConsumerStatus
  = Active
  | Creating
  | Deleting
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

instance FromText ConsumerStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      e ->
        fromTextError $
          "Failure parsing ConsumerStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting"

instance ToText ConsumerStatus where
  toText = \case
    Active -> "ACTIVE"
    Creating -> "CREATING"
    Deleting -> "DELETING"

instance Hashable ConsumerStatus

instance NFData ConsumerStatus

instance ToByteString ConsumerStatus

instance ToQuery ConsumerStatus

instance ToHeader ConsumerStatus

instance FromJSON ConsumerStatus where
  parseJSON = parseJSONText "ConsumerStatus"

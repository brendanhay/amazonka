{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.StreamStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.StreamStatus where

import Network.AWS.Prelude

data StreamStatus
  = Active
  | Creating
  | Deleting
  | Updating
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

instance FromText StreamStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing StreamStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, updating"

instance ToText StreamStatus where
  toText = \case
    Active -> "ACTIVE"
    Creating -> "CREATING"
    Deleting -> "DELETING"
    Updating -> "UPDATING"

instance Hashable StreamStatus

instance NFData StreamStatus

instance ToByteString StreamStatus

instance ToQuery StreamStatus

instance ToHeader StreamStatus

instance FromJSON StreamStatus where
  parseJSON = parseJSONText "StreamStatus"

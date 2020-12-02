{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshotState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceSnapshotState where

import Network.AWS.Prelude

data InstanceSnapshotState
  = ISSAvailable
  | ISSError'
  | ISSPending
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

instance FromText InstanceSnapshotState where
  parser =
    takeLowerText >>= \case
      "available" -> pure ISSAvailable
      "error" -> pure ISSError'
      "pending" -> pure ISSPending
      e ->
        fromTextError $
          "Failure parsing InstanceSnapshotState from value: '" <> e
            <> "'. Accepted values: available, error, pending"

instance ToText InstanceSnapshotState where
  toText = \case
    ISSAvailable -> "available"
    ISSError' -> "error"
    ISSPending -> "pending"

instance Hashable InstanceSnapshotState

instance NFData InstanceSnapshotState

instance ToByteString InstanceSnapshotState

instance ToQuery InstanceSnapshotState

instance ToHeader InstanceSnapshotState

instance FromJSON InstanceSnapshotState where
  parseJSON = parseJSONText "InstanceSnapshotState"

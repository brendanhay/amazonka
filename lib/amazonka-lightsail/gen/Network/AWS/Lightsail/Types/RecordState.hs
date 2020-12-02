{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RecordState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RecordState where

import Network.AWS.Prelude

data RecordState
  = RFailed
  | RStarted
  | RSucceeded
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

instance FromText RecordState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure RFailed
      "started" -> pure RStarted
      "succeeded" -> pure RSucceeded
      e ->
        fromTextError $
          "Failure parsing RecordState from value: '" <> e
            <> "'. Accepted values: failed, started, succeeded"

instance ToText RecordState where
  toText = \case
    RFailed -> "Failed"
    RStarted -> "Started"
    RSucceeded -> "Succeeded"

instance Hashable RecordState

instance NFData RecordState

instance ToByteString RecordState

instance ToQuery RecordState

instance ToHeader RecordState

instance FromJSON RecordState where
  parseJSON = parseJSONText "RecordState"

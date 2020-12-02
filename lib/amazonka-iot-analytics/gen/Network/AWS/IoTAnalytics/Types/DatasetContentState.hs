{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentState where

import Network.AWS.Prelude

data DatasetContentState
  = DCSCreating
  | DCSFailed
  | DCSSucceeded
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

instance FromText DatasetContentState where
  parser =
    takeLowerText >>= \case
      "creating" -> pure DCSCreating
      "failed" -> pure DCSFailed
      "succeeded" -> pure DCSSucceeded
      e ->
        fromTextError $
          "Failure parsing DatasetContentState from value: '" <> e
            <> "'. Accepted values: creating, failed, succeeded"

instance ToText DatasetContentState where
  toText = \case
    DCSCreating -> "CREATING"
    DCSFailed -> "FAILED"
    DCSSucceeded -> "SUCCEEDED"

instance Hashable DatasetContentState

instance NFData DatasetContentState

instance ToByteString DatasetContentState

instance ToQuery DatasetContentState

instance ToHeader DatasetContentState

instance FromJSON DatasetContentState where
  parseJSON = parseJSONText "DatasetContentState"

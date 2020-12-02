{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetStatus where

import Network.AWS.Prelude

data DatasetStatus
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

instance FromText DatasetStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      e ->
        fromTextError $
          "Failure parsing DatasetStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting"

instance ToText DatasetStatus where
  toText = \case
    Active -> "ACTIVE"
    Creating -> "CREATING"
    Deleting -> "DELETING"

instance Hashable DatasetStatus

instance NFData DatasetStatus

instance ToByteString DatasetStatus

instance ToQuery DatasetStatus

instance ToHeader DatasetStatus

instance FromJSON DatasetStatus where
  parseJSON = parseJSONText "DatasetStatus"

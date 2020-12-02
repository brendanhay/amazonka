{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICacheStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APICacheStatus where

import Network.AWS.Prelude

data APICacheStatus
  = Available
  | Creating
  | Deleting
  | Failed
  | Modifying
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

instance FromText APICacheStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      "failed" -> pure Failed
      "modifying" -> pure Modifying
      e ->
        fromTextError $
          "Failure parsing APICacheStatus from value: '" <> e
            <> "'. Accepted values: available, creating, deleting, failed, modifying"

instance ToText APICacheStatus where
  toText = \case
    Available -> "AVAILABLE"
    Creating -> "CREATING"
    Deleting -> "DELETING"
    Failed -> "FAILED"
    Modifying -> "MODIFYING"

instance Hashable APICacheStatus

instance NFData APICacheStatus

instance ToByteString APICacheStatus

instance ToQuery APICacheStatus

instance ToHeader APICacheStatus

instance FromJSON APICacheStatus where
  parseJSON = parseJSONText "APICacheStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackState where

import Network.AWS.Prelude

data ConformancePackState
  = CPSCreateComplete
  | CPSCreateFailed
  | CPSCreateInProgress
  | CPSDeleteFailed
  | CPSDeleteInProgress
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

instance FromText ConformancePackState where
  parser =
    takeLowerText >>= \case
      "create_complete" -> pure CPSCreateComplete
      "create_failed" -> pure CPSCreateFailed
      "create_in_progress" -> pure CPSCreateInProgress
      "delete_failed" -> pure CPSDeleteFailed
      "delete_in_progress" -> pure CPSDeleteInProgress
      e ->
        fromTextError $
          "Failure parsing ConformancePackState from value: '" <> e
            <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_failed, delete_in_progress"

instance ToText ConformancePackState where
  toText = \case
    CPSCreateComplete -> "CREATE_COMPLETE"
    CPSCreateFailed -> "CREATE_FAILED"
    CPSCreateInProgress -> "CREATE_IN_PROGRESS"
    CPSDeleteFailed -> "DELETE_FAILED"
    CPSDeleteInProgress -> "DELETE_IN_PROGRESS"

instance Hashable ConformancePackState

instance NFData ConformancePackState

instance ToByteString ConformancePackState

instance ToQuery ConformancePackState

instance ToHeader ConformancePackState

instance FromJSON ConformancePackState where
  parseJSON = parseJSONText "ConformancePackState"

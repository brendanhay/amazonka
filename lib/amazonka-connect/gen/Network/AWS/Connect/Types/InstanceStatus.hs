{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStatus where

import Network.AWS.Prelude

data InstanceStatus
  = Active
  | CreationFailed
  | CreationInProgress
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

instance FromText InstanceStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creation_failed" -> pure CreationFailed
      "creation_in_progress" -> pure CreationInProgress
      e ->
        fromTextError $
          "Failure parsing InstanceStatus from value: '" <> e
            <> "'. Accepted values: active, creation_failed, creation_in_progress"

instance ToText InstanceStatus where
  toText = \case
    Active -> "ACTIVE"
    CreationFailed -> "CREATION_FAILED"
    CreationInProgress -> "CREATION_IN_PROGRESS"

instance Hashable InstanceStatus

instance NFData InstanceStatus

instance ToByteString InstanceStatus

instance ToQuery InstanceStatus

instance ToHeader InstanceStatus

instance FromJSON InstanceStatus where
  parseJSON = parseJSONText "InstanceStatus"

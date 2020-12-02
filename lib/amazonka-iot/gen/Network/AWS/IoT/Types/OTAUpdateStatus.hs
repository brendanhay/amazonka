{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateStatus where

import Network.AWS.Prelude

data OTAUpdateStatus
  = CreateComplete
  | CreateFailed
  | CreateInProgress
  | CreatePending
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

instance FromText OTAUpdateStatus where
  parser =
    takeLowerText >>= \case
      "create_complete" -> pure CreateComplete
      "create_failed" -> pure CreateFailed
      "create_in_progress" -> pure CreateInProgress
      "create_pending" -> pure CreatePending
      e ->
        fromTextError $
          "Failure parsing OTAUpdateStatus from value: '" <> e
            <> "'. Accepted values: create_complete, create_failed, create_in_progress, create_pending"

instance ToText OTAUpdateStatus where
  toText = \case
    CreateComplete -> "CREATE_COMPLETE"
    CreateFailed -> "CREATE_FAILED"
    CreateInProgress -> "CREATE_IN_PROGRESS"
    CreatePending -> "CREATE_PENDING"

instance Hashable OTAUpdateStatus

instance NFData OTAUpdateStatus

instance ToByteString OTAUpdateStatus

instance ToQuery OTAUpdateStatus

instance ToHeader OTAUpdateStatus

instance ToJSON OTAUpdateStatus where
  toJSON = toJSONText

instance FromJSON OTAUpdateStatus where
  parseJSON = parseJSONText "OTAUpdateStatus"

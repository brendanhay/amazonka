{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceStatus where

import Network.AWS.Prelude

data NotebookInstanceStatus
  = NISDeleting
  | NISFailed
  | NISInService
  | NISPending
  | NISStopped
  | NISStopping
  | NISUpdating
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

instance FromText NotebookInstanceStatus where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure NISDeleting
      "failed" -> pure NISFailed
      "inservice" -> pure NISInService
      "pending" -> pure NISPending
      "stopped" -> pure NISStopped
      "stopping" -> pure NISStopping
      "updating" -> pure NISUpdating
      e ->
        fromTextError $
          "Failure parsing NotebookInstanceStatus from value: '" <> e
            <> "'. Accepted values: deleting, failed, inservice, pending, stopped, stopping, updating"

instance ToText NotebookInstanceStatus where
  toText = \case
    NISDeleting -> "Deleting"
    NISFailed -> "Failed"
    NISInService -> "InService"
    NISPending -> "Pending"
    NISStopped -> "Stopped"
    NISStopping -> "Stopping"
    NISUpdating -> "Updating"

instance Hashable NotebookInstanceStatus

instance NFData NotebookInstanceStatus

instance ToByteString NotebookInstanceStatus

instance ToQuery NotebookInstanceStatus

instance ToHeader NotebookInstanceStatus

instance ToJSON NotebookInstanceStatus where
  toJSON = toJSONText

instance FromJSON NotebookInstanceStatus where
  parseJSON = parseJSONText "NotebookInstanceStatus"

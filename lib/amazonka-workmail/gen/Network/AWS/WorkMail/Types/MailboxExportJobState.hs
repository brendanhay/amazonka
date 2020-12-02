{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.MailboxExportJobState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.MailboxExportJobState where

import Network.AWS.Prelude

data MailboxExportJobState
  = Cancelled
  | Completed
  | Failed
  | Running
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

instance FromText MailboxExportJobState where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure Cancelled
      "completed" -> pure Completed
      "failed" -> pure Failed
      "running" -> pure Running
      e ->
        fromTextError $
          "Failure parsing MailboxExportJobState from value: '" <> e
            <> "'. Accepted values: cancelled, completed, failed, running"

instance ToText MailboxExportJobState where
  toText = \case
    Cancelled -> "CANCELLED"
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    Running -> "RUNNING"

instance Hashable MailboxExportJobState

instance NFData MailboxExportJobState

instance ToByteString MailboxExportJobState

instance ToQuery MailboxExportJobState

instance ToHeader MailboxExportJobState

instance FromJSON MailboxExportJobState where
  parseJSON = parseJSONText "MailboxExportJobState"

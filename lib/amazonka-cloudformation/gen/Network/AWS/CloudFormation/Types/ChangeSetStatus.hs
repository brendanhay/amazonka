{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetStatus where

import Network.AWS.Prelude

data ChangeSetStatus
  = CSSCreateComplete
  | CSSCreateInProgress
  | CSSCreatePending
  | CSSDeleteComplete
  | CSSDeleteFailed
  | CSSDeleteInProgress
  | CSSDeletePending
  | CSSFailed
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

instance FromText ChangeSetStatus where
  parser =
    takeLowerText >>= \case
      "create_complete" -> pure CSSCreateComplete
      "create_in_progress" -> pure CSSCreateInProgress
      "create_pending" -> pure CSSCreatePending
      "delete_complete" -> pure CSSDeleteComplete
      "delete_failed" -> pure CSSDeleteFailed
      "delete_in_progress" -> pure CSSDeleteInProgress
      "delete_pending" -> pure CSSDeletePending
      "failed" -> pure CSSFailed
      e ->
        fromTextError $
          "Failure parsing ChangeSetStatus from value: '" <> e
            <> "'. Accepted values: create_complete, create_in_progress, create_pending, delete_complete, delete_failed, delete_in_progress, delete_pending, failed"

instance ToText ChangeSetStatus where
  toText = \case
    CSSCreateComplete -> "CREATE_COMPLETE"
    CSSCreateInProgress -> "CREATE_IN_PROGRESS"
    CSSCreatePending -> "CREATE_PENDING"
    CSSDeleteComplete -> "DELETE_COMPLETE"
    CSSDeleteFailed -> "DELETE_FAILED"
    CSSDeleteInProgress -> "DELETE_IN_PROGRESS"
    CSSDeletePending -> "DELETE_PENDING"
    CSSFailed -> "FAILED"

instance Hashable ChangeSetStatus

instance NFData ChangeSetStatus

instance ToByteString ChangeSetStatus

instance ToQuery ChangeSetStatus

instance ToHeader ChangeSetStatus

instance FromXML ChangeSetStatus where
  parseXML = parseXMLText "ChangeSetStatus"

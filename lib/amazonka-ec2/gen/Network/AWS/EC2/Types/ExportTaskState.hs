{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ExportTaskState
  = ETSActive
  | ETSCancelled
  | ETSCancelling
  | ETSCompleted
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

instance FromText ExportTaskState where
  parser =
    takeLowerText >>= \case
      "active" -> pure ETSActive
      "cancelled" -> pure ETSCancelled
      "cancelling" -> pure ETSCancelling
      "completed" -> pure ETSCompleted
      e ->
        fromTextError $
          "Failure parsing ExportTaskState from value: '" <> e
            <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ExportTaskState where
  toText = \case
    ETSActive -> "active"
    ETSCancelled -> "cancelled"
    ETSCancelling -> "cancelling"
    ETSCompleted -> "completed"

instance Hashable ExportTaskState

instance NFData ExportTaskState

instance ToByteString ExportTaskState

instance ToQuery ExportTaskState

instance ToHeader ExportTaskState

instance FromXML ExportTaskState where
  parseXML = parseXMLText "ExportTaskState"

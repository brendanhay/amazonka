{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTimeoutType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTimeoutType where

import Network.AWS.Prelude

data WorkflowExecutionTimeoutType = WETTStartToClose
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

instance FromText WorkflowExecutionTimeoutType where
  parser =
    takeLowerText >>= \case
      "start_to_close" -> pure WETTStartToClose
      e ->
        fromTextError $
          "Failure parsing WorkflowExecutionTimeoutType from value: '" <> e
            <> "'. Accepted values: start_to_close"

instance ToText WorkflowExecutionTimeoutType where
  toText = \case
    WETTStartToClose -> "START_TO_CLOSE"

instance Hashable WorkflowExecutionTimeoutType

instance NFData WorkflowExecutionTimeoutType

instance ToByteString WorkflowExecutionTimeoutType

instance ToQuery WorkflowExecutionTimeoutType

instance ToHeader WorkflowExecutionTimeoutType

instance FromJSON WorkflowExecutionTimeoutType where
  parseJSON = parseJSONText "WorkflowExecutionTimeoutType"

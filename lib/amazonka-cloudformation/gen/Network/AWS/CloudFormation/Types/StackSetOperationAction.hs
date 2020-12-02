{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationAction where

import Network.AWS.Prelude

data StackSetOperationAction
  = Create
  | Delete
  | DetectDrift
  | Update
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

instance FromText StackSetOperationAction where
  parser =
    takeLowerText >>= \case
      "create" -> pure Create
      "delete" -> pure Delete
      "detect_drift" -> pure DetectDrift
      "update" -> pure Update
      e ->
        fromTextError $
          "Failure parsing StackSetOperationAction from value: '" <> e
            <> "'. Accepted values: create, delete, detect_drift, update"

instance ToText StackSetOperationAction where
  toText = \case
    Create -> "CREATE"
    Delete -> "DELETE"
    DetectDrift -> "DETECT_DRIFT"
    Update -> "UPDATE"

instance Hashable StackSetOperationAction

instance NFData StackSetOperationAction

instance ToByteString StackSetOperationAction

instance ToQuery StackSetOperationAction

instance ToHeader StackSetOperationAction

instance FromXML StackSetOperationAction where
  parseXML = parseXMLText "StackSetOperationAction"

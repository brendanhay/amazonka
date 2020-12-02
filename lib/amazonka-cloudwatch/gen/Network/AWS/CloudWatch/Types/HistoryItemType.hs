{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.HistoryItemType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.HistoryItemType where

import Network.AWS.Prelude

data HistoryItemType
  = Action
  | ConfigurationUpdate
  | StateUpdate
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

instance FromText HistoryItemType where
  parser =
    takeLowerText >>= \case
      "action" -> pure Action
      "configurationupdate" -> pure ConfigurationUpdate
      "stateupdate" -> pure StateUpdate
      e ->
        fromTextError $
          "Failure parsing HistoryItemType from value: '" <> e
            <> "'. Accepted values: action, configurationupdate, stateupdate"

instance ToText HistoryItemType where
  toText = \case
    Action -> "Action"
    ConfigurationUpdate -> "ConfigurationUpdate"
    StateUpdate -> "StateUpdate"

instance Hashable HistoryItemType

instance NFData HistoryItemType

instance ToByteString HistoryItemType

instance ToQuery HistoryItemType

instance ToHeader HistoryItemType

instance FromXML HistoryItemType where
  parseXML = parseXMLText "HistoryItemType"

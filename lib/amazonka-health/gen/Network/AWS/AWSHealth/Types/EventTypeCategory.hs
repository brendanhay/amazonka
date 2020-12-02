{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventTypeCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventTypeCategory where

import Network.AWS.Prelude

data EventTypeCategory
  = AccountNotification
  | Investigation
  | Issue
  | ScheduledChange
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

instance FromText EventTypeCategory where
  parser =
    takeLowerText >>= \case
      "accountnotification" -> pure AccountNotification
      "investigation" -> pure Investigation
      "issue" -> pure Issue
      "scheduledchange" -> pure ScheduledChange
      e ->
        fromTextError $
          "Failure parsing EventTypeCategory from value: '" <> e
            <> "'. Accepted values: accountnotification, investigation, issue, scheduledchange"

instance ToText EventTypeCategory where
  toText = \case
    AccountNotification -> "accountNotification"
    Investigation -> "investigation"
    Issue -> "issue"
    ScheduledChange -> "scheduledChange"

instance Hashable EventTypeCategory

instance NFData EventTypeCategory

instance ToByteString EventTypeCategory

instance ToQuery EventTypeCategory

instance ToHeader EventTypeCategory

instance ToJSON EventTypeCategory where
  toJSON = toJSONText

instance FromJSON EventTypeCategory where
  parseJSON = parseJSONText "EventTypeCategory"

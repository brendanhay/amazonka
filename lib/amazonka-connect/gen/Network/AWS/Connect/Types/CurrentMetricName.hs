{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricName where

import Network.AWS.Prelude

-- | The current metric names.
data CurrentMetricName
  = AgentsAfterContactWork
  | AgentsAvailable
  | AgentsError
  | AgentsNonProductive
  | AgentsOnCall
  | AgentsOnContact
  | AgentsOnline
  | AgentsStaffed
  | ContactsInQueue
  | ContactsScheduled
  | OldestContactAge
  | SlotsActive
  | SlotsAvailable
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

instance FromText CurrentMetricName where
  parser =
    takeLowerText >>= \case
      "agents_after_contact_work" -> pure AgentsAfterContactWork
      "agents_available" -> pure AgentsAvailable
      "agents_error" -> pure AgentsError
      "agents_non_productive" -> pure AgentsNonProductive
      "agents_on_call" -> pure AgentsOnCall
      "agents_on_contact" -> pure AgentsOnContact
      "agents_online" -> pure AgentsOnline
      "agents_staffed" -> pure AgentsStaffed
      "contacts_in_queue" -> pure ContactsInQueue
      "contacts_scheduled" -> pure ContactsScheduled
      "oldest_contact_age" -> pure OldestContactAge
      "slots_active" -> pure SlotsActive
      "slots_available" -> pure SlotsAvailable
      e ->
        fromTextError $
          "Failure parsing CurrentMetricName from value: '" <> e
            <> "'. Accepted values: agents_after_contact_work, agents_available, agents_error, agents_non_productive, agents_on_call, agents_on_contact, agents_online, agents_staffed, contacts_in_queue, contacts_scheduled, oldest_contact_age, slots_active, slots_available"

instance ToText CurrentMetricName where
  toText = \case
    AgentsAfterContactWork -> "AGENTS_AFTER_CONTACT_WORK"
    AgentsAvailable -> "AGENTS_AVAILABLE"
    AgentsError -> "AGENTS_ERROR"
    AgentsNonProductive -> "AGENTS_NON_PRODUCTIVE"
    AgentsOnCall -> "AGENTS_ON_CALL"
    AgentsOnContact -> "AGENTS_ON_CONTACT"
    AgentsOnline -> "AGENTS_ONLINE"
    AgentsStaffed -> "AGENTS_STAFFED"
    ContactsInQueue -> "CONTACTS_IN_QUEUE"
    ContactsScheduled -> "CONTACTS_SCHEDULED"
    OldestContactAge -> "OLDEST_CONTACT_AGE"
    SlotsActive -> "SLOTS_ACTIVE"
    SlotsAvailable -> "SLOTS_AVAILABLE"

instance Hashable CurrentMetricName

instance NFData CurrentMetricName

instance ToByteString CurrentMetricName

instance ToQuery CurrentMetricName

instance ToHeader CurrentMetricName

instance ToJSON CurrentMetricName where
  toJSON = toJSONText

instance FromJSON CurrentMetricName where
  parseJSON = parseJSONText "CurrentMetricName"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricName where

import Network.AWS.Prelude

-- | The historical metric names.
data HistoricalMetricName
  = APIContactsHandled
  | AbandonTime
  | AfterContactWorkTime
  | CallbackContactsHandled
  | ContactsAbandoned
  | ContactsAgentHungUpFirst
  | ContactsConsulted
  | ContactsHandled
  | ContactsHandledIncoming
  | ContactsHandledOutbound
  | ContactsHoldAbandons
  | ContactsMissed
  | ContactsQueued
  | ContactsTransferredIn
  | ContactsTransferredInFromQueue
  | ContactsTransferredOut
  | ContactsTransferredOutFromQueue
  | HandleTime
  | HoldTime
  | InteractionAndHoldTime
  | InteractionTime
  | Occupancy
  | QueueAnswerTime
  | QueuedTime
  | ServiceLevel
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

instance FromText HistoricalMetricName where
  parser =
    takeLowerText >>= \case
      "api_contacts_handled" -> pure APIContactsHandled
      "abandon_time" -> pure AbandonTime
      "after_contact_work_time" -> pure AfterContactWorkTime
      "callback_contacts_handled" -> pure CallbackContactsHandled
      "contacts_abandoned" -> pure ContactsAbandoned
      "contacts_agent_hung_up_first" -> pure ContactsAgentHungUpFirst
      "contacts_consulted" -> pure ContactsConsulted
      "contacts_handled" -> pure ContactsHandled
      "contacts_handled_incoming" -> pure ContactsHandledIncoming
      "contacts_handled_outbound" -> pure ContactsHandledOutbound
      "contacts_hold_abandons" -> pure ContactsHoldAbandons
      "contacts_missed" -> pure ContactsMissed
      "contacts_queued" -> pure ContactsQueued
      "contacts_transferred_in" -> pure ContactsTransferredIn
      "contacts_transferred_in_from_queue" -> pure ContactsTransferredInFromQueue
      "contacts_transferred_out" -> pure ContactsTransferredOut
      "contacts_transferred_out_from_queue" -> pure ContactsTransferredOutFromQueue
      "handle_time" -> pure HandleTime
      "hold_time" -> pure HoldTime
      "interaction_and_hold_time" -> pure InteractionAndHoldTime
      "interaction_time" -> pure InteractionTime
      "occupancy" -> pure Occupancy
      "queue_answer_time" -> pure QueueAnswerTime
      "queued_time" -> pure QueuedTime
      "service_level" -> pure ServiceLevel
      e ->
        fromTextError $
          "Failure parsing HistoricalMetricName from value: '" <> e
            <> "'. Accepted values: api_contacts_handled, abandon_time, after_contact_work_time, callback_contacts_handled, contacts_abandoned, contacts_agent_hung_up_first, contacts_consulted, contacts_handled, contacts_handled_incoming, contacts_handled_outbound, contacts_hold_abandons, contacts_missed, contacts_queued, contacts_transferred_in, contacts_transferred_in_from_queue, contacts_transferred_out, contacts_transferred_out_from_queue, handle_time, hold_time, interaction_and_hold_time, interaction_time, occupancy, queue_answer_time, queued_time, service_level"

instance ToText HistoricalMetricName where
  toText = \case
    APIContactsHandled -> "API_CONTACTS_HANDLED"
    AbandonTime -> "ABANDON_TIME"
    AfterContactWorkTime -> "AFTER_CONTACT_WORK_TIME"
    CallbackContactsHandled -> "CALLBACK_CONTACTS_HANDLED"
    ContactsAbandoned -> "CONTACTS_ABANDONED"
    ContactsAgentHungUpFirst -> "CONTACTS_AGENT_HUNG_UP_FIRST"
    ContactsConsulted -> "CONTACTS_CONSULTED"
    ContactsHandled -> "CONTACTS_HANDLED"
    ContactsHandledIncoming -> "CONTACTS_HANDLED_INCOMING"
    ContactsHandledOutbound -> "CONTACTS_HANDLED_OUTBOUND"
    ContactsHoldAbandons -> "CONTACTS_HOLD_ABANDONS"
    ContactsMissed -> "CONTACTS_MISSED"
    ContactsQueued -> "CONTACTS_QUEUED"
    ContactsTransferredIn -> "CONTACTS_TRANSFERRED_IN"
    ContactsTransferredInFromQueue -> "CONTACTS_TRANSFERRED_IN_FROM_QUEUE"
    ContactsTransferredOut -> "CONTACTS_TRANSFERRED_OUT"
    ContactsTransferredOutFromQueue -> "CONTACTS_TRANSFERRED_OUT_FROM_QUEUE"
    HandleTime -> "HANDLE_TIME"
    HoldTime -> "HOLD_TIME"
    InteractionAndHoldTime -> "INTERACTION_AND_HOLD_TIME"
    InteractionTime -> "INTERACTION_TIME"
    Occupancy -> "OCCUPANCY"
    QueueAnswerTime -> "QUEUE_ANSWER_TIME"
    QueuedTime -> "QUEUED_TIME"
    ServiceLevel -> "SERVICE_LEVEL"

instance Hashable HistoricalMetricName

instance NFData HistoricalMetricName

instance ToByteString HistoricalMetricName

instance ToQuery HistoricalMetricName

instance ToHeader HistoricalMetricName

instance ToJSON HistoricalMetricName where
  toJSON = toJSONText

instance FromJSON HistoricalMetricName where
  parseJSON = parseJSONText "HistoricalMetricName"

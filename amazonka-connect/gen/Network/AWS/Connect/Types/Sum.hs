{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Sum where

import Network.AWS.Prelude

data Channel =
  Voice
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Channel where
    parser = takeLowerText >>= \case
        "voice" -> pure Voice
        e -> fromTextError $ "Failure parsing Channel from value: '" <> e
           <> "'. Accepted values: voice"

instance ToText Channel where
    toText = \case
        Voice -> "VOICE"

instance Hashable     Channel
instance NFData       Channel
instance ToByteString Channel
instance ToQuery      Channel
instance ToHeader     Channel

instance ToJSON Channel where
    toJSON = toJSONText

instance FromJSON Channel where
    parseJSON = parseJSONText "Channel"

data Comparison =
  LT'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Comparison where
    parser = takeLowerText >>= \case
        "lt" -> pure LT'
        e -> fromTextError $ "Failure parsing Comparison from value: '" <> e
           <> "'. Accepted values: lt"

instance ToText Comparison where
    toText = \case
        LT' -> "LT"

instance Hashable     Comparison
instance NFData       Comparison
instance ToByteString Comparison
instance ToQuery      Comparison
instance ToHeader     Comparison

instance ToJSON Comparison where
    toJSON = toJSONText

instance FromJSON Comparison where
    parseJSON = parseJSONText "Comparison"

-- | A list of current metric names.
--
--
data CurrentMetricName
  = AgentsAfterContactWork
  | AgentsAvailable
  | AgentsError
  | AgentsNonProductive
  | AgentsOnCall
  | AgentsOnline
  | AgentsStaffed
  | ContactsInQueue
  | ContactsScheduled
  | OldestContactAge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CurrentMetricName where
    parser = takeLowerText >>= \case
        "agents_after_contact_work" -> pure AgentsAfterContactWork
        "agents_available" -> pure AgentsAvailable
        "agents_error" -> pure AgentsError
        "agents_non_productive" -> pure AgentsNonProductive
        "agents_on_call" -> pure AgentsOnCall
        "agents_online" -> pure AgentsOnline
        "agents_staffed" -> pure AgentsStaffed
        "contacts_in_queue" -> pure ContactsInQueue
        "contacts_scheduled" -> pure ContactsScheduled
        "oldest_contact_age" -> pure OldestContactAge
        e -> fromTextError $ "Failure parsing CurrentMetricName from value: '" <> e
           <> "'. Accepted values: agents_after_contact_work, agents_available, agents_error, agents_non_productive, agents_on_call, agents_online, agents_staffed, contacts_in_queue, contacts_scheduled, oldest_contact_age"

instance ToText CurrentMetricName where
    toText = \case
        AgentsAfterContactWork -> "AGENTS_AFTER_CONTACT_WORK"
        AgentsAvailable -> "AGENTS_AVAILABLE"
        AgentsError -> "AGENTS_ERROR"
        AgentsNonProductive -> "AGENTS_NON_PRODUCTIVE"
        AgentsOnCall -> "AGENTS_ON_CALL"
        AgentsOnline -> "AGENTS_ONLINE"
        AgentsStaffed -> "AGENTS_STAFFED"
        ContactsInQueue -> "CONTACTS_IN_QUEUE"
        ContactsScheduled -> "CONTACTS_SCHEDULED"
        OldestContactAge -> "OLDEST_CONTACT_AGE"

instance Hashable     CurrentMetricName
instance NFData       CurrentMetricName
instance ToByteString CurrentMetricName
instance ToQuery      CurrentMetricName
instance ToHeader     CurrentMetricName

instance ToJSON CurrentMetricName where
    toJSON = toJSONText

instance FromJSON CurrentMetricName where
    parseJSON = parseJSONText "CurrentMetricName"

data Grouping
  = Channel
  | Queue
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Grouping where
    parser = takeLowerText >>= \case
        "channel" -> pure Channel
        "queue" -> pure Queue
        e -> fromTextError $ "Failure parsing Grouping from value: '" <> e
           <> "'. Accepted values: channel, queue"

instance ToText Grouping where
    toText = \case
        Channel -> "CHANNEL"
        Queue -> "QUEUE"

instance Hashable     Grouping
instance NFData       Grouping
instance ToByteString Grouping
instance ToQuery      Grouping
instance ToHeader     Grouping

instance ToJSON Grouping where
    toJSON = toJSONText

-- | A list of historical metric names.
--
--
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HistoricalMetricName where
    parser = takeLowerText >>= \case
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
        e -> fromTextError $ "Failure parsing HistoricalMetricName from value: '" <> e
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

instance Hashable     HistoricalMetricName
instance NFData       HistoricalMetricName
instance ToByteString HistoricalMetricName
instance ToQuery      HistoricalMetricName
instance ToHeader     HistoricalMetricName

instance ToJSON HistoricalMetricName where
    toJSON = toJSONText

instance FromJSON HistoricalMetricName where
    parseJSON = parseJSONText "HistoricalMetricName"

data PhoneType
  = DeskPhone
  | SoftPhone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PhoneType where
    parser = takeLowerText >>= \case
        "desk_phone" -> pure DeskPhone
        "soft_phone" -> pure SoftPhone
        e -> fromTextError $ "Failure parsing PhoneType from value: '" <> e
           <> "'. Accepted values: desk_phone, soft_phone"

instance ToText PhoneType where
    toText = \case
        DeskPhone -> "DESK_PHONE"
        SoftPhone -> "SOFT_PHONE"

instance Hashable     PhoneType
instance NFData       PhoneType
instance ToByteString PhoneType
instance ToQuery      PhoneType
instance ToHeader     PhoneType

instance ToJSON PhoneType where
    toJSON = toJSONText

instance FromJSON PhoneType where
    parseJSON = parseJSONText "PhoneType"

data Statistic
  = Avg
  | Max
  | Sum
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Statistic where
    parser = takeLowerText >>= \case
        "avg" -> pure Avg
        "max" -> pure Max
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing Statistic from value: '" <> e
           <> "'. Accepted values: avg, max, sum"

instance ToText Statistic where
    toText = \case
        Avg -> "AVG"
        Max -> "MAX"
        Sum -> "SUM"

instance Hashable     Statistic
instance NFData       Statistic
instance ToByteString Statistic
instance ToQuery      Statistic
instance ToHeader     Statistic

instance ToJSON Statistic where
    toJSON = toJSONText

instance FromJSON Statistic where
    parseJSON = parseJSONText "Statistic"

data Unit
  = Count
  | Percent
  | Seconds
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Unit where
    parser = takeLowerText >>= \case
        "count" -> pure Count
        "percent" -> pure Percent
        "seconds" -> pure Seconds
        e -> fromTextError $ "Failure parsing Unit from value: '" <> e
           <> "'. Accepted values: count, percent, seconds"

instance ToText Unit where
    toText = \case
        Count -> "COUNT"
        Percent -> "PERCENT"
        Seconds -> "SECONDS"

instance Hashable     Unit
instance NFData       Unit
instance ToByteString Unit
instance ToQuery      Unit
instance ToHeader     Unit

instance ToJSON Unit where
    toJSON = toJSONText

instance FromJSON Unit where
    parseJSON = parseJSONText "Unit"

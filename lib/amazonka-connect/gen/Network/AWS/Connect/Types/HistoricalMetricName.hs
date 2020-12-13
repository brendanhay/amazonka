{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricName
  ( HistoricalMetricName
      ( HistoricalMetricName',
        ContactsQueued,
        ContactsHandled,
        ContactsAbandoned,
        ContactsConsulted,
        ContactsAgentHungUpFirst,
        ContactsHandledIncoming,
        ContactsHandledOutbound,
        ContactsHoldAbandons,
        ContactsTransferredIn,
        ContactsTransferredOut,
        ContactsTransferredInFromQueue,
        ContactsTransferredOutFromQueue,
        ContactsMissed,
        CallbackContactsHandled,
        APIContactsHandled,
        Occupancy,
        HandleTime,
        AfterContactWorkTime,
        QueuedTime,
        AbandonTime,
        QueueAnswerTime,
        HoldTime,
        InteractionTime,
        InteractionAndHoldTime,
        ServiceLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The historical metric names.
newtype HistoricalMetricName = HistoricalMetricName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ContactsQueued :: HistoricalMetricName
pattern ContactsQueued = HistoricalMetricName' "CONTACTS_QUEUED"

pattern ContactsHandled :: HistoricalMetricName
pattern ContactsHandled = HistoricalMetricName' "CONTACTS_HANDLED"

pattern ContactsAbandoned :: HistoricalMetricName
pattern ContactsAbandoned = HistoricalMetricName' "CONTACTS_ABANDONED"

pattern ContactsConsulted :: HistoricalMetricName
pattern ContactsConsulted = HistoricalMetricName' "CONTACTS_CONSULTED"

pattern ContactsAgentHungUpFirst :: HistoricalMetricName
pattern ContactsAgentHungUpFirst = HistoricalMetricName' "CONTACTS_AGENT_HUNG_UP_FIRST"

pattern ContactsHandledIncoming :: HistoricalMetricName
pattern ContactsHandledIncoming = HistoricalMetricName' "CONTACTS_HANDLED_INCOMING"

pattern ContactsHandledOutbound :: HistoricalMetricName
pattern ContactsHandledOutbound = HistoricalMetricName' "CONTACTS_HANDLED_OUTBOUND"

pattern ContactsHoldAbandons :: HistoricalMetricName
pattern ContactsHoldAbandons = HistoricalMetricName' "CONTACTS_HOLD_ABANDONS"

pattern ContactsTransferredIn :: HistoricalMetricName
pattern ContactsTransferredIn = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN"

pattern ContactsTransferredOut :: HistoricalMetricName
pattern ContactsTransferredOut = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT"

pattern ContactsTransferredInFromQueue :: HistoricalMetricName
pattern ContactsTransferredInFromQueue = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN_FROM_QUEUE"

pattern ContactsTransferredOutFromQueue :: HistoricalMetricName
pattern ContactsTransferredOutFromQueue = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT_FROM_QUEUE"

pattern ContactsMissed :: HistoricalMetricName
pattern ContactsMissed = HistoricalMetricName' "CONTACTS_MISSED"

pattern CallbackContactsHandled :: HistoricalMetricName
pattern CallbackContactsHandled = HistoricalMetricName' "CALLBACK_CONTACTS_HANDLED"

pattern APIContactsHandled :: HistoricalMetricName
pattern APIContactsHandled = HistoricalMetricName' "API_CONTACTS_HANDLED"

pattern Occupancy :: HistoricalMetricName
pattern Occupancy = HistoricalMetricName' "OCCUPANCY"

pattern HandleTime :: HistoricalMetricName
pattern HandleTime = HistoricalMetricName' "HANDLE_TIME"

pattern AfterContactWorkTime :: HistoricalMetricName
pattern AfterContactWorkTime = HistoricalMetricName' "AFTER_CONTACT_WORK_TIME"

pattern QueuedTime :: HistoricalMetricName
pattern QueuedTime = HistoricalMetricName' "QUEUED_TIME"

pattern AbandonTime :: HistoricalMetricName
pattern AbandonTime = HistoricalMetricName' "ABANDON_TIME"

pattern QueueAnswerTime :: HistoricalMetricName
pattern QueueAnswerTime = HistoricalMetricName' "QUEUE_ANSWER_TIME"

pattern HoldTime :: HistoricalMetricName
pattern HoldTime = HistoricalMetricName' "HOLD_TIME"

pattern InteractionTime :: HistoricalMetricName
pattern InteractionTime = HistoricalMetricName' "INTERACTION_TIME"

pattern InteractionAndHoldTime :: HistoricalMetricName
pattern InteractionAndHoldTime = HistoricalMetricName' "INTERACTION_AND_HOLD_TIME"

pattern ServiceLevel :: HistoricalMetricName
pattern ServiceLevel = HistoricalMetricName' "SERVICE_LEVEL"

{-# COMPLETE
  ContactsQueued,
  ContactsHandled,
  ContactsAbandoned,
  ContactsConsulted,
  ContactsAgentHungUpFirst,
  ContactsHandledIncoming,
  ContactsHandledOutbound,
  ContactsHoldAbandons,
  ContactsTransferredIn,
  ContactsTransferredOut,
  ContactsTransferredInFromQueue,
  ContactsTransferredOutFromQueue,
  ContactsMissed,
  CallbackContactsHandled,
  APIContactsHandled,
  Occupancy,
  HandleTime,
  AfterContactWorkTime,
  QueuedTime,
  AbandonTime,
  QueueAnswerTime,
  HoldTime,
  InteractionTime,
  InteractionAndHoldTime,
  ServiceLevel,
  HistoricalMetricName'
  #-}

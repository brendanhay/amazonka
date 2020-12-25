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
        HistoricalMetricNameContactsQueued,
        HistoricalMetricNameContactsHandled,
        HistoricalMetricNameContactsAbandoned,
        HistoricalMetricNameContactsConsulted,
        HistoricalMetricNameContactsAgentHungUpFirst,
        HistoricalMetricNameContactsHandledIncoming,
        HistoricalMetricNameContactsHandledOutbound,
        HistoricalMetricNameContactsHoldAbandons,
        HistoricalMetricNameContactsTransferredIn,
        HistoricalMetricNameContactsTransferredOut,
        HistoricalMetricNameContactsTransferredInFromQueue,
        HistoricalMetricNameContactsTransferredOutFromQueue,
        HistoricalMetricNameContactsMissed,
        HistoricalMetricNameCallbackContactsHandled,
        HistoricalMetricNameApiContactsHandled,
        HistoricalMetricNameOccupancy,
        HistoricalMetricNameHandleTime,
        HistoricalMetricNameAfterContactWorkTime,
        HistoricalMetricNameQueuedTime,
        HistoricalMetricNameAbandonTime,
        HistoricalMetricNameQueueAnswerTime,
        HistoricalMetricNameHoldTime,
        HistoricalMetricNameInteractionTime,
        HistoricalMetricNameInteractionAndHoldTime,
        HistoricalMetricNameServiceLevel,
        fromHistoricalMetricName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The historical metric names.
newtype HistoricalMetricName = HistoricalMetricName'
  { fromHistoricalMetricName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HistoricalMetricNameContactsQueued :: HistoricalMetricName
pattern HistoricalMetricNameContactsQueued = HistoricalMetricName' "CONTACTS_QUEUED"

pattern HistoricalMetricNameContactsHandled :: HistoricalMetricName
pattern HistoricalMetricNameContactsHandled = HistoricalMetricName' "CONTACTS_HANDLED"

pattern HistoricalMetricNameContactsAbandoned :: HistoricalMetricName
pattern HistoricalMetricNameContactsAbandoned = HistoricalMetricName' "CONTACTS_ABANDONED"

pattern HistoricalMetricNameContactsConsulted :: HistoricalMetricName
pattern HistoricalMetricNameContactsConsulted = HistoricalMetricName' "CONTACTS_CONSULTED"

pattern HistoricalMetricNameContactsAgentHungUpFirst :: HistoricalMetricName
pattern HistoricalMetricNameContactsAgentHungUpFirst = HistoricalMetricName' "CONTACTS_AGENT_HUNG_UP_FIRST"

pattern HistoricalMetricNameContactsHandledIncoming :: HistoricalMetricName
pattern HistoricalMetricNameContactsHandledIncoming = HistoricalMetricName' "CONTACTS_HANDLED_INCOMING"

pattern HistoricalMetricNameContactsHandledOutbound :: HistoricalMetricName
pattern HistoricalMetricNameContactsHandledOutbound = HistoricalMetricName' "CONTACTS_HANDLED_OUTBOUND"

pattern HistoricalMetricNameContactsHoldAbandons :: HistoricalMetricName
pattern HistoricalMetricNameContactsHoldAbandons = HistoricalMetricName' "CONTACTS_HOLD_ABANDONS"

pattern HistoricalMetricNameContactsTransferredIn :: HistoricalMetricName
pattern HistoricalMetricNameContactsTransferredIn = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN"

pattern HistoricalMetricNameContactsTransferredOut :: HistoricalMetricName
pattern HistoricalMetricNameContactsTransferredOut = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT"

pattern HistoricalMetricNameContactsTransferredInFromQueue :: HistoricalMetricName
pattern HistoricalMetricNameContactsTransferredInFromQueue = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN_FROM_QUEUE"

pattern HistoricalMetricNameContactsTransferredOutFromQueue :: HistoricalMetricName
pattern HistoricalMetricNameContactsTransferredOutFromQueue = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT_FROM_QUEUE"

pattern HistoricalMetricNameContactsMissed :: HistoricalMetricName
pattern HistoricalMetricNameContactsMissed = HistoricalMetricName' "CONTACTS_MISSED"

pattern HistoricalMetricNameCallbackContactsHandled :: HistoricalMetricName
pattern HistoricalMetricNameCallbackContactsHandled = HistoricalMetricName' "CALLBACK_CONTACTS_HANDLED"

pattern HistoricalMetricNameApiContactsHandled :: HistoricalMetricName
pattern HistoricalMetricNameApiContactsHandled = HistoricalMetricName' "API_CONTACTS_HANDLED"

pattern HistoricalMetricNameOccupancy :: HistoricalMetricName
pattern HistoricalMetricNameOccupancy = HistoricalMetricName' "OCCUPANCY"

pattern HistoricalMetricNameHandleTime :: HistoricalMetricName
pattern HistoricalMetricNameHandleTime = HistoricalMetricName' "HANDLE_TIME"

pattern HistoricalMetricNameAfterContactWorkTime :: HistoricalMetricName
pattern HistoricalMetricNameAfterContactWorkTime = HistoricalMetricName' "AFTER_CONTACT_WORK_TIME"

pattern HistoricalMetricNameQueuedTime :: HistoricalMetricName
pattern HistoricalMetricNameQueuedTime = HistoricalMetricName' "QUEUED_TIME"

pattern HistoricalMetricNameAbandonTime :: HistoricalMetricName
pattern HistoricalMetricNameAbandonTime = HistoricalMetricName' "ABANDON_TIME"

pattern HistoricalMetricNameQueueAnswerTime :: HistoricalMetricName
pattern HistoricalMetricNameQueueAnswerTime = HistoricalMetricName' "QUEUE_ANSWER_TIME"

pattern HistoricalMetricNameHoldTime :: HistoricalMetricName
pattern HistoricalMetricNameHoldTime = HistoricalMetricName' "HOLD_TIME"

pattern HistoricalMetricNameInteractionTime :: HistoricalMetricName
pattern HistoricalMetricNameInteractionTime = HistoricalMetricName' "INTERACTION_TIME"

pattern HistoricalMetricNameInteractionAndHoldTime :: HistoricalMetricName
pattern HistoricalMetricNameInteractionAndHoldTime = HistoricalMetricName' "INTERACTION_AND_HOLD_TIME"

pattern HistoricalMetricNameServiceLevel :: HistoricalMetricName
pattern HistoricalMetricNameServiceLevel = HistoricalMetricName' "SERVICE_LEVEL"

{-# COMPLETE
  HistoricalMetricNameContactsQueued,
  HistoricalMetricNameContactsHandled,
  HistoricalMetricNameContactsAbandoned,
  HistoricalMetricNameContactsConsulted,
  HistoricalMetricNameContactsAgentHungUpFirst,
  HistoricalMetricNameContactsHandledIncoming,
  HistoricalMetricNameContactsHandledOutbound,
  HistoricalMetricNameContactsHoldAbandons,
  HistoricalMetricNameContactsTransferredIn,
  HistoricalMetricNameContactsTransferredOut,
  HistoricalMetricNameContactsTransferredInFromQueue,
  HistoricalMetricNameContactsTransferredOutFromQueue,
  HistoricalMetricNameContactsMissed,
  HistoricalMetricNameCallbackContactsHandled,
  HistoricalMetricNameApiContactsHandled,
  HistoricalMetricNameOccupancy,
  HistoricalMetricNameHandleTime,
  HistoricalMetricNameAfterContactWorkTime,
  HistoricalMetricNameQueuedTime,
  HistoricalMetricNameAbandonTime,
  HistoricalMetricNameQueueAnswerTime,
  HistoricalMetricNameHoldTime,
  HistoricalMetricNameInteractionTime,
  HistoricalMetricNameInteractionAndHoldTime,
  HistoricalMetricNameServiceLevel,
  HistoricalMetricName'
  #-}

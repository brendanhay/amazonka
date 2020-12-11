-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlowType
  ( ContactFlowType
      ( ContactFlowType',
        AgentHold,
        AgentTransfer,
        AgentWhisper,
        ContactFlow,
        CustomerHold,
        CustomerQueue,
        CustomerWhisper,
        OutboundWhisper,
        QueueTransfer
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContactFlowType = ContactFlowType' Lude.Text
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

pattern AgentHold :: ContactFlowType
pattern AgentHold = ContactFlowType' "AGENT_HOLD"

pattern AgentTransfer :: ContactFlowType
pattern AgentTransfer = ContactFlowType' "AGENT_TRANSFER"

pattern AgentWhisper :: ContactFlowType
pattern AgentWhisper = ContactFlowType' "AGENT_WHISPER"

pattern ContactFlow :: ContactFlowType
pattern ContactFlow = ContactFlowType' "CONTACT_FLOW"

pattern CustomerHold :: ContactFlowType
pattern CustomerHold = ContactFlowType' "CUSTOMER_HOLD"

pattern CustomerQueue :: ContactFlowType
pattern CustomerQueue = ContactFlowType' "CUSTOMER_QUEUE"

pattern CustomerWhisper :: ContactFlowType
pattern CustomerWhisper = ContactFlowType' "CUSTOMER_WHISPER"

pattern OutboundWhisper :: ContactFlowType
pattern OutboundWhisper = ContactFlowType' "OUTBOUND_WHISPER"

pattern QueueTransfer :: ContactFlowType
pattern QueueTransfer = ContactFlowType' "QUEUE_TRANSFER"

{-# COMPLETE
  AgentHold,
  AgentTransfer,
  AgentWhisper,
  ContactFlow,
  CustomerHold,
  CustomerQueue,
  CustomerWhisper,
  OutboundWhisper,
  QueueTransfer,
  ContactFlowType'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.ContactFlowType
  ( ContactFlowType
    ( ContactFlowType'
    , ContactFlowTypeContactFlow
    , ContactFlowTypeCustomerQueue
    , ContactFlowTypeCustomerHold
    , ContactFlowTypeCustomerWhisper
    , ContactFlowTypeAgentHold
    , ContactFlowTypeAgentWhisper
    , ContactFlowTypeOutboundWhisper
    , ContactFlowTypeAgentTransfer
    , ContactFlowTypeQueueTransfer
    , fromContactFlowType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContactFlowType = ContactFlowType'{fromContactFlowType ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern ContactFlowTypeContactFlow :: ContactFlowType
pattern ContactFlowTypeContactFlow = ContactFlowType' "CONTACT_FLOW"

pattern ContactFlowTypeCustomerQueue :: ContactFlowType
pattern ContactFlowTypeCustomerQueue = ContactFlowType' "CUSTOMER_QUEUE"

pattern ContactFlowTypeCustomerHold :: ContactFlowType
pattern ContactFlowTypeCustomerHold = ContactFlowType' "CUSTOMER_HOLD"

pattern ContactFlowTypeCustomerWhisper :: ContactFlowType
pattern ContactFlowTypeCustomerWhisper = ContactFlowType' "CUSTOMER_WHISPER"

pattern ContactFlowTypeAgentHold :: ContactFlowType
pattern ContactFlowTypeAgentHold = ContactFlowType' "AGENT_HOLD"

pattern ContactFlowTypeAgentWhisper :: ContactFlowType
pattern ContactFlowTypeAgentWhisper = ContactFlowType' "AGENT_WHISPER"

pattern ContactFlowTypeOutboundWhisper :: ContactFlowType
pattern ContactFlowTypeOutboundWhisper = ContactFlowType' "OUTBOUND_WHISPER"

pattern ContactFlowTypeAgentTransfer :: ContactFlowType
pattern ContactFlowTypeAgentTransfer = ContactFlowType' "AGENT_TRANSFER"

pattern ContactFlowTypeQueueTransfer :: ContactFlowType
pattern ContactFlowTypeQueueTransfer = ContactFlowType' "QUEUE_TRANSFER"

{-# COMPLETE 
  ContactFlowTypeContactFlow,

  ContactFlowTypeCustomerQueue,

  ContactFlowTypeCustomerHold,

  ContactFlowTypeCustomerWhisper,

  ContactFlowTypeAgentHold,

  ContactFlowTypeAgentWhisper,

  ContactFlowTypeOutboundWhisper,

  ContactFlowTypeAgentTransfer,

  ContactFlowTypeQueueTransfer,
  ContactFlowType'
  #-}

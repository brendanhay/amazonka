{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlowType where

import Network.AWS.Prelude

data ContactFlowType
  = AgentHold
  | AgentTransfer
  | AgentWhisper
  | ContactFlow
  | CustomerHold
  | CustomerQueue
  | CustomerWhisper
  | OutboundWhisper
  | QueueTransfer
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

instance FromText ContactFlowType where
  parser =
    takeLowerText >>= \case
      "agent_hold" -> pure AgentHold
      "agent_transfer" -> pure AgentTransfer
      "agent_whisper" -> pure AgentWhisper
      "contact_flow" -> pure ContactFlow
      "customer_hold" -> pure CustomerHold
      "customer_queue" -> pure CustomerQueue
      "customer_whisper" -> pure CustomerWhisper
      "outbound_whisper" -> pure OutboundWhisper
      "queue_transfer" -> pure QueueTransfer
      e ->
        fromTextError $
          "Failure parsing ContactFlowType from value: '" <> e
            <> "'. Accepted values: agent_hold, agent_transfer, agent_whisper, contact_flow, customer_hold, customer_queue, customer_whisper, outbound_whisper, queue_transfer"

instance ToText ContactFlowType where
  toText = \case
    AgentHold -> "AGENT_HOLD"
    AgentTransfer -> "AGENT_TRANSFER"
    AgentWhisper -> "AGENT_WHISPER"
    ContactFlow -> "CONTACT_FLOW"
    CustomerHold -> "CUSTOMER_HOLD"
    CustomerQueue -> "CUSTOMER_QUEUE"
    CustomerWhisper -> "CUSTOMER_WHISPER"
    OutboundWhisper -> "OUTBOUND_WHISPER"
    QueueTransfer -> "QUEUE_TRANSFER"

instance Hashable ContactFlowType

instance NFData ContactFlowType

instance ToByteString ContactFlowType

instance ToQuery ContactFlowType

instance ToHeader ContactFlowType

instance ToJSON ContactFlowType where
  toJSON = toJSONText

instance FromJSON ContactFlowType where
  parseJSON = parseJSONText "ContactFlowType"

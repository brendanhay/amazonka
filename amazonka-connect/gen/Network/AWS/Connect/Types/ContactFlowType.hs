{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlowType
  ( ContactFlowType
      ( ..,
        ContactFlowType_AGENT_HOLD,
        ContactFlowType_AGENT_TRANSFER,
        ContactFlowType_AGENT_WHISPER,
        ContactFlowType_CONTACT_FLOW,
        ContactFlowType_CUSTOMER_HOLD,
        ContactFlowType_CUSTOMER_QUEUE,
        ContactFlowType_CUSTOMER_WHISPER,
        ContactFlowType_OUTBOUND_WHISPER,
        ContactFlowType_QUEUE_TRANSFER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContactFlowType = ContactFlowType'
  { fromContactFlowType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ContactFlowType_AGENT_HOLD :: ContactFlowType
pattern ContactFlowType_AGENT_HOLD = ContactFlowType' "AGENT_HOLD"

pattern ContactFlowType_AGENT_TRANSFER :: ContactFlowType
pattern ContactFlowType_AGENT_TRANSFER = ContactFlowType' "AGENT_TRANSFER"

pattern ContactFlowType_AGENT_WHISPER :: ContactFlowType
pattern ContactFlowType_AGENT_WHISPER = ContactFlowType' "AGENT_WHISPER"

pattern ContactFlowType_CONTACT_FLOW :: ContactFlowType
pattern ContactFlowType_CONTACT_FLOW = ContactFlowType' "CONTACT_FLOW"

pattern ContactFlowType_CUSTOMER_HOLD :: ContactFlowType
pattern ContactFlowType_CUSTOMER_HOLD = ContactFlowType' "CUSTOMER_HOLD"

pattern ContactFlowType_CUSTOMER_QUEUE :: ContactFlowType
pattern ContactFlowType_CUSTOMER_QUEUE = ContactFlowType' "CUSTOMER_QUEUE"

pattern ContactFlowType_CUSTOMER_WHISPER :: ContactFlowType
pattern ContactFlowType_CUSTOMER_WHISPER = ContactFlowType' "CUSTOMER_WHISPER"

pattern ContactFlowType_OUTBOUND_WHISPER :: ContactFlowType
pattern ContactFlowType_OUTBOUND_WHISPER = ContactFlowType' "OUTBOUND_WHISPER"

pattern ContactFlowType_QUEUE_TRANSFER :: ContactFlowType
pattern ContactFlowType_QUEUE_TRANSFER = ContactFlowType' "QUEUE_TRANSFER"

{-# COMPLETE
  ContactFlowType_AGENT_HOLD,
  ContactFlowType_AGENT_TRANSFER,
  ContactFlowType_AGENT_WHISPER,
  ContactFlowType_CONTACT_FLOW,
  ContactFlowType_CUSTOMER_HOLD,
  ContactFlowType_CUSTOMER_QUEUE,
  ContactFlowType_CUSTOMER_WHISPER,
  ContactFlowType_OUTBOUND_WHISPER,
  ContactFlowType_QUEUE_TRANSFER,
  ContactFlowType'
  #-}

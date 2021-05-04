{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ContactFlowType = ContactFlowType'
  { fromContactFlowType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

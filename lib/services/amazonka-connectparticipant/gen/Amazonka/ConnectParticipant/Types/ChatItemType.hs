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
-- Module      : Amazonka.ConnectParticipant.Types.ChatItemType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.ChatItemType
  ( ChatItemType
      ( ..,
        ChatItemType_ATTACHMENT,
        ChatItemType_CHAT_ENDED,
        ChatItemType_CONNECTION_ACK,
        ChatItemType_EVENT,
        ChatItemType_MESSAGE,
        ChatItemType_MESSAGE_DELIVERED,
        ChatItemType_MESSAGE_READ,
        ChatItemType_PARTICIPANT_JOINED,
        ChatItemType_PARTICIPANT_LEFT,
        ChatItemType_TRANSFER_FAILED,
        ChatItemType_TRANSFER_SUCCEEDED,
        ChatItemType_TYPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChatItemType = ChatItemType'
  { fromChatItemType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ChatItemType_ATTACHMENT :: ChatItemType
pattern ChatItemType_ATTACHMENT = ChatItemType' "ATTACHMENT"

pattern ChatItemType_CHAT_ENDED :: ChatItemType
pattern ChatItemType_CHAT_ENDED = ChatItemType' "CHAT_ENDED"

pattern ChatItemType_CONNECTION_ACK :: ChatItemType
pattern ChatItemType_CONNECTION_ACK = ChatItemType' "CONNECTION_ACK"

pattern ChatItemType_EVENT :: ChatItemType
pattern ChatItemType_EVENT = ChatItemType' "EVENT"

pattern ChatItemType_MESSAGE :: ChatItemType
pattern ChatItemType_MESSAGE = ChatItemType' "MESSAGE"

pattern ChatItemType_MESSAGE_DELIVERED :: ChatItemType
pattern ChatItemType_MESSAGE_DELIVERED = ChatItemType' "MESSAGE_DELIVERED"

pattern ChatItemType_MESSAGE_READ :: ChatItemType
pattern ChatItemType_MESSAGE_READ = ChatItemType' "MESSAGE_READ"

pattern ChatItemType_PARTICIPANT_JOINED :: ChatItemType
pattern ChatItemType_PARTICIPANT_JOINED = ChatItemType' "PARTICIPANT_JOINED"

pattern ChatItemType_PARTICIPANT_LEFT :: ChatItemType
pattern ChatItemType_PARTICIPANT_LEFT = ChatItemType' "PARTICIPANT_LEFT"

pattern ChatItemType_TRANSFER_FAILED :: ChatItemType
pattern ChatItemType_TRANSFER_FAILED = ChatItemType' "TRANSFER_FAILED"

pattern ChatItemType_TRANSFER_SUCCEEDED :: ChatItemType
pattern ChatItemType_TRANSFER_SUCCEEDED = ChatItemType' "TRANSFER_SUCCEEDED"

pattern ChatItemType_TYPING :: ChatItemType
pattern ChatItemType_TYPING = ChatItemType' "TYPING"

{-# COMPLETE
  ChatItemType_ATTACHMENT,
  ChatItemType_CHAT_ENDED,
  ChatItemType_CONNECTION_ACK,
  ChatItemType_EVENT,
  ChatItemType_MESSAGE,
  ChatItemType_MESSAGE_DELIVERED,
  ChatItemType_MESSAGE_READ,
  ChatItemType_PARTICIPANT_JOINED,
  ChatItemType_PARTICIPANT_LEFT,
  ChatItemType_TRANSFER_FAILED,
  ChatItemType_TRANSFER_SUCCEEDED,
  ChatItemType_TYPING,
  ChatItemType'
  #-}

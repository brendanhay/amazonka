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
-- Module      : Amazonka.IVSChat.Types.ChatTokenCapability
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Types.ChatTokenCapability
  ( ChatTokenCapability
      ( ..,
        ChatTokenCapability_DELETE_MESSAGE,
        ChatTokenCapability_DISCONNECT_USER,
        ChatTokenCapability_SEND_MESSAGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChatTokenCapability = ChatTokenCapability'
  { fromChatTokenCapability ::
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

pattern ChatTokenCapability_DELETE_MESSAGE :: ChatTokenCapability
pattern ChatTokenCapability_DELETE_MESSAGE = ChatTokenCapability' "DELETE_MESSAGE"

pattern ChatTokenCapability_DISCONNECT_USER :: ChatTokenCapability
pattern ChatTokenCapability_DISCONNECT_USER = ChatTokenCapability' "DISCONNECT_USER"

pattern ChatTokenCapability_SEND_MESSAGE :: ChatTokenCapability
pattern ChatTokenCapability_SEND_MESSAGE = ChatTokenCapability' "SEND_MESSAGE"

{-# COMPLETE
  ChatTokenCapability_DELETE_MESSAGE,
  ChatTokenCapability_DISCONNECT_USER,
  ChatTokenCapability_SEND_MESSAGE,
  ChatTokenCapability'
  #-}

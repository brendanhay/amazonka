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
-- Module      : Amazonka.IoTWireless.Types.MessageType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.MessageType
  ( MessageType
      ( ..,
        MessageType_CUSTOM_COMMAND_ID_GET,
        MessageType_CUSTOM_COMMAND_ID_NOTIFY,
        MessageType_CUSTOM_COMMAND_ID_RESP,
        MessageType_CUSTOM_COMMAND_ID_SET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk device message type. Default value is
-- @CUSTOM_COMMAND_ID_NOTIFY@.
newtype MessageType = MessageType'
  { fromMessageType ::
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

pattern MessageType_CUSTOM_COMMAND_ID_GET :: MessageType
pattern MessageType_CUSTOM_COMMAND_ID_GET = MessageType' "CUSTOM_COMMAND_ID_GET"

pattern MessageType_CUSTOM_COMMAND_ID_NOTIFY :: MessageType
pattern MessageType_CUSTOM_COMMAND_ID_NOTIFY = MessageType' "CUSTOM_COMMAND_ID_NOTIFY"

pattern MessageType_CUSTOM_COMMAND_ID_RESP :: MessageType
pattern MessageType_CUSTOM_COMMAND_ID_RESP = MessageType' "CUSTOM_COMMAND_ID_RESP"

pattern MessageType_CUSTOM_COMMAND_ID_SET :: MessageType
pattern MessageType_CUSTOM_COMMAND_ID_SET = MessageType' "CUSTOM_COMMAND_ID_SET"

{-# COMPLETE
  MessageType_CUSTOM_COMMAND_ID_GET,
  MessageType_CUSTOM_COMMAND_ID_NOTIFY,
  MessageType_CUSTOM_COMMAND_ID_RESP,
  MessageType_CUSTOM_COMMAND_ID_SET,
  MessageType'
  #-}

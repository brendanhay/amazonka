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
-- Module      : Network.AWS.KMS.Types.ConnectionStateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ConnectionStateType
  ( ConnectionStateType
      ( ..,
        ConnectionStateType_CONNECTED,
        ConnectionStateType_CONNECTING,
        ConnectionStateType_DISCONNECTED,
        ConnectionStateType_DISCONNECTING,
        ConnectionStateType_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConnectionStateType = ConnectionStateType'
  { fromConnectionStateType ::
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

pattern ConnectionStateType_CONNECTED :: ConnectionStateType
pattern ConnectionStateType_CONNECTED = ConnectionStateType' "CONNECTED"

pattern ConnectionStateType_CONNECTING :: ConnectionStateType
pattern ConnectionStateType_CONNECTING = ConnectionStateType' "CONNECTING"

pattern ConnectionStateType_DISCONNECTED :: ConnectionStateType
pattern ConnectionStateType_DISCONNECTED = ConnectionStateType' "DISCONNECTED"

pattern ConnectionStateType_DISCONNECTING :: ConnectionStateType
pattern ConnectionStateType_DISCONNECTING = ConnectionStateType' "DISCONNECTING"

pattern ConnectionStateType_FAILED :: ConnectionStateType
pattern ConnectionStateType_FAILED = ConnectionStateType' "FAILED"

{-# COMPLETE
  ConnectionStateType_CONNECTED,
  ConnectionStateType_CONNECTING,
  ConnectionStateType_DISCONNECTED,
  ConnectionStateType_DISCONNECTING,
  ConnectionStateType_FAILED,
  ConnectionStateType'
  #-}

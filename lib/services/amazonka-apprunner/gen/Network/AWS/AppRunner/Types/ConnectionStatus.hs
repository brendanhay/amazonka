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
-- Module      : Network.AWS.AppRunner.Types.ConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppRunner.Types.ConnectionStatus
  ( ConnectionStatus
      ( ..,
        ConnectionStatus_AVAILABLE,
        ConnectionStatus_DELETED,
        ConnectionStatus_ERROR,
        ConnectionStatus_PENDING_HANDSHAKE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConnectionStatus = ConnectionStatus'
  { fromConnectionStatus ::
      Core.Text
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

pattern ConnectionStatus_AVAILABLE :: ConnectionStatus
pattern ConnectionStatus_AVAILABLE = ConnectionStatus' "AVAILABLE"

pattern ConnectionStatus_DELETED :: ConnectionStatus
pattern ConnectionStatus_DELETED = ConnectionStatus' "DELETED"

pattern ConnectionStatus_ERROR :: ConnectionStatus
pattern ConnectionStatus_ERROR = ConnectionStatus' "ERROR"

pattern ConnectionStatus_PENDING_HANDSHAKE :: ConnectionStatus
pattern ConnectionStatus_PENDING_HANDSHAKE = ConnectionStatus' "PENDING_HANDSHAKE"

{-# COMPLETE
  ConnectionStatus_AVAILABLE,
  ConnectionStatus_DELETED,
  ConnectionStatus_ERROR,
  ConnectionStatus_PENDING_HANDSHAKE,
  ConnectionStatus'
  #-}

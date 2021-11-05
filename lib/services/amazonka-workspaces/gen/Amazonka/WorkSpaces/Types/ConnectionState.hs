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
-- Module      : Amazonka.WorkSpaces.Types.ConnectionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ConnectionState
  ( ConnectionState
      ( ..,
        ConnectionState_CONNECTED,
        ConnectionState_DISCONNECTED,
        ConnectionState_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ConnectionState = ConnectionState'
  { fromConnectionState ::
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

pattern ConnectionState_CONNECTED :: ConnectionState
pattern ConnectionState_CONNECTED = ConnectionState' "CONNECTED"

pattern ConnectionState_DISCONNECTED :: ConnectionState
pattern ConnectionState_DISCONNECTED = ConnectionState' "DISCONNECTED"

pattern ConnectionState_UNKNOWN :: ConnectionState
pattern ConnectionState_UNKNOWN = ConnectionState' "UNKNOWN"

{-# COMPLETE
  ConnectionState_CONNECTED,
  ConnectionState_DISCONNECTED,
  ConnectionState_UNKNOWN,
  ConnectionState'
  #-}

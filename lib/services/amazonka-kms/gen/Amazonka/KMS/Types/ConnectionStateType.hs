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
-- Module      : Amazonka.KMS.Types.ConnectionStateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.ConnectionStateType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionStateType = ConnectionStateType'
  { fromConnectionStateType ::
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

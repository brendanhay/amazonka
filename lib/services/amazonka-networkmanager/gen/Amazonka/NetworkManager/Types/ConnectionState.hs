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
-- Module      : Amazonka.NetworkManager.Types.ConnectionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectionState
  ( ConnectionState
      ( ..,
        ConnectionState_AVAILABLE,
        ConnectionState_DELETING,
        ConnectionState_PENDING,
        ConnectionState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionState = ConnectionState'
  { fromConnectionState ::
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

pattern ConnectionState_AVAILABLE :: ConnectionState
pattern ConnectionState_AVAILABLE = ConnectionState' "AVAILABLE"

pattern ConnectionState_DELETING :: ConnectionState
pattern ConnectionState_DELETING = ConnectionState' "DELETING"

pattern ConnectionState_PENDING :: ConnectionState
pattern ConnectionState_PENDING = ConnectionState' "PENDING"

pattern ConnectionState_UPDATING :: ConnectionState
pattern ConnectionState_UPDATING = ConnectionState' "UPDATING"

{-# COMPLETE
  ConnectionState_AVAILABLE,
  ConnectionState_DELETING,
  ConnectionState_PENDING,
  ConnectionState_UPDATING,
  ConnectionState'
  #-}

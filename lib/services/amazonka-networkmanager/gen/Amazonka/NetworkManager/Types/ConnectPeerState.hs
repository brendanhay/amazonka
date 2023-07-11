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
-- Module      : Amazonka.NetworkManager.Types.ConnectPeerState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectPeerState
  ( ConnectPeerState
      ( ..,
        ConnectPeerState_AVAILABLE,
        ConnectPeerState_CREATING,
        ConnectPeerState_DELETING,
        ConnectPeerState_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectPeerState = ConnectPeerState'
  { fromConnectPeerState ::
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

pattern ConnectPeerState_AVAILABLE :: ConnectPeerState
pattern ConnectPeerState_AVAILABLE = ConnectPeerState' "AVAILABLE"

pattern ConnectPeerState_CREATING :: ConnectPeerState
pattern ConnectPeerState_CREATING = ConnectPeerState' "CREATING"

pattern ConnectPeerState_DELETING :: ConnectPeerState
pattern ConnectPeerState_DELETING = ConnectPeerState' "DELETING"

pattern ConnectPeerState_FAILED :: ConnectPeerState
pattern ConnectPeerState_FAILED = ConnectPeerState' "FAILED"

{-# COMPLETE
  ConnectPeerState_AVAILABLE,
  ConnectPeerState_CREATING,
  ConnectPeerState_DELETING,
  ConnectPeerState_FAILED,
  ConnectPeerState'
  #-}

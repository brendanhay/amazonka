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
-- Module      : Amazonka.NetworkManager.Types.ConnectPeerAssociationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectPeerAssociationState
  ( ConnectPeerAssociationState
      ( ..,
        ConnectPeerAssociationState_AVAILABLE,
        ConnectPeerAssociationState_DELETED,
        ConnectPeerAssociationState_DELETING,
        ConnectPeerAssociationState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ConnectPeerAssociationState = ConnectPeerAssociationState'
  { fromConnectPeerAssociationState ::
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

pattern ConnectPeerAssociationState_AVAILABLE :: ConnectPeerAssociationState
pattern ConnectPeerAssociationState_AVAILABLE = ConnectPeerAssociationState' "AVAILABLE"

pattern ConnectPeerAssociationState_DELETED :: ConnectPeerAssociationState
pattern ConnectPeerAssociationState_DELETED = ConnectPeerAssociationState' "DELETED"

pattern ConnectPeerAssociationState_DELETING :: ConnectPeerAssociationState
pattern ConnectPeerAssociationState_DELETING = ConnectPeerAssociationState' "DELETING"

pattern ConnectPeerAssociationState_PENDING :: ConnectPeerAssociationState
pattern ConnectPeerAssociationState_PENDING = ConnectPeerAssociationState' "PENDING"

{-# COMPLETE
  ConnectPeerAssociationState_AVAILABLE,
  ConnectPeerAssociationState_DELETED,
  ConnectPeerAssociationState_DELETING,
  ConnectPeerAssociationState_PENDING,
  ConnectPeerAssociationState'
  #-}

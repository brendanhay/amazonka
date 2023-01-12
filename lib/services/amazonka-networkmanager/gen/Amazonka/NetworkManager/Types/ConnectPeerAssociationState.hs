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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectPeerAssociationState = ConnectPeerAssociationState'
  { fromConnectPeerAssociationState ::
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

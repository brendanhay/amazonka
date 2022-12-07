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
-- Module      : Amazonka.NetworkManager.Types.TransitGatewayRegistrationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.TransitGatewayRegistrationState
  ( TransitGatewayRegistrationState
      ( ..,
        TransitGatewayRegistrationState_AVAILABLE,
        TransitGatewayRegistrationState_DELETED,
        TransitGatewayRegistrationState_DELETING,
        TransitGatewayRegistrationState_FAILED,
        TransitGatewayRegistrationState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayRegistrationState = TransitGatewayRegistrationState'
  { fromTransitGatewayRegistrationState ::
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

pattern TransitGatewayRegistrationState_AVAILABLE :: TransitGatewayRegistrationState
pattern TransitGatewayRegistrationState_AVAILABLE = TransitGatewayRegistrationState' "AVAILABLE"

pattern TransitGatewayRegistrationState_DELETED :: TransitGatewayRegistrationState
pattern TransitGatewayRegistrationState_DELETED = TransitGatewayRegistrationState' "DELETED"

pattern TransitGatewayRegistrationState_DELETING :: TransitGatewayRegistrationState
pattern TransitGatewayRegistrationState_DELETING = TransitGatewayRegistrationState' "DELETING"

pattern TransitGatewayRegistrationState_FAILED :: TransitGatewayRegistrationState
pattern TransitGatewayRegistrationState_FAILED = TransitGatewayRegistrationState' "FAILED"

pattern TransitGatewayRegistrationState_PENDING :: TransitGatewayRegistrationState
pattern TransitGatewayRegistrationState_PENDING = TransitGatewayRegistrationState' "PENDING"

{-# COMPLETE
  TransitGatewayRegistrationState_AVAILABLE,
  TransitGatewayRegistrationState_DELETED,
  TransitGatewayRegistrationState_DELETING,
  TransitGatewayRegistrationState_FAILED,
  TransitGatewayRegistrationState_PENDING,
  TransitGatewayRegistrationState'
  #-}

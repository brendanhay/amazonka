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
-- Module      : Amazonka.EC2.Types.TransitGatewayPolicyTableState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPolicyTableState
  ( TransitGatewayPolicyTableState
      ( ..,
        TransitGatewayPolicyTableState_Available,
        TransitGatewayPolicyTableState_Deleted,
        TransitGatewayPolicyTableState_Deleting,
        TransitGatewayPolicyTableState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayPolicyTableState = TransitGatewayPolicyTableState'
  { fromTransitGatewayPolicyTableState ::
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

pattern TransitGatewayPolicyTableState_Available :: TransitGatewayPolicyTableState
pattern TransitGatewayPolicyTableState_Available = TransitGatewayPolicyTableState' "available"

pattern TransitGatewayPolicyTableState_Deleted :: TransitGatewayPolicyTableState
pattern TransitGatewayPolicyTableState_Deleted = TransitGatewayPolicyTableState' "deleted"

pattern TransitGatewayPolicyTableState_Deleting :: TransitGatewayPolicyTableState
pattern TransitGatewayPolicyTableState_Deleting = TransitGatewayPolicyTableState' "deleting"

pattern TransitGatewayPolicyTableState_Pending :: TransitGatewayPolicyTableState
pattern TransitGatewayPolicyTableState_Pending = TransitGatewayPolicyTableState' "pending"

{-# COMPLETE
  TransitGatewayPolicyTableState_Available,
  TransitGatewayPolicyTableState_Deleted,
  TransitGatewayPolicyTableState_Deleting,
  TransitGatewayPolicyTableState_Pending,
  TransitGatewayPolicyTableState'
  #-}

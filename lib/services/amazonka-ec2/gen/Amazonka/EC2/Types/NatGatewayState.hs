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
-- Module      : Amazonka.EC2.Types.NatGatewayState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NatGatewayState
  ( NatGatewayState
      ( ..,
        NatGatewayState_Available,
        NatGatewayState_Deleted,
        NatGatewayState_Deleting,
        NatGatewayState_Failed,
        NatGatewayState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype NatGatewayState = NatGatewayState'
  { fromNatGatewayState ::
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

pattern NatGatewayState_Available :: NatGatewayState
pattern NatGatewayState_Available = NatGatewayState' "available"

pattern NatGatewayState_Deleted :: NatGatewayState
pattern NatGatewayState_Deleted = NatGatewayState' "deleted"

pattern NatGatewayState_Deleting :: NatGatewayState
pattern NatGatewayState_Deleting = NatGatewayState' "deleting"

pattern NatGatewayState_Failed :: NatGatewayState
pattern NatGatewayState_Failed = NatGatewayState' "failed"

pattern NatGatewayState_Pending :: NatGatewayState
pattern NatGatewayState_Pending = NatGatewayState' "pending"

{-# COMPLETE
  NatGatewayState_Available,
  NatGatewayState_Deleted,
  NatGatewayState_Deleting,
  NatGatewayState_Failed,
  NatGatewayState_Pending,
  NatGatewayState'
  #-}

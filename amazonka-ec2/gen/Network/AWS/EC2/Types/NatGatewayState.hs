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
-- Module      : Network.AWS.EC2.Types.NatGatewayState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGatewayState
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype NatGatewayState = NatGatewayState'
  { fromNatGatewayState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

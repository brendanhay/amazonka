{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype NatGatewayState = NatGatewayState'
  { fromNatGatewayState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

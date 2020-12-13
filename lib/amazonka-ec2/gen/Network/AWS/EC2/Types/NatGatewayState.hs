{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NatGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGatewayState
  ( NatGatewayState
      ( NatGatewayState',
        NGSPending,
        NGSFailed,
        NGSAvailable,
        NGSDeleting,
        NGSDeleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NatGatewayState = NatGatewayState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern NGSPending :: NatGatewayState
pattern NGSPending = NatGatewayState' "pending"

pattern NGSFailed :: NatGatewayState
pattern NGSFailed = NatGatewayState' "failed"

pattern NGSAvailable :: NatGatewayState
pattern NGSAvailable = NatGatewayState' "available"

pattern NGSDeleting :: NatGatewayState
pattern NGSDeleting = NatGatewayState' "deleting"

pattern NGSDeleted :: NatGatewayState
pattern NGSDeleted = NatGatewayState' "deleted"

{-# COMPLETE
  NGSPending,
  NGSFailed,
  NGSAvailable,
  NGSDeleting,
  NGSDeleted,
  NatGatewayState'
  #-}

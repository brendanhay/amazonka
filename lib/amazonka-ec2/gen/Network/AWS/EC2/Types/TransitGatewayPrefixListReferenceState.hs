{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
  ( TransitGatewayPrefixListReferenceState
      ( TransitGatewayPrefixListReferenceState',
        TGPLRSPending,
        TGPLRSAvailable,
        TGPLRSModifying,
        TGPLRSDeleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitGatewayPrefixListReferenceState = TransitGatewayPrefixListReferenceState' Lude.Text
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

pattern TGPLRSPending :: TransitGatewayPrefixListReferenceState
pattern TGPLRSPending = TransitGatewayPrefixListReferenceState' "pending"

pattern TGPLRSAvailable :: TransitGatewayPrefixListReferenceState
pattern TGPLRSAvailable = TransitGatewayPrefixListReferenceState' "available"

pattern TGPLRSModifying :: TransitGatewayPrefixListReferenceState
pattern TGPLRSModifying = TransitGatewayPrefixListReferenceState' "modifying"

pattern TGPLRSDeleting :: TransitGatewayPrefixListReferenceState
pattern TGPLRSDeleting = TransitGatewayPrefixListReferenceState' "deleting"

{-# COMPLETE
  TGPLRSPending,
  TGPLRSAvailable,
  TGPLRSModifying,
  TGPLRSDeleting,
  TransitGatewayPrefixListReferenceState'
  #-}

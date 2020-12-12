{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociationStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociationStateCode
  ( RouteTableAssociationStateCode
      ( RouteTableAssociationStateCode',
        RTASCAssociated,
        RTASCAssociating,
        RTASCDisassociated,
        RTASCDisassociating,
        RTASCFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RouteTableAssociationStateCode = RouteTableAssociationStateCode' Lude.Text
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

pattern RTASCAssociated :: RouteTableAssociationStateCode
pattern RTASCAssociated = RouteTableAssociationStateCode' "associated"

pattern RTASCAssociating :: RouteTableAssociationStateCode
pattern RTASCAssociating = RouteTableAssociationStateCode' "associating"

pattern RTASCDisassociated :: RouteTableAssociationStateCode
pattern RTASCDisassociated = RouteTableAssociationStateCode' "disassociated"

pattern RTASCDisassociating :: RouteTableAssociationStateCode
pattern RTASCDisassociating = RouteTableAssociationStateCode' "disassociating"

pattern RTASCFailed :: RouteTableAssociationStateCode
pattern RTASCFailed = RouteTableAssociationStateCode' "failed"

{-# COMPLETE
  RTASCAssociated,
  RTASCAssociating,
  RTASCDisassociated,
  RTASCDisassociating,
  RTASCFailed,
  RouteTableAssociationStateCode'
  #-}

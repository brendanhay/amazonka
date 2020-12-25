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
        RouteTableAssociationStateCodeAssociating,
        RouteTableAssociationStateCodeAssociated,
        RouteTableAssociationStateCodeDisassociating,
        RouteTableAssociationStateCodeDisassociated,
        RouteTableAssociationStateCodeFailed,
        fromRouteTableAssociationStateCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RouteTableAssociationStateCode = RouteTableAssociationStateCode'
  { fromRouteTableAssociationStateCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern RouteTableAssociationStateCodeAssociating :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCodeAssociating = RouteTableAssociationStateCode' "associating"

pattern RouteTableAssociationStateCodeAssociated :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCodeAssociated = RouteTableAssociationStateCode' "associated"

pattern RouteTableAssociationStateCodeDisassociating :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCodeDisassociating = RouteTableAssociationStateCode' "disassociating"

pattern RouteTableAssociationStateCodeDisassociated :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCodeDisassociated = RouteTableAssociationStateCode' "disassociated"

pattern RouteTableAssociationStateCodeFailed :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCodeFailed = RouteTableAssociationStateCode' "failed"

{-# COMPLETE
  RouteTableAssociationStateCodeAssociating,
  RouteTableAssociationStateCodeAssociated,
  RouteTableAssociationStateCodeDisassociating,
  RouteTableAssociationStateCodeDisassociated,
  RouteTableAssociationStateCodeFailed,
  RouteTableAssociationStateCode'
  #-}

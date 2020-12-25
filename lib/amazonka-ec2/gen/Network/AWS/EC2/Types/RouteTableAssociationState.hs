{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociationState
  ( RouteTableAssociationState (..),

    -- * Smart constructor
    mkRouteTableAssociationState,

    -- * Lenses
    rtasState,
    rtasStatusMessage,
  )
where

import qualified Network.AWS.EC2.Types.RouteTableAssociationStateCode as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of an association between a route table and a subnet or gateway.
--
-- /See:/ 'mkRouteTableAssociationState' smart constructor.
data RouteTableAssociationState = RouteTableAssociationState'
  { -- | The state of the association.
    state :: Core.Maybe Types.RouteTableAssociationStateCode,
    -- | The status message, if applicable.
    statusMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RouteTableAssociationState' value with any optional fields omitted.
mkRouteTableAssociationState ::
  RouteTableAssociationState
mkRouteTableAssociationState =
  RouteTableAssociationState'
    { state = Core.Nothing,
      statusMessage = Core.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtasState :: Lens.Lens' RouteTableAssociationState (Core.Maybe Types.RouteTableAssociationStateCode)
rtasState = Lens.field @"state"
{-# DEPRECATED rtasState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The status message, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtasStatusMessage :: Lens.Lens' RouteTableAssociationState (Core.Maybe Types.String)
rtasStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED rtasStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Core.FromXML RouteTableAssociationState where
  parseXML x =
    RouteTableAssociationState'
      Core.<$> (x Core..@? "state") Core.<*> (x Core..@? "statusMessage")

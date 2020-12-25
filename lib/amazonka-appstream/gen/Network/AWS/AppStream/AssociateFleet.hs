{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.AssociateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified fleet with the specified stack.
module Network.AWS.AppStream.AssociateFleet
  ( -- * Creating a request
    AssociateFleet (..),
    mkAssociateFleet,

    -- ** Request lenses
    afFleetName,
    afStackName,

    -- * Destructuring the response
    AssociateFleetResponse (..),
    mkAssociateFleetResponse,

    -- ** Response lenses
    afrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateFleet' smart constructor.
data AssociateFleet = AssociateFleet'
  { -- | The name of the fleet.
    fleetName :: Types.String,
    -- | The name of the stack.
    stackName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateFleet' value with any optional fields omitted.
mkAssociateFleet ::
  -- | 'fleetName'
  Types.String ->
  -- | 'stackName'
  Types.String ->
  AssociateFleet
mkAssociateFleet fleetName stackName =
  AssociateFleet' {fleetName, stackName}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFleetName :: Lens.Lens' AssociateFleet Types.String
afFleetName = Lens.field @"fleetName"
{-# DEPRECATED afFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afStackName :: Lens.Lens' AssociateFleet Types.String
afStackName = Lens.field @"stackName"
{-# DEPRECATED afStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.FromJSON AssociateFleet where
  toJSON AssociateFleet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetName" Core..= fleetName),
            Core.Just ("StackName" Core..= stackName)
          ]
      )

instance Core.AWSRequest AssociateFleet where
  type Rs AssociateFleet = AssociateFleetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.AssociateFleet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateFleetResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateFleetResponse' smart constructor.
newtype AssociateFleetResponse = AssociateFleetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateFleetResponse' value with any optional fields omitted.
mkAssociateFleetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateFleetResponse
mkAssociateFleetResponse responseStatus =
  AssociateFleetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afrrsResponseStatus :: Lens.Lens' AssociateFleetResponse Core.Int
afrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED afrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

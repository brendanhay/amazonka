{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DisassociateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified fleet from the specified stack.
module Network.AWS.AppStream.DisassociateFleet
  ( -- * Creating a request
    DisassociateFleet (..),
    mkDisassociateFleet,

    -- ** Request lenses
    dfFleetName,
    dfStackName,

    -- * Destructuring the response
    DisassociateFleetResponse (..),
    mkDisassociateFleetResponse,

    -- ** Response lenses
    dfrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateFleet' smart constructor.
data DisassociateFleet = DisassociateFleet'
  { -- | The name of the fleet.
    fleetName :: Types.String,
    -- | The name of the stack.
    stackName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateFleet' value with any optional fields omitted.
mkDisassociateFleet ::
  -- | 'fleetName'
  Types.String ->
  -- | 'stackName'
  Types.String ->
  DisassociateFleet
mkDisassociateFleet fleetName stackName =
  DisassociateFleet' {fleetName, stackName}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFleetName :: Lens.Lens' DisassociateFleet Types.String
dfFleetName = Lens.field @"fleetName"
{-# DEPRECATED dfFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfStackName :: Lens.Lens' DisassociateFleet Types.String
dfStackName = Lens.field @"stackName"
{-# DEPRECATED dfStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.FromJSON DisassociateFleet where
  toJSON DisassociateFleet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetName" Core..= fleetName),
            Core.Just ("StackName" Core..= stackName)
          ]
      )

instance Core.AWSRequest DisassociateFleet where
  type Rs DisassociateFleet = DisassociateFleetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DisassociateFleet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFleetResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateFleetResponse' smart constructor.
newtype DisassociateFleetResponse = DisassociateFleetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateFleetResponse' value with any optional fields omitted.
mkDisassociateFleetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateFleetResponse
mkDisassociateFleetResponse responseStatus =
  DisassociateFleetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DisassociateFleetResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

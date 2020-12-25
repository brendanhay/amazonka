{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified destination, and eventually disables all the subscription filters that publish to it. This operation does not delete the physical resource encapsulated by the destination.
module Network.AWS.CloudWatchLogs.DeleteDestination
  ( -- * Creating a request
    DeleteDestination (..),
    mkDeleteDestination,

    -- ** Request lenses
    ddDestinationName,

    -- * Destructuring the response
    DeleteDestinationResponse (..),
    mkDeleteDestinationResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDestination' smart constructor.
newtype DeleteDestination = DeleteDestination'
  { -- | The name of the destination.
    destinationName :: Types.DestinationName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDestination' value with any optional fields omitted.
mkDeleteDestination ::
  -- | 'destinationName'
  Types.DestinationName ->
  DeleteDestination
mkDeleteDestination destinationName =
  DeleteDestination' {destinationName}

-- | The name of the destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDestinationName :: Lens.Lens' DeleteDestination Types.DestinationName
ddDestinationName = Lens.field @"destinationName"
{-# DEPRECATED ddDestinationName "Use generic-lens or generic-optics with 'destinationName' instead." #-}

instance Core.FromJSON DeleteDestination where
  toJSON DeleteDestination {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("destinationName" Core..= destinationName)]
      )

instance Core.AWSRequest DeleteDestination where
  type Rs DeleteDestination = DeleteDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DeleteDestination")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteDestinationResponse'

-- | /See:/ 'mkDeleteDestinationResponse' smart constructor.
data DeleteDestinationResponse = DeleteDestinationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDestinationResponse' value with any optional fields omitted.
mkDeleteDestinationResponse ::
  DeleteDestinationResponse
mkDeleteDestinationResponse = DeleteDestinationResponse'

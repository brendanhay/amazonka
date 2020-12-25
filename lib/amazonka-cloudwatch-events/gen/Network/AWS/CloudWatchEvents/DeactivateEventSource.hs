{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeactivateEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this operation to temporarily stop receiving events from the specified partner event source. The matching event bus is not deleted.
--
-- When you deactivate a partner event source, the source goes into PENDING state. If it remains in PENDING state for more than two weeks, it is deleted.
-- To activate a deactivated partner event source, use 'ActivateEventSource' .
module Network.AWS.CloudWatchEvents.DeactivateEventSource
  ( -- * Creating a request
    DeactivateEventSource (..),
    mkDeactivateEventSource,

    -- ** Request lenses
    desfName,

    -- * Destructuring the response
    DeactivateEventSourceResponse (..),
    mkDeactivateEventSourceResponse,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeactivateEventSource' smart constructor.
newtype DeactivateEventSource = DeactivateEventSource'
  { -- | The name of the partner event source to deactivate.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivateEventSource' value with any optional fields omitted.
mkDeactivateEventSource ::
  -- | 'name'
  Types.Name ->
  DeactivateEventSource
mkDeactivateEventSource name = DeactivateEventSource' {name}

-- | The name of the partner event source to deactivate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desfName :: Lens.Lens' DeactivateEventSource Types.Name
desfName = Lens.field @"name"
{-# DEPRECATED desfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeactivateEventSource where
  toJSON DeactivateEventSource {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeactivateEventSource where
  type Rs DeactivateEventSource = DeactivateEventSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DeactivateEventSource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeactivateEventSourceResponse'

-- | /See:/ 'mkDeactivateEventSourceResponse' smart constructor.
data DeactivateEventSourceResponse = DeactivateEventSourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivateEventSourceResponse' value with any optional fields omitted.
mkDeactivateEventSourceResponse ::
  DeactivateEventSourceResponse
mkDeactivateEventSourceResponse = DeactivateEventSourceResponse'

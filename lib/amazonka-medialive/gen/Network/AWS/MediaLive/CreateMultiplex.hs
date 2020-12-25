{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new multiplex.
module Network.AWS.MediaLive.CreateMultiplex
  ( -- * Creating a request
    CreateMultiplex (..),
    mkCreateMultiplex,

    -- ** Request lenses
    cmRequestId,
    cmMultiplexSettings,
    cmAvailabilityZones,
    cmName,
    cmTags,

    -- * Destructuring the response
    CreateMultiplexResponse (..),
    mkCreateMultiplexResponse,

    -- ** Response lenses
    cmrrsMultiplex,
    cmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a multiplex.
--
-- /See:/ 'mkCreateMultiplex' smart constructor.
data CreateMultiplex = CreateMultiplex'
  { -- | Unique request ID. This prevents retries from creating multiple
    --
    -- resources.
    requestId :: Core.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Types.MultiplexSettings,
    -- | A list of availability zones for the multiplex. You must specify exactly two.
    availabilityZones :: [Core.Text],
    -- | Name of multiplex.
    name :: Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplex' value with any optional fields omitted.
mkCreateMultiplex ::
  -- | 'requestId'
  Core.Text ->
  -- | 'multiplexSettings'
  Types.MultiplexSettings ->
  -- | 'name'
  Core.Text ->
  CreateMultiplex
mkCreateMultiplex requestId multiplexSettings name =
  CreateMultiplex'
    { requestId,
      multiplexSettings,
      availabilityZones = Core.mempty,
      name,
      tags = Core.Nothing
    }

-- | Unique request ID. This prevents retries from creating multiple
--
-- resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRequestId :: Lens.Lens' CreateMultiplex Core.Text
cmRequestId = Lens.field @"requestId"
{-# DEPRECATED cmRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmMultiplexSettings :: Lens.Lens' CreateMultiplex Types.MultiplexSettings
cmMultiplexSettings = Lens.field @"multiplexSettings"
{-# DEPRECATED cmMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A list of availability zones for the multiplex. You must specify exactly two.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmAvailabilityZones :: Lens.Lens' CreateMultiplex [Core.Text]
cmAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED cmAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Name of multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' CreateMultiplex Core.Text
cmName = Lens.field @"name"
{-# DEPRECATED cmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTags :: Lens.Lens' CreateMultiplex (Core.Maybe (Core.HashMap Core.Text Core.Text))
cmTags = Lens.field @"tags"
{-# DEPRECATED cmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateMultiplex where
  toJSON CreateMultiplex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("requestId" Core..= requestId),
            Core.Just ("multiplexSettings" Core..= multiplexSettings),
            Core.Just ("availabilityZones" Core..= availabilityZones),
            Core.Just ("name" Core..= name),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateMultiplex where
  type Rs CreateMultiplex = CreateMultiplexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/prod/multiplexes",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMultiplexResponse'
            Core.<$> (x Core..:? "multiplex") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for CreateMultiplexResponse
--
-- /See:/ 'mkCreateMultiplexResponse' smart constructor.
data CreateMultiplexResponse = CreateMultiplexResponse'
  { -- | The newly created multiplex.
    multiplex :: Core.Maybe Types.Multiplex,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplexResponse' value with any optional fields omitted.
mkCreateMultiplexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateMultiplexResponse
mkCreateMultiplexResponse responseStatus =
  CreateMultiplexResponse'
    { multiplex = Core.Nothing,
      responseStatus
    }

-- | The newly created multiplex.
--
-- /Note:/ Consider using 'multiplex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsMultiplex :: Lens.Lens' CreateMultiplexResponse (Core.Maybe Types.Multiplex)
cmrrsMultiplex = Lens.field @"multiplex"
{-# DEPRECATED cmrrsMultiplex "Use generic-lens or generic-optics with 'multiplex' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsResponseStatus :: Lens.Lens' CreateMultiplexResponse Core.Int
cmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

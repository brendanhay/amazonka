{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeStudio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details for the specified Amazon EMR Studio including ID, Name, VPC, Studio access URL, and so on.
module Network.AWS.EMR.DescribeStudio
  ( -- * Creating a request
    DescribeStudio (..),
    mkDescribeStudio,

    -- ** Request lenses
    dsStudioId,

    -- * Destructuring the response
    DescribeStudioResponse (..),
    mkDescribeStudioResponse,

    -- ** Response lenses
    drsStudio,
    drsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStudio' smart constructor.
newtype DescribeStudio = DescribeStudio'
  { -- | The Amazon EMR Studio ID.
    studioId :: Types.StudioId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStudio' value with any optional fields omitted.
mkDescribeStudio ::
  -- | 'studioId'
  Types.StudioId ->
  DescribeStudio
mkDescribeStudio studioId = DescribeStudio' {studioId}

-- | The Amazon EMR Studio ID.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStudioId :: Lens.Lens' DescribeStudio Types.StudioId
dsStudioId = Lens.field @"studioId"
{-# DEPRECATED dsStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

instance Core.FromJSON DescribeStudio where
  toJSON DescribeStudio {..} =
    Core.object
      (Core.catMaybes [Core.Just ("StudioId" Core..= studioId)])

instance Core.AWSRequest DescribeStudio where
  type Rs DescribeStudio = DescribeStudioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.DescribeStudio")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStudioResponse'
            Core.<$> (x Core..:? "Studio") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStudioResponse' smart constructor.
data DescribeStudioResponse = DescribeStudioResponse'
  { -- | The Amazon EMR Studio details.
    studio :: Core.Maybe Types.Studio,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStudioResponse' value with any optional fields omitted.
mkDescribeStudioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStudioResponse
mkDescribeStudioResponse responseStatus =
  DescribeStudioResponse' {studio = Core.Nothing, responseStatus}

-- | The Amazon EMR Studio details.
--
-- /Note:/ Consider using 'studio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStudio :: Lens.Lens' DescribeStudioResponse (Core.Maybe Types.Studio)
drsStudio = Lens.field @"studio"
{-# DEPRECATED drsStudio "Use generic-lens or generic-optics with 'studio' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeStudioResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

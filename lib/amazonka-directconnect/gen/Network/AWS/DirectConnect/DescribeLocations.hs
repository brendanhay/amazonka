{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS Direct Connect locations in the current AWS Region. These are the locations that can be selected when calling 'CreateConnection' or 'CreateInterconnect' .
module Network.AWS.DirectConnect.DescribeLocations
  ( -- * Creating a request
    DescribeLocations (..),
    mkDescribeLocations,

    -- * Destructuring the response
    DescribeLocationsResponse (..),
    mkDescribeLocationsResponse,

    -- ** Response lenses
    dlrrsLocations,
    dlrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLocations' smart constructor.
data DescribeLocations = DescribeLocations'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocations' value with any optional fields omitted.
mkDescribeLocations ::
  DescribeLocations
mkDescribeLocations = DescribeLocations'

instance Core.FromJSON DescribeLocations where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeLocations where
  type Rs DescribeLocations = DescribeLocationsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.DescribeLocations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationsResponse'
            Core.<$> (x Core..:? "locations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLocationsResponse' smart constructor.
data DescribeLocationsResponse = DescribeLocationsResponse'
  { -- | The locations.
    locations :: Core.Maybe [Types.Location],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocationsResponse' value with any optional fields omitted.
mkDescribeLocationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLocationsResponse
mkDescribeLocationsResponse responseStatus =
  DescribeLocationsResponse'
    { locations = Core.Nothing,
      responseStatus
    }

-- | The locations.
--
-- /Note:/ Consider using 'locations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsLocations :: Lens.Lens' DescribeLocationsResponse (Core.Maybe [Types.Location])
dlrrsLocations = Lens.field @"locations"
{-# DEPRECATED dlrrsLocations "Use generic-lens or generic-optics with 'locations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DescribeLocationsResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

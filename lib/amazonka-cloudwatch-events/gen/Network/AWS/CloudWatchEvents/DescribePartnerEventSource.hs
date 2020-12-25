{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribePartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list details about a partner event source that they have created. AWS customers do not use this operation. Instead, AWS customers can use 'DescribeEventSource' to see details about a partner event source that is shared with them.
module Network.AWS.CloudWatchEvents.DescribePartnerEventSource
  ( -- * Creating a request
    DescribePartnerEventSource (..),
    mkDescribePartnerEventSource,

    -- ** Request lenses
    dpesfName,

    -- * Destructuring the response
    DescribePartnerEventSourceResponse (..),
    mkDescribePartnerEventSourceResponse,

    -- ** Response lenses
    dpesrrsArn,
    dpesrrsName,
    dpesrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePartnerEventSource' smart constructor.
newtype DescribePartnerEventSource = DescribePartnerEventSource'
  { -- | The name of the event source to display.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePartnerEventSource' value with any optional fields omitted.
mkDescribePartnerEventSource ::
  -- | 'name'
  Types.Name ->
  DescribePartnerEventSource
mkDescribePartnerEventSource name =
  DescribePartnerEventSource' {name}

-- | The name of the event source to display.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesfName :: Lens.Lens' DescribePartnerEventSource Types.Name
dpesfName = Lens.field @"name"
{-# DEPRECATED dpesfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribePartnerEventSource where
  toJSON DescribePartnerEventSource {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DescribePartnerEventSource where
  type
    Rs DescribePartnerEventSource =
      DescribePartnerEventSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DescribePartnerEventSource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePartnerEventSourceResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribePartnerEventSourceResponse' smart constructor.
data DescribePartnerEventSourceResponse = DescribePartnerEventSourceResponse'
  { -- | The ARN of the event source.
    arn :: Core.Maybe Types.String,
    -- | The name of the event source.
    name :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePartnerEventSourceResponse' value with any optional fields omitted.
mkDescribePartnerEventSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePartnerEventSourceResponse
mkDescribePartnerEventSourceResponse responseStatus =
  DescribePartnerEventSourceResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The ARN of the event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesrrsArn :: Lens.Lens' DescribePartnerEventSourceResponse (Core.Maybe Types.String)
dpesrrsArn = Lens.field @"arn"
{-# DEPRECATED dpesrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesrrsName :: Lens.Lens' DescribePartnerEventSourceResponse (Core.Maybe Types.String)
dpesrrsName = Lens.field @"name"
{-# DEPRECATED dpesrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesrrsResponseStatus :: Lens.Lens' DescribePartnerEventSourceResponse Core.Int
dpesrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current state of the specified instance identifier. It tracks the instance while it is being created and returns an error status if applicable.
--
-- If an instance is not created successfully, the instance status reason field returns details relevant to the reason. The instance in a failed state is returned only for 24 hours after the CreateInstance API was invoked.
module Network.AWS.Connect.DescribeInstance
  ( -- * Creating a request
    DescribeInstance (..),
    mkDescribeInstance,

    -- ** Request lenses
    diInstanceId,

    -- * Destructuring the response
    DescribeInstanceResponse (..),
    mkDescribeInstanceResponse,

    -- ** Response lenses
    dirrsInstance,
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstance' smart constructor.
newtype DescribeInstance = DescribeInstance'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstance' value with any optional fields omitted.
mkDescribeInstance ::
  -- | 'instanceId'
  Types.InstanceId ->
  DescribeInstance
mkDescribeInstance instanceId = DescribeInstance' {instanceId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DescribeInstance Types.InstanceId
diInstanceId = Lens.field @"instanceId"
{-# DEPRECATED diInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.AWSRequest DescribeInstance where
  type Rs DescribeInstance = DescribeInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/instance/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceResponse'
            Core.<$> (x Core..:? "Instance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeInstanceResponse' smart constructor.
data DescribeInstanceResponse = DescribeInstanceResponse'
  { -- | The name of the instance.
    instance' :: Core.Maybe Types.Instance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeInstanceResponse' value with any optional fields omitted.
mkDescribeInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceResponse
mkDescribeInstanceResponse responseStatus =
  DescribeInstanceResponse'
    { instance' = Core.Nothing,
      responseStatus
    }

-- | The name of the instance.
--
-- /Note:/ Consider using 'instance'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsInstance :: Lens.Lens' DescribeInstanceResponse (Core.Maybe Types.Instance)
dirrsInstance = Lens.field @"instance'"
{-# DEPRECATED dirrsInstance "Use generic-lens or generic-optics with 'instance'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DescribeInstanceResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

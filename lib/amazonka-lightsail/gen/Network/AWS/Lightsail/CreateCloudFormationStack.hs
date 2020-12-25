{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateCloudFormationStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation stack, which creates a new Amazon EC2 instance from an exported Amazon Lightsail snapshot. This operation results in a CloudFormation stack record that can be used to track the AWS CloudFormation stack created. Use the @get cloud formation stack records@ operation to get a list of the CloudFormation stacks created.
--
-- /Important:/ Wait until after your new Amazon EC2 instance is created before running the @create cloud formation stack@ operation again with the same export snapshot record.
module Network.AWS.Lightsail.CreateCloudFormationStack
  ( -- * Creating a request
    CreateCloudFormationStack (..),
    mkCreateCloudFormationStack,

    -- ** Request lenses
    ccfsInstances,

    -- * Destructuring the response
    CreateCloudFormationStackResponse (..),
    mkCreateCloudFormationStackResponse,

    -- ** Response lenses
    ccfsrrsOperations,
    ccfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCloudFormationStack' smart constructor.
newtype CreateCloudFormationStack = CreateCloudFormationStack'
  { -- | An array of parameters that will be used to create the new Amazon EC2 instance. You can only pass one instance entry at a time in this array. You will get an invalid parameter error if you pass more than one instance entry in this array.
    instances :: [Types.InstanceEntry]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFormationStack' value with any optional fields omitted.
mkCreateCloudFormationStack ::
  CreateCloudFormationStack
mkCreateCloudFormationStack =
  CreateCloudFormationStack' {instances = Core.mempty}

-- | An array of parameters that will be used to create the new Amazon EC2 instance. You can only pass one instance entry at a time in this array. You will get an invalid parameter error if you pass more than one instance entry in this array.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfsInstances :: Lens.Lens' CreateCloudFormationStack [Types.InstanceEntry]
ccfsInstances = Lens.field @"instances"
{-# DEPRECATED ccfsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Core.FromJSON CreateCloudFormationStack where
  toJSON CreateCloudFormationStack {..} =
    Core.object
      (Core.catMaybes [Core.Just ("instances" Core..= instances)])

instance Core.AWSRequest CreateCloudFormationStack where
  type
    Rs CreateCloudFormationStack =
      CreateCloudFormationStackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.CreateCloudFormationStack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationStackResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCloudFormationStackResponse' smart constructor.
data CreateCloudFormationStackResponse = CreateCloudFormationStackResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateCloudFormationStackResponse' value with any optional fields omitted.
mkCreateCloudFormationStackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCloudFormationStackResponse
mkCreateCloudFormationStackResponse responseStatus =
  CreateCloudFormationStackResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfsrrsOperations :: Lens.Lens' CreateCloudFormationStackResponse (Core.Maybe [Types.Operation])
ccfsrrsOperations = Lens.field @"operations"
{-# DEPRECATED ccfsrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfsrrsResponseStatus :: Lens.Lens' CreateCloudFormationStackResponse Core.Int
ccfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resource descriptions for running and deleted stacks. If @StackName@ is specified, all the associated resources that are part of the stack are returned. If @PhysicalResourceId@ is specified, the associated resources of the stack that the resource belongs to are returned.
--
-- For deleted stacks, @DescribeStackResources@ returns resource information for up to 90 days after the stack has been deleted.
-- You must specify either @StackName@ or @PhysicalResourceId@ , but not both. In addition, you can specify @LogicalResourceId@ to filter the returned result. For more information about resources, the @LogicalResourceId@ and @PhysicalResourceId@ , go to the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/ AWS CloudFormation User Guide> .
module Network.AWS.CloudFormation.DescribeStackResources
  ( -- * Creating a request
    DescribeStackResources (..),
    mkDescribeStackResources,

    -- ** Request lenses
    dsrLogicalResourceId,
    dsrPhysicalResourceId,
    dsrStackName,

    -- * Destructuring the response
    DescribeStackResourcesResponse (..),
    mkDescribeStackResourcesResponse,

    -- ** Response lenses
    dsrrrsStackResources,
    dsrrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for 'DescribeStackResources' action.
--
-- /See:/ 'mkDescribeStackResources' smart constructor.
data DescribeStackResources = DescribeStackResources'
  { -- | The logical name of the resource as specified in the template.
    --
    -- Default: There is no default value.
    logicalResourceId :: Core.Maybe Types.LogicalResourceId,
    -- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
    --
    -- For example, for an Amazon Elastic Compute Cloud (EC2) instance, @PhysicalResourceId@ corresponds to the @InstanceId@ . You can pass the EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the instance belongs to and what other resources are part of the stack.
    -- Required: Conditional. If you do not specify @PhysicalResourceId@ , you must specify @StackName@ .
    -- Default: There is no default value.
    physicalResourceId :: Core.Maybe Types.PhysicalResourceId,
    -- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
    --
    --
    --     * Running stacks: You can specify either the stack's name or its unique stack ID.
    --
    --
    --     * Deleted stacks: You must specify the unique stack ID.
    --
    --
    -- Default: There is no default value.
    -- Required: Conditional. If you do not specify @StackName@ , you must specify @PhysicalResourceId@ .
    stackName :: Core.Maybe Types.StackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackResources' value with any optional fields omitted.
mkDescribeStackResources ::
  DescribeStackResources
mkDescribeStackResources =
  DescribeStackResources'
    { logicalResourceId = Core.Nothing,
      physicalResourceId = Core.Nothing,
      stackName = Core.Nothing
    }

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrLogicalResourceId :: Lens.Lens' DescribeStackResources (Core.Maybe Types.LogicalResourceId)
dsrLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED dsrLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- For example, for an Amazon Elastic Compute Cloud (EC2) instance, @PhysicalResourceId@ corresponds to the @InstanceId@ . You can pass the EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the instance belongs to and what other resources are part of the stack.
-- Required: Conditional. If you do not specify @PhysicalResourceId@ , you must specify @StackName@ .
-- Default: There is no default value.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrPhysicalResourceId :: Lens.Lens' DescribeStackResources (Core.Maybe Types.PhysicalResourceId)
dsrPhysicalResourceId = Lens.field @"physicalResourceId"
{-# DEPRECATED dsrPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
-- Required: Conditional. If you do not specify @StackName@ , you must specify @PhysicalResourceId@ .
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrStackName :: Lens.Lens' DescribeStackResources (Core.Maybe Types.StackName)
dsrStackName = Lens.field @"stackName"
{-# DEPRECATED dsrStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.AWSRequest DescribeStackResources where
  type Rs DescribeStackResources = DescribeStackResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeStackResources")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "LogicalResourceId" Core.<$> logicalResourceId)
                Core.<> ( Core.toQueryValue "PhysicalResourceId"
                            Core.<$> physicalResourceId
                        )
                Core.<> (Core.toQueryValue "StackName" Core.<$> stackName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeStackResourcesResult"
      ( \s h x ->
          DescribeStackResourcesResponse'
            Core.<$> (x Core..@? "StackResources" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for a 'DescribeStackResources' action.
--
-- /See:/ 'mkDescribeStackResourcesResponse' smart constructor.
data DescribeStackResourcesResponse = DescribeStackResourcesResponse'
  { -- | A list of @StackResource@ structures.
    stackResources :: Core.Maybe [Types.StackResource],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStackResourcesResponse' value with any optional fields omitted.
mkDescribeStackResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackResourcesResponse
mkDescribeStackResourcesResponse responseStatus =
  DescribeStackResourcesResponse'
    { stackResources = Core.Nothing,
      responseStatus
    }

-- | A list of @StackResource@ structures.
--
-- /Note:/ Consider using 'stackResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsStackResources :: Lens.Lens' DescribeStackResourcesResponse (Core.Maybe [Types.StackResource])
dsrrrsStackResources = Lens.field @"stackResources"
{-# DEPRECATED dsrrrsStackResources "Use generic-lens or generic-optics with 'stackResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsResponseStatus :: Lens.Lens' DescribeStackResourcesResponse Core.Int
dsrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

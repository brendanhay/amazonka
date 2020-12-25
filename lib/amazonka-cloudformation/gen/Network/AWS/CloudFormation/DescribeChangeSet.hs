{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the inputs for the change set and a list of changes that AWS CloudFormation will make if you execute the change set. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html Updating Stacks Using Change Sets> in the AWS CloudFormation User Guide.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeChangeSet
  ( -- * Creating a request
    DescribeChangeSet (..),
    mkDescribeChangeSet,

    -- ** Request lenses
    dcsfChangeSetName,
    dcsfNextToken,
    dcsfStackName,

    -- * Destructuring the response
    DescribeChangeSetResponse (..),
    mkDescribeChangeSetResponse,

    -- ** Response lenses
    dcsrfrsCapabilities,
    dcsrfrsChangeSetId,
    dcsrfrsChangeSetName,
    dcsrfrsChanges,
    dcsrfrsCreationTime,
    dcsrfrsDescription,
    dcsrfrsExecutionStatus,
    dcsrfrsIncludeNestedStacks,
    dcsrfrsNextToken,
    dcsrfrsNotificationARNs,
    dcsrfrsParameters,
    dcsrfrsParentChangeSetId,
    dcsrfrsRollbackConfiguration,
    dcsrfrsRootChangeSetId,
    dcsrfrsStackId,
    dcsrfrsStackName,
    dcsrfrsStatus,
    dcsrfrsStatusReason,
    dcsrfrsTags,
    dcsrfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DescribeChangeSet' action.
--
-- /See:/ 'mkDescribeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'
  { -- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
    changeSetName :: Types.ChangeSetNameOrId,
    -- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
    nextToken :: Core.Maybe Types.NextToken,
    -- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
    stackName :: Core.Maybe Types.StackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChangeSet' value with any optional fields omitted.
mkDescribeChangeSet ::
  -- | 'changeSetName'
  Types.ChangeSetNameOrId ->
  DescribeChangeSet
mkDescribeChangeSet changeSetName =
  DescribeChangeSet'
    { changeSetName,
      nextToken = Core.Nothing,
      stackName = Core.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfChangeSetName :: Lens.Lens' DescribeChangeSet Types.ChangeSetNameOrId
dcsfChangeSetName = Lens.field @"changeSetName"
{-# DEPRECATED dcsfChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfNextToken :: Lens.Lens' DescribeChangeSet (Core.Maybe Types.NextToken)
dcsfNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcsfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfStackName :: Lens.Lens' DescribeChangeSet (Core.Maybe Types.StackName)
dcsfStackName = Lens.field @"stackName"
{-# DEPRECATED dcsfStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.AWSRequest DescribeChangeSet where
  type Rs DescribeChangeSet = DescribeChangeSetResponse
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
            ( Core.pure ("Action", "DescribeChangeSet")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "ChangeSetName" changeSetName)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "StackName" Core.<$> stackName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeChangeSetResult"
      ( \s h x ->
          DescribeChangeSetResponse'
            Core.<$> (x Core..@? "Capabilities" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "ChangeSetId")
            Core.<*> (x Core..@? "ChangeSetName")
            Core.<*> (x Core..@? "Changes" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "CreationTime")
            Core.<*> (x Core..@? "Description")
            Core.<*> (x Core..@? "ExecutionStatus")
            Core.<*> (x Core..@? "IncludeNestedStacks")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "NotificationARNs"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "ParentChangeSetId")
            Core.<*> (x Core..@? "RollbackConfiguration")
            Core.<*> (x Core..@? "RootChangeSetId")
            Core.<*> (x Core..@? "StackId")
            Core.<*> (x Core..@? "StackName")
            Core.<*> (x Core..@ "Status")
            Core.<*> (x Core..@? "StatusReason")
            Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeChangeSet where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"changes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output for the 'DescribeChangeSet' action.
--
-- /See:/ 'mkDescribeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { -- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
    capabilities :: Core.Maybe [Types.Capability],
    -- | The ARN of the change set.
    changeSetId :: Core.Maybe Types.ChangeSetId,
    -- | The name of the change set.
    changeSetName :: Core.Maybe Types.ChangeSetName,
    -- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
    changes :: Core.Maybe [Types.Change],
    -- | The start time when the change set was created, in UTC.
    creationTime :: Core.Maybe Core.UTCTime,
    -- | Information about the change set.
    description :: Core.Maybe Types.Description,
    -- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
    executionStatus :: Core.Maybe Types.ExecutionStatus,
    -- | Verifies if @IncludeNestedStacks@ is set to @True@ .
    includeNestedStacks :: Core.Maybe Core.Bool,
    -- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
    notificationARNs :: Core.Maybe [Types.NotificationARN],
    -- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
    parameters :: Core.Maybe [Types.Parameter],
    -- | Specifies the change set ID of the parent change set in the current nested change set hierarchy.
    parentChangeSetId :: Core.Maybe Types.ChangeSetId,
    -- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
    rollbackConfiguration :: Core.Maybe Types.RollbackConfiguration,
    -- | Specifies the change set ID of the root change set in the current nested change set hierarchy.
    rootChangeSetId :: Core.Maybe Types.ChangeSetId,
    -- | The ARN of the stack that is associated with the change set.
    stackId :: Core.Maybe Types.StackId,
    -- | The name of the stack that is associated with the change set.
    stackName :: Core.Maybe Types.StackName,
    -- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
    status :: Types.ChangeSetStatus,
    -- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
    statusReason :: Core.Maybe Types.ChangeSetStatusReason,
    -- | If you execute the change set, the tags that will be associated with the stack.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeChangeSetResponse' value with any optional fields omitted.
mkDescribeChangeSetResponse ::
  -- | 'status'
  Types.ChangeSetStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeChangeSetResponse
mkDescribeChangeSetResponse status responseStatus =
  DescribeChangeSetResponse'
    { capabilities = Core.Nothing,
      changeSetId = Core.Nothing,
      changeSetName = Core.Nothing,
      changes = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      executionStatus = Core.Nothing,
      includeNestedStacks = Core.Nothing,
      nextToken = Core.Nothing,
      notificationARNs = Core.Nothing,
      parameters = Core.Nothing,
      parentChangeSetId = Core.Nothing,
      rollbackConfiguration = Core.Nothing,
      rootChangeSetId = Core.Nothing,
      stackId = Core.Nothing,
      stackName = Core.Nothing,
      status,
      statusReason = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsCapabilities :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe [Types.Capability])
dcsrfrsCapabilities = Lens.field @"capabilities"
{-# DEPRECATED dcsrfrsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The ARN of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.ChangeSetId)
dcsrfrsChangeSetId = Lens.field @"changeSetId"
{-# DEPRECATED dcsrfrsChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | The name of the change set.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsChangeSetName :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.ChangeSetName)
dcsrfrsChangeSetName = Lens.field @"changeSetName"
{-# DEPRECATED dcsrfrsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
--
-- /Note:/ Consider using 'changes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsChanges :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe [Types.Change])
dcsrfrsChanges = Lens.field @"changes"
{-# DEPRECATED dcsrfrsChanges "Use generic-lens or generic-optics with 'changes' instead." #-}

-- | The start time when the change set was created, in UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsCreationTime :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Core.UTCTime)
dcsrfrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dcsrfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Information about the change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsDescription :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.Description)
dcsrfrsDescription = Lens.field @"description"
{-# DEPRECATED dcsrfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- /Note:/ Consider using 'executionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsExecutionStatus :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.ExecutionStatus)
dcsrfrsExecutionStatus = Lens.field @"executionStatus"
{-# DEPRECATED dcsrfrsExecutionStatus "Use generic-lens or generic-optics with 'executionStatus' instead." #-}

-- | Verifies if @IncludeNestedStacks@ is set to @True@ .
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsIncludeNestedStacks :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Core.Bool)
dcsrfrsIncludeNestedStacks = Lens.field @"includeNestedStacks"
{-# DEPRECATED dcsrfrsIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsNextToken :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.NextToken)
dcsrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcsrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsNotificationARNs :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe [Types.NotificationARN])
dcsrfrsNotificationARNs = Lens.field @"notificationARNs"
{-# DEPRECATED dcsrfrsNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsParameters :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe [Types.Parameter])
dcsrfrsParameters = Lens.field @"parameters"
{-# DEPRECATED dcsrfrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Specifies the change set ID of the parent change set in the current nested change set hierarchy.
--
-- /Note:/ Consider using 'parentChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsParentChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.ChangeSetId)
dcsrfrsParentChangeSetId = Lens.field @"parentChangeSetId"
{-# DEPRECATED dcsrfrsParentChangeSetId "Use generic-lens or generic-optics with 'parentChangeSetId' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsRollbackConfiguration :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.RollbackConfiguration)
dcsrfrsRollbackConfiguration = Lens.field @"rollbackConfiguration"
{-# DEPRECATED dcsrfrsRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | Specifies the change set ID of the root change set in the current nested change set hierarchy.
--
-- /Note:/ Consider using 'rootChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsRootChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.ChangeSetId)
dcsrfrsRootChangeSetId = Lens.field @"rootChangeSetId"
{-# DEPRECATED dcsrfrsRootChangeSetId "Use generic-lens or generic-optics with 'rootChangeSetId' instead." #-}

-- | The ARN of the stack that is associated with the change set.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsStackId :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.StackId)
dcsrfrsStackId = Lens.field @"stackId"
{-# DEPRECATED dcsrfrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The name of the stack that is associated with the change set.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsStackName :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.StackName)
dcsrfrsStackName = Lens.field @"stackName"
{-# DEPRECATED dcsrfrsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsStatus :: Lens.Lens' DescribeChangeSetResponse Types.ChangeSetStatus
dcsrfrsStatus = Lens.field @"status"
{-# DEPRECATED dcsrfrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsStatusReason :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe Types.ChangeSetStatusReason)
dcsrfrsStatusReason = Lens.field @"statusReason"
{-# DEPRECATED dcsrfrsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | If you execute the change set, the tags that will be associated with the stack.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsTags :: Lens.Lens' DescribeChangeSetResponse (Core.Maybe [Types.Tag])
dcsrfrsTags = Lens.field @"tags"
{-# DEPRECATED dcsrfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrfrsResponseStatus :: Lens.Lens' DescribeChangeSetResponse Core.Int
dcsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

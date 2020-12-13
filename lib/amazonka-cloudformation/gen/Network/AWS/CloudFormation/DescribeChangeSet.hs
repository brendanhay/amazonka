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
    dcsChangeSetName,
    dcsNextToken,
    dcsStackName,

    -- * Destructuring the response
    DescribeChangeSetResponse (..),
    mkDescribeChangeSetResponse,

    -- ** Response lenses
    dcsfrsCreationTime,
    dcsfrsStatus,
    dcsfrsParentChangeSetId,
    dcsfrsChanges,
    dcsfrsNotificationARNs,
    dcsfrsChangeSetName,
    dcsfrsExecutionStatus,
    dcsfrsChangeSetId,
    dcsfrsIncludeNestedStacks,
    dcsfrsNextToken,
    dcsfrsRootChangeSetId,
    dcsfrsParameters,
    dcsfrsStatusReason,
    dcsfrsStackId,
    dcsfrsDescription,
    dcsfrsCapabilities,
    dcsfrsRollbackConfiguration,
    dcsfrsTags,
    dcsfrsStackName,
    dcsfrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DescribeChangeSet' action.
--
-- /See:/ 'mkDescribeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'
  { -- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
    changeSetName :: Lude.Text,
    -- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChangeSet' with the minimum fields required to make a request.
--
-- * 'changeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want to describe.
-- * 'nextToken' - A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
-- * 'stackName' - If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
mkDescribeChangeSet ::
  -- | 'changeSetName'
  Lude.Text ->
  DescribeChangeSet
mkDescribeChangeSet pChangeSetName_ =
  DescribeChangeSet'
    { changeSetName = pChangeSetName_,
      nextToken = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsChangeSetName :: Lens.Lens' DescribeChangeSet Lude.Text
dcsChangeSetName = Lens.lens (changeSetName :: DescribeChangeSet -> Lude.Text) (\s a -> s {changeSetName = a} :: DescribeChangeSet)
{-# DEPRECATED dcsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsNextToken :: Lens.Lens' DescribeChangeSet (Lude.Maybe Lude.Text)
dcsNextToken = Lens.lens (nextToken :: DescribeChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeChangeSet)
{-# DEPRECATED dcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStackName :: Lens.Lens' DescribeChangeSet (Lude.Maybe Lude.Text)
dcsStackName = Lens.lens (stackName :: DescribeChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeChangeSet)
{-# DEPRECATED dcsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager DescribeChangeSet where
  page rq rs
    | Page.stop (rs Lens.^. dcsfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcsfrsChanges) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcsNextToken Lens..~ rs Lens.^. dcsfrsNextToken

instance Lude.AWSRequest DescribeChangeSet where
  type Rs DescribeChangeSet = DescribeChangeSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeChangeSetResult"
      ( \s h x ->
          DescribeChangeSetResponse'
            Lude.<$> (x Lude..@? "CreationTime")
            Lude.<*> (x Lude..@ "Status")
            Lude.<*> (x Lude..@? "ParentChangeSetId")
            Lude.<*> ( x Lude..@? "Changes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "NotificationARNs" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "ChangeSetName")
            Lude.<*> (x Lude..@? "ExecutionStatus")
            Lude.<*> (x Lude..@? "ChangeSetId")
            Lude.<*> (x Lude..@? "IncludeNestedStacks")
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (x Lude..@? "RootChangeSetId")
            Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "StatusReason")
            Lude.<*> (x Lude..@? "StackId")
            Lude.<*> (x Lude..@? "Description")
            Lude.<*> ( x Lude..@? "Capabilities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "RollbackConfiguration")
            Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "StackName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeChangeSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeChangeSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeChangeSet where
  toQuery DescribeChangeSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeChangeSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "ChangeSetName" Lude.=: changeSetName,
        "NextToken" Lude.=: nextToken,
        "StackName" Lude.=: stackName
      ]

-- | The output for the 'DescribeChangeSet' action.
--
-- /See:/ 'mkDescribeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { -- | The start time when the change set was created, in UTC.
    creationTime :: Lude.Maybe Lude.DateTime,
    -- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
    status :: ChangeSetStatus,
    -- | Specifies the change set ID of the parent change set in the current nested change set hierarchy.
    parentChangeSetId :: Lude.Maybe Lude.Text,
    -- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
    changes :: Lude.Maybe [Change],
    -- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
    notificationARNs :: Lude.Maybe [Lude.Text],
    -- | The name of the change set.
    changeSetName :: Lude.Maybe Lude.Text,
    -- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
    executionStatus :: Lude.Maybe ExecutionStatus,
    -- | The ARN of the change set.
    changeSetId :: Lude.Maybe Lude.Text,
    -- | Verifies if @IncludeNestedStacks@ is set to @True@ .
    includeNestedStacks :: Lude.Maybe Lude.Bool,
    -- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specifies the change set ID of the root change set in the current nested change set hierarchy.
    rootChangeSetId :: Lude.Maybe Lude.Text,
    -- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
    parameters :: Lude.Maybe [Parameter],
    -- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
    statusReason :: Lude.Maybe Lude.Text,
    -- | The ARN of the stack that is associated with the change set.
    stackId :: Lude.Maybe Lude.Text,
    -- | Information about the change set.
    description :: Lude.Maybe Lude.Text,
    -- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
    capabilities :: Lude.Maybe [Capability],
    -- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
    rollbackConfiguration :: Lude.Maybe RollbackConfiguration,
    -- | If you execute the change set, the tags that will be associated with the stack.
    tags :: Lude.Maybe [Tag],
    -- | The name of the stack that is associated with the change set.
    stackName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChangeSetResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The start time when the change set was created, in UTC.
-- * 'status' - The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
-- * 'parentChangeSetId' - Specifies the change set ID of the parent change set in the current nested change set hierarchy.
-- * 'changes' - A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
-- * 'notificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
-- * 'changeSetName' - The name of the change set.
-- * 'executionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
-- * 'changeSetId' - The ARN of the change set.
-- * 'includeNestedStacks' - Verifies if @IncludeNestedStacks@ is set to @True@ .
-- * 'nextToken' - If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
-- * 'rootChangeSetId' - Specifies the change set ID of the root change set in the current nested change set hierarchy.
-- * 'parameters' - A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
-- * 'statusReason' - A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
-- * 'stackId' - The ARN of the stack that is associated with the change set.
-- * 'description' - Information about the change set.
-- * 'capabilities' - If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
-- * 'rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
-- * 'tags' - If you execute the change set, the tags that will be associated with the stack.
-- * 'stackName' - The name of the stack that is associated with the change set.
-- * 'responseStatus' - The response status code.
mkDescribeChangeSetResponse ::
  -- | 'status'
  ChangeSetStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeChangeSetResponse
mkDescribeChangeSetResponse pStatus_ pResponseStatus_ =
  DescribeChangeSetResponse'
    { creationTime = Lude.Nothing,
      status = pStatus_,
      parentChangeSetId = Lude.Nothing,
      changes = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      changeSetName = Lude.Nothing,
      executionStatus = Lude.Nothing,
      changeSetId = Lude.Nothing,
      includeNestedStacks = Lude.Nothing,
      nextToken = Lude.Nothing,
      rootChangeSetId = Lude.Nothing,
      parameters = Lude.Nothing,
      statusReason = Lude.Nothing,
      stackId = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      rollbackConfiguration = Lude.Nothing,
      tags = Lude.Nothing,
      stackName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The start time when the change set was created, in UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsCreationTime :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.DateTime)
dcsfrsCreationTime = Lens.lens (creationTime :: DescribeChangeSetResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTime = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsStatus :: Lens.Lens' DescribeChangeSetResponse ChangeSetStatus
dcsfrsStatus = Lens.lens (status :: DescribeChangeSetResponse -> ChangeSetStatus) (\s a -> s {status = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the change set ID of the parent change set in the current nested change set hierarchy.
--
-- /Note:/ Consider using 'parentChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsParentChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsParentChangeSetId = Lens.lens (parentChangeSetId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentChangeSetId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsParentChangeSetId "Use generic-lens or generic-optics with 'parentChangeSetId' instead." #-}

-- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
--
-- /Note:/ Consider using 'changes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsChanges :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Change])
dcsfrsChanges = Lens.lens (changes :: DescribeChangeSetResponse -> Lude.Maybe [Change]) (\s a -> s {changes = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsChanges "Use generic-lens or generic-optics with 'changes' instead." #-}

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsNotificationARNs :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Lude.Text])
dcsfrsNotificationARNs = Lens.lens (notificationARNs :: DescribeChangeSetResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | The name of the change set.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsChangeSetName :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsChangeSetName = Lens.lens (changeSetName :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeSetName = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- /Note:/ Consider using 'executionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsExecutionStatus :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe ExecutionStatus)
dcsfrsExecutionStatus = Lens.lens (executionStatus :: DescribeChangeSetResponse -> Lude.Maybe ExecutionStatus) (\s a -> s {executionStatus = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsExecutionStatus "Use generic-lens or generic-optics with 'executionStatus' instead." #-}

-- | The ARN of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsChangeSetId = Lens.lens (changeSetId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | Verifies if @IncludeNestedStacks@ is set to @True@ .
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsIncludeNestedStacks :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Bool)
dcsfrsIncludeNestedStacks = Lens.lens (includeNestedStacks :: DescribeChangeSetResponse -> Lude.Maybe Lude.Bool) (\s a -> s {includeNestedStacks = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsNextToken :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsNextToken = Lens.lens (nextToken :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the change set ID of the root change set in the current nested change set hierarchy.
--
-- /Note:/ Consider using 'rootChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsRootChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsRootChangeSetId = Lens.lens (rootChangeSetId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {rootChangeSetId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsRootChangeSetId "Use generic-lens or generic-optics with 'rootChangeSetId' instead." #-}

-- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsParameters :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Parameter])
dcsfrsParameters = Lens.lens (parameters :: DescribeChangeSetResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsStatusReason :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsStatusReason = Lens.lens (statusReason :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The ARN of the stack that is associated with the change set.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsStackId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsStackId = Lens.lens (stackId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Information about the change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsDescription :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsDescription = Lens.lens (description :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsCapabilities :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Capability])
dcsfrsCapabilities = Lens.lens (capabilities :: DescribeChangeSetResponse -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsRollbackConfiguration :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe RollbackConfiguration)
dcsfrsRollbackConfiguration = Lens.lens (rollbackConfiguration :: DescribeChangeSetResponse -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | If you execute the change set, the tags that will be associated with the stack.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsTags :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Tag])
dcsfrsTags = Lens.lens (tags :: DescribeChangeSetResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the stack that is associated with the change set.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsStackName :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcsfrsStackName = Lens.lens (stackName :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfrsResponseStatus :: Lens.Lens' DescribeChangeSetResponse Lude.Int
dcsfrsResponseStatus = Lens.lens (responseStatus :: DescribeChangeSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

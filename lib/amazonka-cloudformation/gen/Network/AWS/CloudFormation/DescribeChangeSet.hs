{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    desNextToken,
    desStackName,
    desChangeSetName,

    -- * Destructuring the response
    DescribeChangeSetResponse (..),
    mkDescribeChangeSetResponse,

    -- ** Response lenses
    dcscrsCreationTime,
    dcscrsParentChangeSetId,
    dcscrsChanges,
    dcscrsNotificationARNs,
    dcscrsChangeSetName,
    dcscrsExecutionStatus,
    dcscrsChangeSetId,
    dcscrsIncludeNestedStacks,
    dcscrsNextToken,
    dcscrsRootChangeSetId,
    dcscrsParameters,
    dcscrsStatusReason,
    dcscrsStackId,
    dcscrsDescription,
    dcscrsCapabilities,
    dcscrsRollbackConfiguration,
    dcscrsTags,
    dcscrsStackName,
    dcscrsResponseStatus,
    dcscrsStatus,
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
  { nextToken ::
      Lude.Maybe Lude.Text,
    stackName :: Lude.Maybe Lude.Text,
    changeSetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
    { nextToken = Lude.Nothing,
      stackName = Lude.Nothing,
      changeSetName = pChangeSetName_
    }

-- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desNextToken :: Lens.Lens' DescribeChangeSet (Lude.Maybe Lude.Text)
desNextToken = Lens.lens (nextToken :: DescribeChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeChangeSet)
{-# DEPRECATED desNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desStackName :: Lens.Lens' DescribeChangeSet (Lude.Maybe Lude.Text)
desStackName = Lens.lens (stackName :: DescribeChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeChangeSet)
{-# DEPRECATED desStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desChangeSetName :: Lens.Lens' DescribeChangeSet Lude.Text
desChangeSetName = Lens.lens (changeSetName :: DescribeChangeSet -> Lude.Text) (\s a -> s {changeSetName = a} :: DescribeChangeSet)
{-# DEPRECATED desChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

instance Page.AWSPager DescribeChangeSet where
  page rq rs
    | Page.stop (rs Lens.^. dcscrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcscrsChanges) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& desNextToken Lens..~ rs Lens.^. dcscrsNextToken

instance Lude.AWSRequest DescribeChangeSet where
  type Rs DescribeChangeSet = DescribeChangeSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeChangeSetResult"
      ( \s h x ->
          DescribeChangeSetResponse'
            Lude.<$> (x Lude..@? "CreationTime")
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
            Lude.<*> (x Lude..@ "Status")
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
        "NextToken" Lude.=: nextToken,
        "StackName" Lude.=: stackName,
        "ChangeSetName" Lude.=: changeSetName
      ]

-- | The output for the 'DescribeChangeSet' action.
--
-- /See:/ 'mkDescribeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { creationTime ::
      Lude.Maybe Lude.DateTime,
    parentChangeSetId ::
      Lude.Maybe Lude.Text,
    changes :: Lude.Maybe [Change],
    notificationARNs ::
      Lude.Maybe [Lude.Text],
    changeSetName :: Lude.Maybe Lude.Text,
    executionStatus ::
      Lude.Maybe ExecutionStatus,
    changeSetId :: Lude.Maybe Lude.Text,
    includeNestedStacks ::
      Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    rootChangeSetId :: Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe [Parameter],
    statusReason :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    capabilities :: Lude.Maybe [Capability],
    rollbackConfiguration ::
      Lude.Maybe RollbackConfiguration,
    tags :: Lude.Maybe [Tag],
    stackName :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    status :: ChangeSetStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChangeSetResponse' with the minimum fields required to make a request.
--
-- * 'capabilities' - If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
-- * 'changeSetId' - The ARN of the change set.
-- * 'changeSetName' - The name of the change set.
-- * 'changes' - A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
-- * 'creationTime' - The start time when the change set was created, in UTC.
-- * 'description' - Information about the change set.
-- * 'executionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
-- * 'includeNestedStacks' - Verifies if @IncludeNestedStacks@ is set to @True@ .
-- * 'nextToken' - If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
-- * 'notificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
-- * 'parameters' - A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
-- * 'parentChangeSetId' - Specifies the change set ID of the parent change set in the current nested change set hierarchy.
-- * 'responseStatus' - The response status code.
-- * 'rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
-- * 'rootChangeSetId' - Specifies the change set ID of the root change set in the current nested change set hierarchy.
-- * 'stackId' - The ARN of the stack that is associated with the change set.
-- * 'stackName' - The name of the stack that is associated with the change set.
-- * 'status' - The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
-- * 'statusReason' - A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
-- * 'tags' - If you execute the change set, the tags that will be associated with the stack.
mkDescribeChangeSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'status'
  ChangeSetStatus ->
  DescribeChangeSetResponse
mkDescribeChangeSetResponse pResponseStatus_ pStatus_ =
  DescribeChangeSetResponse'
    { creationTime = Lude.Nothing,
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
      responseStatus = pResponseStatus_,
      status = pStatus_
    }

-- | The start time when the change set was created, in UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsCreationTime :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.DateTime)
dcscrsCreationTime = Lens.lens (creationTime :: DescribeChangeSetResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTime = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Specifies the change set ID of the parent change set in the current nested change set hierarchy.
--
-- /Note:/ Consider using 'parentChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsParentChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsParentChangeSetId = Lens.lens (parentChangeSetId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentChangeSetId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsParentChangeSetId "Use generic-lens or generic-optics with 'parentChangeSetId' instead." #-}

-- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
--
-- /Note:/ Consider using 'changes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsChanges :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Change])
dcscrsChanges = Lens.lens (changes :: DescribeChangeSetResponse -> Lude.Maybe [Change]) (\s a -> s {changes = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsChanges "Use generic-lens or generic-optics with 'changes' instead." #-}

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsNotificationARNs :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Lude.Text])
dcscrsNotificationARNs = Lens.lens (notificationARNs :: DescribeChangeSetResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | The name of the change set.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsChangeSetName :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsChangeSetName = Lens.lens (changeSetName :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeSetName = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- /Note:/ Consider using 'executionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsExecutionStatus :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe ExecutionStatus)
dcscrsExecutionStatus = Lens.lens (executionStatus :: DescribeChangeSetResponse -> Lude.Maybe ExecutionStatus) (\s a -> s {executionStatus = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsExecutionStatus "Use generic-lens or generic-optics with 'executionStatus' instead." #-}

-- | The ARN of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsChangeSetId = Lens.lens (changeSetId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | Verifies if @IncludeNestedStacks@ is set to @True@ .
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsIncludeNestedStacks :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Bool)
dcscrsIncludeNestedStacks = Lens.lens (includeNestedStacks :: DescribeChangeSetResponse -> Lude.Maybe Lude.Bool) (\s a -> s {includeNestedStacks = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsNextToken :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsNextToken = Lens.lens (nextToken :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the change set ID of the root change set in the current nested change set hierarchy.
--
-- /Note:/ Consider using 'rootChangeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsRootChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsRootChangeSetId = Lens.lens (rootChangeSetId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {rootChangeSetId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsRootChangeSetId "Use generic-lens or generic-optics with 'rootChangeSetId' instead." #-}

-- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsParameters :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Parameter])
dcscrsParameters = Lens.lens (parameters :: DescribeChangeSetResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsStatusReason :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsStatusReason = Lens.lens (statusReason :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The ARN of the stack that is associated with the change set.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsStackId :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsStackId = Lens.lens (stackId :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Information about the change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsDescription :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsDescription = Lens.lens (description :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsCapabilities :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Capability])
dcscrsCapabilities = Lens.lens (capabilities :: DescribeChangeSetResponse -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsRollbackConfiguration :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe RollbackConfiguration)
dcscrsRollbackConfiguration = Lens.lens (rollbackConfiguration :: DescribeChangeSetResponse -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | If you execute the change set, the tags that will be associated with the stack.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsTags :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe [Tag])
dcscrsTags = Lens.lens (tags :: DescribeChangeSetResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the stack that is associated with the change set.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsStackName :: Lens.Lens' DescribeChangeSetResponse (Lude.Maybe Lude.Text)
dcscrsStackName = Lens.lens (stackName :: DescribeChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsResponseStatus :: Lens.Lens' DescribeChangeSetResponse Lude.Int
dcscrsResponseStatus = Lens.lens (responseStatus :: DescribeChangeSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsStatus :: Lens.Lens' DescribeChangeSetResponse ChangeSetStatus
dcscrsStatus = Lens.lens (status :: DescribeChangeSetResponse -> ChangeSetStatus) (\s a -> s {status = a} :: DescribeChangeSetResponse)
{-# DEPRECATED dcscrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

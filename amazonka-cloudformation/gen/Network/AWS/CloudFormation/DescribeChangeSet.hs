{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeChangeSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the inputs for the change set and a list of changes that AWS
-- CloudFormation will make if you execute the change set. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html Updating Stacks Using Change Sets>
-- in the AWS CloudFormation User Guide.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeChangeSet
  ( -- * Creating a Request
    DescribeChangeSet (..),
    newDescribeChangeSet,

    -- * Request Lenses
    describeChangeSet_nextToken,
    describeChangeSet_stackName,
    describeChangeSet_changeSetName,

    -- * Destructuring the Response
    DescribeChangeSetResponse (..),
    newDescribeChangeSetResponse,

    -- * Response Lenses
    describeChangeSetResponse_rootChangeSetId,
    describeChangeSetResponse_nextToken,
    describeChangeSetResponse_creationTime,
    describeChangeSetResponse_includeNestedStacks,
    describeChangeSetResponse_stackName,
    describeChangeSetResponse_capabilities,
    describeChangeSetResponse_executionStatus,
    describeChangeSetResponse_stackId,
    describeChangeSetResponse_notificationARNs,
    describeChangeSetResponse_changes,
    describeChangeSetResponse_parentChangeSetId,
    describeChangeSetResponse_tags,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_rollbackConfiguration,
    describeChangeSetResponse_description,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_statusReason,
    describeChangeSetResponse_parameters,
    describeChangeSetResponse_httpStatus,
    describeChangeSetResponse_status,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeChangeSet action.
--
-- /See:/ 'newDescribeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'
  { -- | A string (provided by the DescribeChangeSet response output) that
    -- identifies the next page of information that you want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If you specified the name of a change set, specify the stack name or ID
    -- (ARN) of the change set you want to describe.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the change set that you want
    -- to describe.
    changeSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeChangeSet_nextToken' - A string (provided by the DescribeChangeSet response output) that
-- identifies the next page of information that you want to retrieve.
--
-- 'stackName', 'describeChangeSet_stackName' - If you specified the name of a change set, specify the stack name or ID
-- (ARN) of the change set you want to describe.
--
-- 'changeSetName', 'describeChangeSet_changeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want
-- to describe.
newDescribeChangeSet ::
  -- | 'changeSetName'
  Prelude.Text ->
  DescribeChangeSet
newDescribeChangeSet pChangeSetName_ =
  DescribeChangeSet'
    { nextToken = Prelude.Nothing,
      stackName = Prelude.Nothing,
      changeSetName = pChangeSetName_
    }

-- | A string (provided by the DescribeChangeSet response output) that
-- identifies the next page of information that you want to retrieve.
describeChangeSet_nextToken :: Lens.Lens' DescribeChangeSet (Prelude.Maybe Prelude.Text)
describeChangeSet_nextToken = Lens.lens (\DescribeChangeSet' {nextToken} -> nextToken) (\s@DescribeChangeSet' {} a -> s {nextToken = a} :: DescribeChangeSet)

-- | If you specified the name of a change set, specify the stack name or ID
-- (ARN) of the change set you want to describe.
describeChangeSet_stackName :: Lens.Lens' DescribeChangeSet (Prelude.Maybe Prelude.Text)
describeChangeSet_stackName = Lens.lens (\DescribeChangeSet' {stackName} -> stackName) (\s@DescribeChangeSet' {} a -> s {stackName = a} :: DescribeChangeSet)

-- | The name or Amazon Resource Name (ARN) of the change set that you want
-- to describe.
describeChangeSet_changeSetName :: Lens.Lens' DescribeChangeSet Prelude.Text
describeChangeSet_changeSetName = Lens.lens (\DescribeChangeSet' {changeSetName} -> changeSetName) (\s@DescribeChangeSet' {} a -> s {changeSetName = a} :: DescribeChangeSet)

instance Pager.AWSPager DescribeChangeSet where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeChangeSetResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeChangeSetResponse_changes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeChangeSet_nextToken
          Lens..~ rs
          Lens.^? describeChangeSetResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeChangeSet where
  type Rs DescribeChangeSet = DescribeChangeSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeChangeSetResult"
      ( \s h x ->
          DescribeChangeSetResponse'
            Prelude.<$> (x Prelude..@? "RootChangeSetId")
            Prelude.<*> (x Prelude..@? "NextToken")
            Prelude.<*> (x Prelude..@? "CreationTime")
            Prelude.<*> (x Prelude..@? "IncludeNestedStacks")
            Prelude.<*> (x Prelude..@? "StackName")
            Prelude.<*> ( x Prelude..@? "Capabilities"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "ExecutionStatus")
            Prelude.<*> (x Prelude..@? "StackId")
            Prelude.<*> ( x Prelude..@? "NotificationARNs"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> ( x Prelude..@? "Changes" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "ParentChangeSetId")
            Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "ChangeSetId")
            Prelude.<*> (x Prelude..@? "RollbackConfiguration")
            Prelude.<*> (x Prelude..@? "Description")
            Prelude.<*> (x Prelude..@? "ChangeSetName")
            Prelude.<*> (x Prelude..@? "StatusReason")
            Prelude.<*> ( x Prelude..@? "Parameters"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "Status")
      )

instance Prelude.Hashable DescribeChangeSet

instance Prelude.NFData DescribeChangeSet

instance Prelude.ToHeaders DescribeChangeSet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeChangeSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeChangeSet where
  toQuery DescribeChangeSet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeChangeSet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "StackName" Prelude.=: stackName,
        "ChangeSetName" Prelude.=: changeSetName
      ]

-- | The output for the DescribeChangeSet action.
--
-- /See:/ 'newDescribeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { -- | Specifies the change set ID of the root change set in the current nested
    -- change set hierarchy.
    rootChangeSetId :: Prelude.Maybe Prelude.Text,
    -- | If the output exceeds 1 MB, a string that identifies the next page of
    -- changes. If there is no additional page, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The start time when the change set was created, in UTC.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | Verifies if @IncludeNestedStacks@ is set to @True@.
    includeNestedStacks :: Prelude.Maybe Prelude.Bool,
    -- | The name of the stack that is associated with the change set.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | If you execute the change set, the list of capabilities that were
    -- explicitly acknowledged when the change set was created.
    capabilities :: Prelude.Maybe [Capability],
    -- | If the change set execution status is @AVAILABLE@, you can execute the
    -- change set. If you can’t execute the change set, the status indicates
    -- why. For example, a change set might be in an @UNAVAILABLE@ state
    -- because AWS CloudFormation is still creating it or in an @OBSOLETE@
    -- state because the stack was already updated.
    executionStatus :: Prelude.Maybe ExecutionStatus,
    -- | The ARN of the stack that is associated with the change set.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics
    -- that will be associated with the stack if you execute the change set.
    notificationARNs :: Prelude.Maybe [Prelude.Text],
    -- | A list of @Change@ structures that describes the resources AWS
    -- CloudFormation changes if you execute the change set.
    changes :: Prelude.Maybe [Change],
    -- | Specifies the change set ID of the parent change set in the current
    -- nested change set hierarchy.
    parentChangeSetId :: Prelude.Maybe Prelude.Text,
    -- | If you execute the change set, the tags that will be associated with the
    -- stack.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the change set.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The rollback triggers for AWS CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Prelude.Maybe RollbackConfiguration,
    -- | Information about the change set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the change set.
    changeSetName :: Prelude.Maybe Prelude.Text,
    -- | A description of the change set\'s status. For example, if your attempt
    -- to create a change set failed, AWS CloudFormation shows the error
    -- message.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | A list of @Parameter@ structures that describes the input parameters and
    -- their values used to create the change set. For more information, see
    -- the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
    -- data type.
    parameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current status of the change set, such as @CREATE_IN_PROGRESS@,
    -- @CREATE_COMPLETE@, or @FAILED@.
    status :: ChangeSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootChangeSetId', 'describeChangeSetResponse_rootChangeSetId' - Specifies the change set ID of the root change set in the current nested
-- change set hierarchy.
--
-- 'nextToken', 'describeChangeSetResponse_nextToken' - If the output exceeds 1 MB, a string that identifies the next page of
-- changes. If there is no additional page, this value is null.
--
-- 'creationTime', 'describeChangeSetResponse_creationTime' - The start time when the change set was created, in UTC.
--
-- 'includeNestedStacks', 'describeChangeSetResponse_includeNestedStacks' - Verifies if @IncludeNestedStacks@ is set to @True@.
--
-- 'stackName', 'describeChangeSetResponse_stackName' - The name of the stack that is associated with the change set.
--
-- 'capabilities', 'describeChangeSetResponse_capabilities' - If you execute the change set, the list of capabilities that were
-- explicitly acknowledged when the change set was created.
--
-- 'executionStatus', 'describeChangeSetResponse_executionStatus' - If the change set execution status is @AVAILABLE@, you can execute the
-- change set. If you can’t execute the change set, the status indicates
-- why. For example, a change set might be in an @UNAVAILABLE@ state
-- because AWS CloudFormation is still creating it or in an @OBSOLETE@
-- state because the stack was already updated.
--
-- 'stackId', 'describeChangeSetResponse_stackId' - The ARN of the stack that is associated with the change set.
--
-- 'notificationARNs', 'describeChangeSetResponse_notificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics
-- that will be associated with the stack if you execute the change set.
--
-- 'changes', 'describeChangeSetResponse_changes' - A list of @Change@ structures that describes the resources AWS
-- CloudFormation changes if you execute the change set.
--
-- 'parentChangeSetId', 'describeChangeSetResponse_parentChangeSetId' - Specifies the change set ID of the parent change set in the current
-- nested change set hierarchy.
--
-- 'tags', 'describeChangeSetResponse_tags' - If you execute the change set, the tags that will be associated with the
-- stack.
--
-- 'changeSetId', 'describeChangeSetResponse_changeSetId' - The ARN of the change set.
--
-- 'rollbackConfiguration', 'describeChangeSetResponse_rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'description', 'describeChangeSetResponse_description' - Information about the change set.
--
-- 'changeSetName', 'describeChangeSetResponse_changeSetName' - The name of the change set.
--
-- 'statusReason', 'describeChangeSetResponse_statusReason' - A description of the change set\'s status. For example, if your attempt
-- to create a change set failed, AWS CloudFormation shows the error
-- message.
--
-- 'parameters', 'describeChangeSetResponse_parameters' - A list of @Parameter@ structures that describes the input parameters and
-- their values used to create the change set. For more information, see
-- the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
--
-- 'httpStatus', 'describeChangeSetResponse_httpStatus' - The response's http status code.
--
-- 'status', 'describeChangeSetResponse_status' - The current status of the change set, such as @CREATE_IN_PROGRESS@,
-- @CREATE_COMPLETE@, or @FAILED@.
newDescribeChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  ChangeSetStatus ->
  DescribeChangeSetResponse
newDescribeChangeSetResponse pHttpStatus_ pStatus_ =
  DescribeChangeSetResponse'
    { rootChangeSetId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      includeNestedStacks = Prelude.Nothing,
      stackName = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      executionStatus = Prelude.Nothing,
      stackId = Prelude.Nothing,
      notificationARNs = Prelude.Nothing,
      changes = Prelude.Nothing,
      parentChangeSetId = Prelude.Nothing,
      tags = Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      rollbackConfiguration = Prelude.Nothing,
      description = Prelude.Nothing,
      changeSetName = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      status = pStatus_
    }

-- | Specifies the change set ID of the root change set in the current nested
-- change set hierarchy.
describeChangeSetResponse_rootChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_rootChangeSetId = Lens.lens (\DescribeChangeSetResponse' {rootChangeSetId} -> rootChangeSetId) (\s@DescribeChangeSetResponse' {} a -> s {rootChangeSetId = a} :: DescribeChangeSetResponse)

-- | If the output exceeds 1 MB, a string that identifies the next page of
-- changes. If there is no additional page, this value is null.
describeChangeSetResponse_nextToken :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_nextToken = Lens.lens (\DescribeChangeSetResponse' {nextToken} -> nextToken) (\s@DescribeChangeSetResponse' {} a -> s {nextToken = a} :: DescribeChangeSetResponse)

-- | The start time when the change set was created, in UTC.
describeChangeSetResponse_creationTime :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.UTCTime)
describeChangeSetResponse_creationTime = Lens.lens (\DescribeChangeSetResponse' {creationTime} -> creationTime) (\s@DescribeChangeSetResponse' {} a -> s {creationTime = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Prelude._Time

-- | Verifies if @IncludeNestedStacks@ is set to @True@.
describeChangeSetResponse_includeNestedStacks :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Bool)
describeChangeSetResponse_includeNestedStacks = Lens.lens (\DescribeChangeSetResponse' {includeNestedStacks} -> includeNestedStacks) (\s@DescribeChangeSetResponse' {} a -> s {includeNestedStacks = a} :: DescribeChangeSetResponse)

-- | The name of the stack that is associated with the change set.
describeChangeSetResponse_stackName :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_stackName = Lens.lens (\DescribeChangeSetResponse' {stackName} -> stackName) (\s@DescribeChangeSetResponse' {} a -> s {stackName = a} :: DescribeChangeSetResponse)

-- | If you execute the change set, the list of capabilities that were
-- explicitly acknowledged when the change set was created.
describeChangeSetResponse_capabilities :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Capability])
describeChangeSetResponse_capabilities = Lens.lens (\DescribeChangeSetResponse' {capabilities} -> capabilities) (\s@DescribeChangeSetResponse' {} a -> s {capabilities = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If the change set execution status is @AVAILABLE@, you can execute the
-- change set. If you can’t execute the change set, the status indicates
-- why. For example, a change set might be in an @UNAVAILABLE@ state
-- because AWS CloudFormation is still creating it or in an @OBSOLETE@
-- state because the stack was already updated.
describeChangeSetResponse_executionStatus :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe ExecutionStatus)
describeChangeSetResponse_executionStatus = Lens.lens (\DescribeChangeSetResponse' {executionStatus} -> executionStatus) (\s@DescribeChangeSetResponse' {} a -> s {executionStatus = a} :: DescribeChangeSetResponse)

-- | The ARN of the stack that is associated with the change set.
describeChangeSetResponse_stackId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_stackId = Lens.lens (\DescribeChangeSetResponse' {stackId} -> stackId) (\s@DescribeChangeSetResponse' {} a -> s {stackId = a} :: DescribeChangeSetResponse)

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics
-- that will be associated with the stack if you execute the change set.
describeChangeSetResponse_notificationARNs :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Prelude.Text])
describeChangeSetResponse_notificationARNs = Lens.lens (\DescribeChangeSetResponse' {notificationARNs} -> notificationARNs) (\s@DescribeChangeSetResponse' {} a -> s {notificationARNs = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of @Change@ structures that describes the resources AWS
-- CloudFormation changes if you execute the change set.
describeChangeSetResponse_changes :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Change])
describeChangeSetResponse_changes = Lens.lens (\DescribeChangeSetResponse' {changes} -> changes) (\s@DescribeChangeSetResponse' {} a -> s {changes = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the change set ID of the parent change set in the current
-- nested change set hierarchy.
describeChangeSetResponse_parentChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_parentChangeSetId = Lens.lens (\DescribeChangeSetResponse' {parentChangeSetId} -> parentChangeSetId) (\s@DescribeChangeSetResponse' {} a -> s {parentChangeSetId = a} :: DescribeChangeSetResponse)

-- | If you execute the change set, the tags that will be associated with the
-- stack.
describeChangeSetResponse_tags :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Tag])
describeChangeSetResponse_tags = Lens.lens (\DescribeChangeSetResponse' {tags} -> tags) (\s@DescribeChangeSetResponse' {} a -> s {tags = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of the change set.
describeChangeSetResponse_changeSetId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_changeSetId = Lens.lens (\DescribeChangeSetResponse' {changeSetId} -> changeSetId) (\s@DescribeChangeSetResponse' {} a -> s {changeSetId = a} :: DescribeChangeSetResponse)

-- | The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
describeChangeSetResponse_rollbackConfiguration :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe RollbackConfiguration)
describeChangeSetResponse_rollbackConfiguration = Lens.lens (\DescribeChangeSetResponse' {rollbackConfiguration} -> rollbackConfiguration) (\s@DescribeChangeSetResponse' {} a -> s {rollbackConfiguration = a} :: DescribeChangeSetResponse)

-- | Information about the change set.
describeChangeSetResponse_description :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_description = Lens.lens (\DescribeChangeSetResponse' {description} -> description) (\s@DescribeChangeSetResponse' {} a -> s {description = a} :: DescribeChangeSetResponse)

-- | The name of the change set.
describeChangeSetResponse_changeSetName :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_changeSetName = Lens.lens (\DescribeChangeSetResponse' {changeSetName} -> changeSetName) (\s@DescribeChangeSetResponse' {} a -> s {changeSetName = a} :: DescribeChangeSetResponse)

-- | A description of the change set\'s status. For example, if your attempt
-- to create a change set failed, AWS CloudFormation shows the error
-- message.
describeChangeSetResponse_statusReason :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_statusReason = Lens.lens (\DescribeChangeSetResponse' {statusReason} -> statusReason) (\s@DescribeChangeSetResponse' {} a -> s {statusReason = a} :: DescribeChangeSetResponse)

-- | A list of @Parameter@ structures that describes the input parameters and
-- their values used to create the change set. For more information, see
-- the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
describeChangeSetResponse_parameters :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Parameter])
describeChangeSetResponse_parameters = Lens.lens (\DescribeChangeSetResponse' {parameters} -> parameters) (\s@DescribeChangeSetResponse' {} a -> s {parameters = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeChangeSetResponse_httpStatus :: Lens.Lens' DescribeChangeSetResponse Prelude.Int
describeChangeSetResponse_httpStatus = Lens.lens (\DescribeChangeSetResponse' {httpStatus} -> httpStatus) (\s@DescribeChangeSetResponse' {} a -> s {httpStatus = a} :: DescribeChangeSetResponse)

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@,
-- @CREATE_COMPLETE@, or @FAILED@.
describeChangeSetResponse_status :: Lens.Lens' DescribeChangeSetResponse ChangeSetStatus
describeChangeSetResponse_status = Lens.lens (\DescribeChangeSetResponse' {status} -> status) (\s@DescribeChangeSetResponse' {} a -> s {status = a} :: DescribeChangeSetResponse)

instance Prelude.NFData DescribeChangeSetResponse

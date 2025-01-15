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
-- Module      : Amazonka.CloudFormation.DescribeChangeSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the inputs for the change set and a list of changes that
-- CloudFormation will make if you execute the change set. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html Updating Stacks Using Change Sets>
-- in the CloudFormation User Guide.
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.DescribeChangeSet
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
    describeChangeSetResponse_capabilities,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_changes,
    describeChangeSetResponse_creationTime,
    describeChangeSetResponse_description,
    describeChangeSetResponse_executionStatus,
    describeChangeSetResponse_includeNestedStacks,
    describeChangeSetResponse_nextToken,
    describeChangeSetResponse_notificationARNs,
    describeChangeSetResponse_parameters,
    describeChangeSetResponse_parentChangeSetId,
    describeChangeSetResponse_rollbackConfiguration,
    describeChangeSetResponse_rootChangeSetId,
    describeChangeSetResponse_stackId,
    describeChangeSetResponse_stackName,
    describeChangeSetResponse_statusReason,
    describeChangeSetResponse_tags,
    describeChangeSetResponse_httpStatus,
    describeChangeSetResponse_status,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSPager DescribeChangeSet where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeChangeSetResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeChangeSetResponse_changes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeChangeSet_nextToken
              Lens..~ rs
              Lens.^? describeChangeSetResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeChangeSet where
  type
    AWSResponse DescribeChangeSet =
      DescribeChangeSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeChangeSetResult"
      ( \s h x ->
          DescribeChangeSetResponse'
            Prelude.<$> ( x Data..@? "Capabilities" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "ChangeSetId")
            Prelude.<*> (x Data..@? "ChangeSetName")
            Prelude.<*> ( x Data..@? "Changes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "CreationTime")
            Prelude.<*> (x Data..@? "Description")
            Prelude.<*> (x Data..@? "ExecutionStatus")
            Prelude.<*> (x Data..@? "IncludeNestedStacks")
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> ( x
                            Data..@? "NotificationARNs"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "Parameters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "ParentChangeSetId")
            Prelude.<*> (x Data..@? "RollbackConfiguration")
            Prelude.<*> (x Data..@? "RootChangeSetId")
            Prelude.<*> (x Data..@? "StackId")
            Prelude.<*> (x Data..@? "StackName")
            Prelude.<*> (x Data..@? "StatusReason")
            Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Status")
      )

instance Prelude.Hashable DescribeChangeSet where
  hashWithSalt _salt DescribeChangeSet' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` changeSetName

instance Prelude.NFData DescribeChangeSet where
  rnf DescribeChangeSet' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf stackName `Prelude.seq`
        Prelude.rnf changeSetName

instance Data.ToHeaders DescribeChangeSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeChangeSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeChangeSet where
  toQuery DescribeChangeSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeChangeSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "StackName" Data.=: stackName,
        "ChangeSetName" Data.=: changeSetName
      ]

-- | The output for the DescribeChangeSet action.
--
-- /See:/ 'newDescribeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { -- | If you execute the change set, the list of capabilities that were
    -- explicitly acknowledged when the change set was created.
    capabilities :: Prelude.Maybe [Capability],
    -- | The Amazon Resource Name (ARN) of the change set.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the change set.
    changeSetName :: Prelude.Maybe Prelude.Text,
    -- | A list of @Change@ structures that describes the resources
    -- CloudFormation changes if you execute the change set.
    changes :: Prelude.Maybe [Change],
    -- | The start time when the change set was created, in UTC.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | Information about the change set.
    description :: Prelude.Maybe Prelude.Text,
    -- | If the change set execution status is @AVAILABLE@, you can execute the
    -- change set. If you can\'t execute the change set, the status indicates
    -- why. For example, a change set might be in an @UNAVAILABLE@ state
    -- because CloudFormation is still creating it or in an @OBSOLETE@ state
    -- because the stack was already updated.
    executionStatus :: Prelude.Maybe ExecutionStatus,
    -- | Verifies if @IncludeNestedStacks@ is set to @True@.
    includeNestedStacks :: Prelude.Maybe Prelude.Bool,
    -- | If the output exceeds 1 MB, a string that identifies the next page of
    -- changes. If there is no additional page, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics
    -- that will be associated with the stack if you execute the change set.
    notificationARNs :: Prelude.Maybe [Prelude.Text],
    -- | A list of @Parameter@ structures that describes the input parameters and
    -- their values used to create the change set. For more information, see
    -- the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
    -- data type.
    parameters :: Prelude.Maybe [Parameter],
    -- | Specifies the change set ID of the parent change set in the current
    -- nested change set hierarchy.
    parentChangeSetId :: Prelude.Maybe Prelude.Text,
    -- | The rollback triggers for CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Prelude.Maybe RollbackConfiguration,
    -- | Specifies the change set ID of the root change set in the current nested
    -- change set hierarchy.
    rootChangeSetId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stack that\'s associated with the
    -- change set.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The name of the stack that\'s associated with the change set.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | A description of the change set\'s status. For example, if your attempt
    -- to create a change set failed, CloudFormation shows the error message.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | If you execute the change set, the tags that will be associated with the
    -- stack.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current status of the change set, such as @CREATE_IN_PROGRESS@,
    -- @CREATE_COMPLETE@, or @FAILED@.
    status :: ChangeSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'describeChangeSetResponse_capabilities' - If you execute the change set, the list of capabilities that were
-- explicitly acknowledged when the change set was created.
--
-- 'changeSetId', 'describeChangeSetResponse_changeSetId' - The Amazon Resource Name (ARN) of the change set.
--
-- 'changeSetName', 'describeChangeSetResponse_changeSetName' - The name of the change set.
--
-- 'changes', 'describeChangeSetResponse_changes' - A list of @Change@ structures that describes the resources
-- CloudFormation changes if you execute the change set.
--
-- 'creationTime', 'describeChangeSetResponse_creationTime' - The start time when the change set was created, in UTC.
--
-- 'description', 'describeChangeSetResponse_description' - Information about the change set.
--
-- 'executionStatus', 'describeChangeSetResponse_executionStatus' - If the change set execution status is @AVAILABLE@, you can execute the
-- change set. If you can\'t execute the change set, the status indicates
-- why. For example, a change set might be in an @UNAVAILABLE@ state
-- because CloudFormation is still creating it or in an @OBSOLETE@ state
-- because the stack was already updated.
--
-- 'includeNestedStacks', 'describeChangeSetResponse_includeNestedStacks' - Verifies if @IncludeNestedStacks@ is set to @True@.
--
-- 'nextToken', 'describeChangeSetResponse_nextToken' - If the output exceeds 1 MB, a string that identifies the next page of
-- changes. If there is no additional page, this value is null.
--
-- 'notificationARNs', 'describeChangeSetResponse_notificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics
-- that will be associated with the stack if you execute the change set.
--
-- 'parameters', 'describeChangeSetResponse_parameters' - A list of @Parameter@ structures that describes the input parameters and
-- their values used to create the change set. For more information, see
-- the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
--
-- 'parentChangeSetId', 'describeChangeSetResponse_parentChangeSetId' - Specifies the change set ID of the parent change set in the current
-- nested change set hierarchy.
--
-- 'rollbackConfiguration', 'describeChangeSetResponse_rollbackConfiguration' - The rollback triggers for CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'rootChangeSetId', 'describeChangeSetResponse_rootChangeSetId' - Specifies the change set ID of the root change set in the current nested
-- change set hierarchy.
--
-- 'stackId', 'describeChangeSetResponse_stackId' - The Amazon Resource Name (ARN) of the stack that\'s associated with the
-- change set.
--
-- 'stackName', 'describeChangeSetResponse_stackName' - The name of the stack that\'s associated with the change set.
--
-- 'statusReason', 'describeChangeSetResponse_statusReason' - A description of the change set\'s status. For example, if your attempt
-- to create a change set failed, CloudFormation shows the error message.
--
-- 'tags', 'describeChangeSetResponse_tags' - If you execute the change set, the tags that will be associated with the
-- stack.
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
    { capabilities =
        Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      changeSetName = Prelude.Nothing,
      changes = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      executionStatus = Prelude.Nothing,
      includeNestedStacks = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      notificationARNs = Prelude.Nothing,
      parameters = Prelude.Nothing,
      parentChangeSetId = Prelude.Nothing,
      rollbackConfiguration = Prelude.Nothing,
      rootChangeSetId = Prelude.Nothing,
      stackId = Prelude.Nothing,
      stackName = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      status = pStatus_
    }

-- | If you execute the change set, the list of capabilities that were
-- explicitly acknowledged when the change set was created.
describeChangeSetResponse_capabilities :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Capability])
describeChangeSetResponse_capabilities = Lens.lens (\DescribeChangeSetResponse' {capabilities} -> capabilities) (\s@DescribeChangeSetResponse' {} a -> s {capabilities = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the change set.
describeChangeSetResponse_changeSetId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_changeSetId = Lens.lens (\DescribeChangeSetResponse' {changeSetId} -> changeSetId) (\s@DescribeChangeSetResponse' {} a -> s {changeSetId = a} :: DescribeChangeSetResponse)

-- | The name of the change set.
describeChangeSetResponse_changeSetName :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_changeSetName = Lens.lens (\DescribeChangeSetResponse' {changeSetName} -> changeSetName) (\s@DescribeChangeSetResponse' {} a -> s {changeSetName = a} :: DescribeChangeSetResponse)

-- | A list of @Change@ structures that describes the resources
-- CloudFormation changes if you execute the change set.
describeChangeSetResponse_changes :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Change])
describeChangeSetResponse_changes = Lens.lens (\DescribeChangeSetResponse' {changes} -> changes) (\s@DescribeChangeSetResponse' {} a -> s {changes = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The start time when the change set was created, in UTC.
describeChangeSetResponse_creationTime :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.UTCTime)
describeChangeSetResponse_creationTime = Lens.lens (\DescribeChangeSetResponse' {creationTime} -> creationTime) (\s@DescribeChangeSetResponse' {} a -> s {creationTime = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Data._Time

-- | Information about the change set.
describeChangeSetResponse_description :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_description = Lens.lens (\DescribeChangeSetResponse' {description} -> description) (\s@DescribeChangeSetResponse' {} a -> s {description = a} :: DescribeChangeSetResponse)

-- | If the change set execution status is @AVAILABLE@, you can execute the
-- change set. If you can\'t execute the change set, the status indicates
-- why. For example, a change set might be in an @UNAVAILABLE@ state
-- because CloudFormation is still creating it or in an @OBSOLETE@ state
-- because the stack was already updated.
describeChangeSetResponse_executionStatus :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe ExecutionStatus)
describeChangeSetResponse_executionStatus = Lens.lens (\DescribeChangeSetResponse' {executionStatus} -> executionStatus) (\s@DescribeChangeSetResponse' {} a -> s {executionStatus = a} :: DescribeChangeSetResponse)

-- | Verifies if @IncludeNestedStacks@ is set to @True@.
describeChangeSetResponse_includeNestedStacks :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Bool)
describeChangeSetResponse_includeNestedStacks = Lens.lens (\DescribeChangeSetResponse' {includeNestedStacks} -> includeNestedStacks) (\s@DescribeChangeSetResponse' {} a -> s {includeNestedStacks = a} :: DescribeChangeSetResponse)

-- | If the output exceeds 1 MB, a string that identifies the next page of
-- changes. If there is no additional page, this value is null.
describeChangeSetResponse_nextToken :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_nextToken = Lens.lens (\DescribeChangeSetResponse' {nextToken} -> nextToken) (\s@DescribeChangeSetResponse' {} a -> s {nextToken = a} :: DescribeChangeSetResponse)

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics
-- that will be associated with the stack if you execute the change set.
describeChangeSetResponse_notificationARNs :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Prelude.Text])
describeChangeSetResponse_notificationARNs = Lens.lens (\DescribeChangeSetResponse' {notificationARNs} -> notificationARNs) (\s@DescribeChangeSetResponse' {} a -> s {notificationARNs = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of @Parameter@ structures that describes the input parameters and
-- their values used to create the change set. For more information, see
-- the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
describeChangeSetResponse_parameters :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Parameter])
describeChangeSetResponse_parameters = Lens.lens (\DescribeChangeSetResponse' {parameters} -> parameters) (\s@DescribeChangeSetResponse' {} a -> s {parameters = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the change set ID of the parent change set in the current
-- nested change set hierarchy.
describeChangeSetResponse_parentChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_parentChangeSetId = Lens.lens (\DescribeChangeSetResponse' {parentChangeSetId} -> parentChangeSetId) (\s@DescribeChangeSetResponse' {} a -> s {parentChangeSetId = a} :: DescribeChangeSetResponse)

-- | The rollback triggers for CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
describeChangeSetResponse_rollbackConfiguration :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe RollbackConfiguration)
describeChangeSetResponse_rollbackConfiguration = Lens.lens (\DescribeChangeSetResponse' {rollbackConfiguration} -> rollbackConfiguration) (\s@DescribeChangeSetResponse' {} a -> s {rollbackConfiguration = a} :: DescribeChangeSetResponse)

-- | Specifies the change set ID of the root change set in the current nested
-- change set hierarchy.
describeChangeSetResponse_rootChangeSetId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_rootChangeSetId = Lens.lens (\DescribeChangeSetResponse' {rootChangeSetId} -> rootChangeSetId) (\s@DescribeChangeSetResponse' {} a -> s {rootChangeSetId = a} :: DescribeChangeSetResponse)

-- | The Amazon Resource Name (ARN) of the stack that\'s associated with the
-- change set.
describeChangeSetResponse_stackId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_stackId = Lens.lens (\DescribeChangeSetResponse' {stackId} -> stackId) (\s@DescribeChangeSetResponse' {} a -> s {stackId = a} :: DescribeChangeSetResponse)

-- | The name of the stack that\'s associated with the change set.
describeChangeSetResponse_stackName :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_stackName = Lens.lens (\DescribeChangeSetResponse' {stackName} -> stackName) (\s@DescribeChangeSetResponse' {} a -> s {stackName = a} :: DescribeChangeSetResponse)

-- | A description of the change set\'s status. For example, if your attempt
-- to create a change set failed, CloudFormation shows the error message.
describeChangeSetResponse_statusReason :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_statusReason = Lens.lens (\DescribeChangeSetResponse' {statusReason} -> statusReason) (\s@DescribeChangeSetResponse' {} a -> s {statusReason = a} :: DescribeChangeSetResponse)

-- | If you execute the change set, the tags that will be associated with the
-- stack.
describeChangeSetResponse_tags :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [Tag])
describeChangeSetResponse_tags = Lens.lens (\DescribeChangeSetResponse' {tags} -> tags) (\s@DescribeChangeSetResponse' {} a -> s {tags = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeChangeSetResponse_httpStatus :: Lens.Lens' DescribeChangeSetResponse Prelude.Int
describeChangeSetResponse_httpStatus = Lens.lens (\DescribeChangeSetResponse' {httpStatus} -> httpStatus) (\s@DescribeChangeSetResponse' {} a -> s {httpStatus = a} :: DescribeChangeSetResponse)

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@,
-- @CREATE_COMPLETE@, or @FAILED@.
describeChangeSetResponse_status :: Lens.Lens' DescribeChangeSetResponse ChangeSetStatus
describeChangeSetResponse_status = Lens.lens (\DescribeChangeSetResponse' {status} -> status) (\s@DescribeChangeSetResponse' {} a -> s {status = a} :: DescribeChangeSetResponse)

instance Prelude.NFData DescribeChangeSetResponse where
  rnf DescribeChangeSetResponse' {..} =
    Prelude.rnf capabilities `Prelude.seq`
      Prelude.rnf changeSetId `Prelude.seq`
        Prelude.rnf changeSetName `Prelude.seq`
          Prelude.rnf changes `Prelude.seq`
            Prelude.rnf creationTime `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf executionStatus `Prelude.seq`
                  Prelude.rnf includeNestedStacks `Prelude.seq`
                    Prelude.rnf nextToken `Prelude.seq`
                      Prelude.rnf notificationARNs `Prelude.seq`
                        Prelude.rnf parameters `Prelude.seq`
                          Prelude.rnf parentChangeSetId `Prelude.seq`
                            Prelude.rnf rollbackConfiguration `Prelude.seq`
                              Prelude.rnf rootChangeSetId `Prelude.seq`
                                Prelude.rnf stackId `Prelude.seq`
                                  Prelude.rnf stackName `Prelude.seq`
                                    Prelude.rnf statusReason `Prelude.seq`
                                      Prelude.rnf tags `Prelude.seq`
                                        Prelude.rnf httpStatus `Prelude.seq`
                                          Prelude.rnf status

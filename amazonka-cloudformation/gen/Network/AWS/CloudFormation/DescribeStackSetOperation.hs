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
-- Module      : Network.AWS.CloudFormation.DescribeStackSetOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set operation.
module Network.AWS.CloudFormation.DescribeStackSetOperation
  ( -- * Creating a Request
    DescribeStackSetOperation (..),
    newDescribeStackSetOperation,

    -- * Request Lenses
    describeStackSetOperation_callAs,
    describeStackSetOperation_stackSetName,
    describeStackSetOperation_operationId,

    -- * Destructuring the Response
    DescribeStackSetOperationResponse (..),
    newDescribeStackSetOperationResponse,

    -- * Response Lenses
    describeStackSetOperationResponse_stackSetOperation,
    describeStackSetOperationResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStackSetOperation' smart constructor.
data DescribeStackSetOperation = DescribeStackSetOperation'
  { -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your AWS account must be registered as a delegated administrator in
    --     the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /AWS CloudFormation User Guide/.
    callAs :: Core.Maybe CallAs,
    -- | The name or the unique stack ID of the stack set for the stack
    -- operation.
    stackSetName :: Core.Text,
    -- | The unique ID of the stack set operation.
    operationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStackSetOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'describeStackSetOperation_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
--
-- 'stackSetName', 'describeStackSetOperation_stackSetName' - The name or the unique stack ID of the stack set for the stack
-- operation.
--
-- 'operationId', 'describeStackSetOperation_operationId' - The unique ID of the stack set operation.
newDescribeStackSetOperation ::
  -- | 'stackSetName'
  Core.Text ->
  -- | 'operationId'
  Core.Text ->
  DescribeStackSetOperation
newDescribeStackSetOperation
  pStackSetName_
  pOperationId_ =
    DescribeStackSetOperation'
      { callAs = Core.Nothing,
        stackSetName = pStackSetName_,
        operationId = pOperationId_
      }

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
describeStackSetOperation_callAs :: Lens.Lens' DescribeStackSetOperation (Core.Maybe CallAs)
describeStackSetOperation_callAs = Lens.lens (\DescribeStackSetOperation' {callAs} -> callAs) (\s@DescribeStackSetOperation' {} a -> s {callAs = a} :: DescribeStackSetOperation)

-- | The name or the unique stack ID of the stack set for the stack
-- operation.
describeStackSetOperation_stackSetName :: Lens.Lens' DescribeStackSetOperation Core.Text
describeStackSetOperation_stackSetName = Lens.lens (\DescribeStackSetOperation' {stackSetName} -> stackSetName) (\s@DescribeStackSetOperation' {} a -> s {stackSetName = a} :: DescribeStackSetOperation)

-- | The unique ID of the stack set operation.
describeStackSetOperation_operationId :: Lens.Lens' DescribeStackSetOperation Core.Text
describeStackSetOperation_operationId = Lens.lens (\DescribeStackSetOperation' {operationId} -> operationId) (\s@DescribeStackSetOperation' {} a -> s {operationId = a} :: DescribeStackSetOperation)

instance Core.AWSRequest DescribeStackSetOperation where
  type
    AWSResponse DescribeStackSetOperation =
      DescribeStackSetOperationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStackSetOperationResult"
      ( \s h x ->
          DescribeStackSetOperationResponse'
            Core.<$> (x Core..@? "StackSetOperation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStackSetOperation

instance Core.NFData DescribeStackSetOperation

instance Core.ToHeaders DescribeStackSetOperation where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeStackSetOperation where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStackSetOperation where
  toQuery DescribeStackSetOperation' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeStackSetOperation" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "CallAs" Core.=: callAs,
        "StackSetName" Core.=: stackSetName,
        "OperationId" Core.=: operationId
      ]

-- | /See:/ 'newDescribeStackSetOperationResponse' smart constructor.
data DescribeStackSetOperationResponse = DescribeStackSetOperationResponse'
  { -- | The specified stack set operation.
    stackSetOperation :: Core.Maybe StackSetOperation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStackSetOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetOperation', 'describeStackSetOperationResponse_stackSetOperation' - The specified stack set operation.
--
-- 'httpStatus', 'describeStackSetOperationResponse_httpStatus' - The response's http status code.
newDescribeStackSetOperationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStackSetOperationResponse
newDescribeStackSetOperationResponse pHttpStatus_ =
  DescribeStackSetOperationResponse'
    { stackSetOperation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The specified stack set operation.
describeStackSetOperationResponse_stackSetOperation :: Lens.Lens' DescribeStackSetOperationResponse (Core.Maybe StackSetOperation)
describeStackSetOperationResponse_stackSetOperation = Lens.lens (\DescribeStackSetOperationResponse' {stackSetOperation} -> stackSetOperation) (\s@DescribeStackSetOperationResponse' {} a -> s {stackSetOperation = a} :: DescribeStackSetOperationResponse)

-- | The response's http status code.
describeStackSetOperationResponse_httpStatus :: Lens.Lens' DescribeStackSetOperationResponse Core.Int
describeStackSetOperationResponse_httpStatus = Lens.lens (\DescribeStackSetOperationResponse' {httpStatus} -> httpStatus) (\s@DescribeStackSetOperationResponse' {} a -> s {httpStatus = a} :: DescribeStackSetOperationResponse)

instance
  Core.NFData
    DescribeStackSetOperationResponse

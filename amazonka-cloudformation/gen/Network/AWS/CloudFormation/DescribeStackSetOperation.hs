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
import qualified Network.AWS.Prelude as Prelude
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
    callAs :: Prelude.Maybe CallAs,
    -- | The name or the unique stack ID of the stack set for the stack
    -- operation.
    stackSetName :: Prelude.Text,
    -- | The unique ID of the stack set operation.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'operationId'
  Prelude.Text ->
  DescribeStackSetOperation
newDescribeStackSetOperation
  pStackSetName_
  pOperationId_ =
    DescribeStackSetOperation'
      { callAs =
          Prelude.Nothing,
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
describeStackSetOperation_callAs :: Lens.Lens' DescribeStackSetOperation (Prelude.Maybe CallAs)
describeStackSetOperation_callAs = Lens.lens (\DescribeStackSetOperation' {callAs} -> callAs) (\s@DescribeStackSetOperation' {} a -> s {callAs = a} :: DescribeStackSetOperation)

-- | The name or the unique stack ID of the stack set for the stack
-- operation.
describeStackSetOperation_stackSetName :: Lens.Lens' DescribeStackSetOperation Prelude.Text
describeStackSetOperation_stackSetName = Lens.lens (\DescribeStackSetOperation' {stackSetName} -> stackSetName) (\s@DescribeStackSetOperation' {} a -> s {stackSetName = a} :: DescribeStackSetOperation)

-- | The unique ID of the stack set operation.
describeStackSetOperation_operationId :: Lens.Lens' DescribeStackSetOperation Prelude.Text
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
            Prelude.<$> (x Core..@? "StackSetOperation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackSetOperation

instance Prelude.NFData DescribeStackSetOperation

instance Core.ToHeaders DescribeStackSetOperation where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeStackSetOperation where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStackSetOperation where
  toQuery DescribeStackSetOperation' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeStackSetOperation" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Core.=: callAs,
        "StackSetName" Core.=: stackSetName,
        "OperationId" Core.=: operationId
      ]

-- | /See:/ 'newDescribeStackSetOperationResponse' smart constructor.
data DescribeStackSetOperationResponse = DescribeStackSetOperationResponse'
  { -- | The specified stack set operation.
    stackSetOperation :: Prelude.Maybe StackSetOperation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeStackSetOperationResponse
newDescribeStackSetOperationResponse pHttpStatus_ =
  DescribeStackSetOperationResponse'
    { stackSetOperation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The specified stack set operation.
describeStackSetOperationResponse_stackSetOperation :: Lens.Lens' DescribeStackSetOperationResponse (Prelude.Maybe StackSetOperation)
describeStackSetOperationResponse_stackSetOperation = Lens.lens (\DescribeStackSetOperationResponse' {stackSetOperation} -> stackSetOperation) (\s@DescribeStackSetOperationResponse' {} a -> s {stackSetOperation = a} :: DescribeStackSetOperationResponse)

-- | The response's http status code.
describeStackSetOperationResponse_httpStatus :: Lens.Lens' DescribeStackSetOperationResponse Prelude.Int
describeStackSetOperationResponse_httpStatus = Lens.lens (\DescribeStackSetOperationResponse' {httpStatus} -> httpStatus) (\s@DescribeStackSetOperationResponse' {} a -> s {httpStatus = a} :: DescribeStackSetOperationResponse)

instance
  Prelude.NFData
    DescribeStackSetOperationResponse

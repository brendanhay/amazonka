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
-- Module      : Amazonka.CloudFormation.DescribeStackSetOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set operation.
module Amazonka.CloudFormation.DescribeStackSetOperation
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

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    --     Your Amazon Web Services account must be registered as a delegated
    --     administrator in the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /CloudFormation User Guide/.
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
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
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
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeStackSetOperationResult"
      ( \s h x ->
          DescribeStackSetOperationResponse'
            Prelude.<$> (x Data..@? "StackSetOperation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackSetOperation where
  hashWithSalt _salt DescribeStackSetOperation' {..} =
    _salt
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` operationId

instance Prelude.NFData DescribeStackSetOperation where
  rnf DescribeStackSetOperation' {..} =
    Prelude.rnf callAs
      `Prelude.seq` Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf operationId

instance Data.ToHeaders DescribeStackSetOperation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeStackSetOperation where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStackSetOperation where
  toQuery DescribeStackSetOperation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeStackSetOperation" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Data.=: callAs,
        "StackSetName" Data.=: stackSetName,
        "OperationId" Data.=: operationId
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
  where
  rnf DescribeStackSetOperationResponse' {..} =
    Prelude.rnf stackSetOperation
      `Prelude.seq` Prelude.rnf httpStatus

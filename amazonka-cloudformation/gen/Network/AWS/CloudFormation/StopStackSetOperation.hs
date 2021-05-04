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
-- Module      : Network.AWS.CloudFormation.StopStackSetOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an in-progress operation on a stack set and its associated stack
-- instances.
module Network.AWS.CloudFormation.StopStackSetOperation
  ( -- * Creating a Request
    StopStackSetOperation (..),
    newStopStackSetOperation,

    -- * Request Lenses
    stopStackSetOperation_callAs,
    stopStackSetOperation_stackSetName,
    stopStackSetOperation_operationId,

    -- * Destructuring the Response
    StopStackSetOperationResponse (..),
    newStopStackSetOperationResponse,

    -- * Response Lenses
    stopStackSetOperationResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopStackSetOperation' smart constructor.
data StopStackSetOperation = StopStackSetOperation'
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
    -- | The name or unique ID of the stack set that you want to stop the
    -- operation for.
    stackSetName :: Prelude.Text,
    -- | The ID of the stack operation.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopStackSetOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'stopStackSetOperation_callAs' - [Service-managed permissions] Specifies whether you are acting as an
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
-- 'stackSetName', 'stopStackSetOperation_stackSetName' - The name or unique ID of the stack set that you want to stop the
-- operation for.
--
-- 'operationId', 'stopStackSetOperation_operationId' - The ID of the stack operation.
newStopStackSetOperation ::
  -- | 'stackSetName'
  Prelude.Text ->
  -- | 'operationId'
  Prelude.Text ->
  StopStackSetOperation
newStopStackSetOperation pStackSetName_ pOperationId_ =
  StopStackSetOperation'
    { callAs = Prelude.Nothing,
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
stopStackSetOperation_callAs :: Lens.Lens' StopStackSetOperation (Prelude.Maybe CallAs)
stopStackSetOperation_callAs = Lens.lens (\StopStackSetOperation' {callAs} -> callAs) (\s@StopStackSetOperation' {} a -> s {callAs = a} :: StopStackSetOperation)

-- | The name or unique ID of the stack set that you want to stop the
-- operation for.
stopStackSetOperation_stackSetName :: Lens.Lens' StopStackSetOperation Prelude.Text
stopStackSetOperation_stackSetName = Lens.lens (\StopStackSetOperation' {stackSetName} -> stackSetName) (\s@StopStackSetOperation' {} a -> s {stackSetName = a} :: StopStackSetOperation)

-- | The ID of the stack operation.
stopStackSetOperation_operationId :: Lens.Lens' StopStackSetOperation Prelude.Text
stopStackSetOperation_operationId = Lens.lens (\StopStackSetOperation' {operationId} -> operationId) (\s@StopStackSetOperation' {} a -> s {operationId = a} :: StopStackSetOperation)

instance Prelude.AWSRequest StopStackSetOperation where
  type
    Rs StopStackSetOperation =
      StopStackSetOperationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StopStackSetOperationResult"
      ( \s h x ->
          StopStackSetOperationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopStackSetOperation

instance Prelude.NFData StopStackSetOperation

instance Prelude.ToHeaders StopStackSetOperation where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath StopStackSetOperation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopStackSetOperation where
  toQuery StopStackSetOperation' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("StopStackSetOperation" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Prelude.=: callAs,
        "StackSetName" Prelude.=: stackSetName,
        "OperationId" Prelude.=: operationId
      ]

-- | /See:/ 'newStopStackSetOperationResponse' smart constructor.
data StopStackSetOperationResponse = StopStackSetOperationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopStackSetOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopStackSetOperationResponse_httpStatus' - The response's http status code.
newStopStackSetOperationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopStackSetOperationResponse
newStopStackSetOperationResponse pHttpStatus_ =
  StopStackSetOperationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopStackSetOperationResponse_httpStatus :: Lens.Lens' StopStackSetOperationResponse Prelude.Int
stopStackSetOperationResponse_httpStatus = Lens.lens (\StopStackSetOperationResponse' {httpStatus} -> httpStatus) (\s@StopStackSetOperationResponse' {} a -> s {httpStatus = a} :: StopStackSetOperationResponse)

instance Prelude.NFData StopStackSetOperationResponse

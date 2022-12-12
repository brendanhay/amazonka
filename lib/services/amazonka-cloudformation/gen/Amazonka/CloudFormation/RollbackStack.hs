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
-- Module      : Amazonka.CloudFormation.RollbackStack
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When specifying @RollbackStack@, you preserve the state of previously
-- provisioned resources when an operation fails. You can check the status
-- of the stack through the DescribeStacks operation.
--
-- Rolls back the specified stack to the last known stable state from
-- @CREATE_FAILED@ or @UPDATE_FAILED@ stack statuses.
--
-- This operation will delete a stack if it doesn\'t contain a last known
-- stable state. A last known stable state includes any status in a
-- @*_COMPLETE@. This includes the following stack statuses.
--
-- -   @CREATE_COMPLETE@
--
-- -   @UPDATE_COMPLETE@
--
-- -   @UPDATE_ROLLBACK_COMPLETE@
--
-- -   @IMPORT_COMPLETE@
--
-- -   @IMPORT_ROLLBACK_COMPLETE@
module Amazonka.CloudFormation.RollbackStack
  ( -- * Creating a Request
    RollbackStack (..),
    newRollbackStack,

    -- * Request Lenses
    rollbackStack_clientRequestToken,
    rollbackStack_roleARN,
    rollbackStack_stackName,

    -- * Destructuring the Response
    RollbackStackResponse (..),
    newRollbackStackResponse,

    -- * Response Lenses
    rollbackStackResponse_stackId,
    rollbackStackResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRollbackStack' smart constructor.
data RollbackStack = RollbackStack'
  { -- | A unique identifier for this @RollbackStack@ request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Identity and Access Management role
    -- that CloudFormation assumes to rollback the stack.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The name that\'s associated with the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'rollbackStack_clientRequestToken' - A unique identifier for this @RollbackStack@ request.
--
-- 'roleARN', 'rollbackStack_roleARN' - The Amazon Resource Name (ARN) of an Identity and Access Management role
-- that CloudFormation assumes to rollback the stack.
--
-- 'stackName', 'rollbackStack_stackName' - The name that\'s associated with the stack.
newRollbackStack ::
  -- | 'stackName'
  Prelude.Text ->
  RollbackStack
newRollbackStack pStackName_ =
  RollbackStack'
    { clientRequestToken =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      stackName = pStackName_
    }

-- | A unique identifier for this @RollbackStack@ request.
rollbackStack_clientRequestToken :: Lens.Lens' RollbackStack (Prelude.Maybe Prelude.Text)
rollbackStack_clientRequestToken = Lens.lens (\RollbackStack' {clientRequestToken} -> clientRequestToken) (\s@RollbackStack' {} a -> s {clientRequestToken = a} :: RollbackStack)

-- | The Amazon Resource Name (ARN) of an Identity and Access Management role
-- that CloudFormation assumes to rollback the stack.
rollbackStack_roleARN :: Lens.Lens' RollbackStack (Prelude.Maybe Prelude.Text)
rollbackStack_roleARN = Lens.lens (\RollbackStack' {roleARN} -> roleARN) (\s@RollbackStack' {} a -> s {roleARN = a} :: RollbackStack)

-- | The name that\'s associated with the stack.
rollbackStack_stackName :: Lens.Lens' RollbackStack Prelude.Text
rollbackStack_stackName = Lens.lens (\RollbackStack' {stackName} -> stackName) (\s@RollbackStack' {} a -> s {stackName = a} :: RollbackStack)

instance Core.AWSRequest RollbackStack where
  type
    AWSResponse RollbackStack =
      RollbackStackResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RollbackStackResult"
      ( \s h x ->
          RollbackStackResponse'
            Prelude.<$> (x Data..@? "StackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RollbackStack where
  hashWithSalt _salt RollbackStack' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData RollbackStack where
  rnf RollbackStack' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders RollbackStack where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RollbackStack where
  toPath = Prelude.const "/"

instance Data.ToQuery RollbackStack where
  toQuery RollbackStack' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RollbackStack" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "ClientRequestToken" Data.=: clientRequestToken,
        "RoleARN" Data.=: roleARN,
        "StackName" Data.=: stackName
      ]

-- | /See:/ 'newRollbackStackResponse' smart constructor.
data RollbackStackResponse = RollbackStackResponse'
  { -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'rollbackStackResponse_stackId' - Unique identifier of the stack.
--
-- 'httpStatus', 'rollbackStackResponse_httpStatus' - The response's http status code.
newRollbackStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RollbackStackResponse
newRollbackStackResponse pHttpStatus_ =
  RollbackStackResponse'
    { stackId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier of the stack.
rollbackStackResponse_stackId :: Lens.Lens' RollbackStackResponse (Prelude.Maybe Prelude.Text)
rollbackStackResponse_stackId = Lens.lens (\RollbackStackResponse' {stackId} -> stackId) (\s@RollbackStackResponse' {} a -> s {stackId = a} :: RollbackStackResponse)

-- | The response's http status code.
rollbackStackResponse_httpStatus :: Lens.Lens' RollbackStackResponse Prelude.Int
rollbackStackResponse_httpStatus = Lens.lens (\RollbackStackResponse' {httpStatus} -> httpStatus) (\s@RollbackStackResponse' {} a -> s {httpStatus = a} :: RollbackStackResponse)

instance Prelude.NFData RollbackStackResponse where
  rnf RollbackStackResponse' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.CloudFormation.ExecuteChangeSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack using the input information that was provided when the
-- specified change set was created. After the call successfully completes,
-- CloudFormation starts updating the stack. Use the DescribeStacks action
-- to view the status of the update.
--
-- When you execute a change set, CloudFormation deletes all other change
-- sets associated with the stack because they aren\'t valid for the
-- updated stack.
--
-- If a stack policy is associated with the stack, CloudFormation enforces
-- the policy during the update. You can\'t specify a temporary stack
-- policy that overrides the current policy.
--
-- To create a change set for the entire stack hierarchy,
-- @IncludeNestedStacks@ must have been set to @True@.
module Amazonka.CloudFormation.ExecuteChangeSet
  ( -- * Creating a Request
    ExecuteChangeSet (..),
    newExecuteChangeSet,

    -- * Request Lenses
    executeChangeSet_clientRequestToken,
    executeChangeSet_disableRollback,
    executeChangeSet_stackName,
    executeChangeSet_changeSetName,

    -- * Destructuring the Response
    ExecuteChangeSetResponse (..),
    newExecuteChangeSetResponse,

    -- * Response Lenses
    executeChangeSetResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ExecuteChangeSet action.
--
-- /See:/ 'newExecuteChangeSet' smart constructor.
data ExecuteChangeSet = ExecuteChangeSet'
  { -- | A unique identifier for this @ExecuteChangeSet@ request. Specify this
    -- token if you plan to retry requests so that CloudFormation knows that
    -- you\'re not attempting to execute a change set to update a stack with
    -- the same name. You might retry @ExecuteChangeSet@ requests to ensure
    -- that CloudFormation successfully received them.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Preserves the state of previously provisioned resources when an
    -- operation fails. This parameter can\'t be specified when the
    -- @OnStackFailure@ parameter to the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
    -- API operation was specified.
    --
    -- -   @True@ - if the stack creation fails, do nothing. This is equivalent
    --     to specifying @DO_NOTHING@ for the @OnStackFailure@ parameter to the
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
    --     API operation.
    --
    -- -   @False@ - if the stack creation fails, roll back the stack. This is
    --     equivalent to specifying @ROLLBACK@ for the @OnStackFailure@
    --     parameter to the
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
    --     API operation.
    --
    -- Default: @True@
    disableRollback :: Prelude.Maybe Prelude.Bool,
    -- | If you specified the name of a change set, specify the stack name or
    -- Amazon Resource Name (ARN) that\'s associated with the change set you
    -- want to execute.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the change set that you want
    -- use to update the specified stack.
    changeSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'executeChangeSet_clientRequestToken' - A unique identifier for this @ExecuteChangeSet@ request. Specify this
-- token if you plan to retry requests so that CloudFormation knows that
-- you\'re not attempting to execute a change set to update a stack with
-- the same name. You might retry @ExecuteChangeSet@ requests to ensure
-- that CloudFormation successfully received them.
--
-- 'disableRollback', 'executeChangeSet_disableRollback' - Preserves the state of previously provisioned resources when an
-- operation fails. This parameter can\'t be specified when the
-- @OnStackFailure@ parameter to the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
-- API operation was specified.
--
-- -   @True@ - if the stack creation fails, do nothing. This is equivalent
--     to specifying @DO_NOTHING@ for the @OnStackFailure@ parameter to the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
--     API operation.
--
-- -   @False@ - if the stack creation fails, roll back the stack. This is
--     equivalent to specifying @ROLLBACK@ for the @OnStackFailure@
--     parameter to the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
--     API operation.
--
-- Default: @True@
--
-- 'stackName', 'executeChangeSet_stackName' - If you specified the name of a change set, specify the stack name or
-- Amazon Resource Name (ARN) that\'s associated with the change set you
-- want to execute.
--
-- 'changeSetName', 'executeChangeSet_changeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want
-- use to update the specified stack.
newExecuteChangeSet ::
  -- | 'changeSetName'
  Prelude.Text ->
  ExecuteChangeSet
newExecuteChangeSet pChangeSetName_ =
  ExecuteChangeSet'
    { clientRequestToken =
        Prelude.Nothing,
      disableRollback = Prelude.Nothing,
      stackName = Prelude.Nothing,
      changeSetName = pChangeSetName_
    }

-- | A unique identifier for this @ExecuteChangeSet@ request. Specify this
-- token if you plan to retry requests so that CloudFormation knows that
-- you\'re not attempting to execute a change set to update a stack with
-- the same name. You might retry @ExecuteChangeSet@ requests to ensure
-- that CloudFormation successfully received them.
executeChangeSet_clientRequestToken :: Lens.Lens' ExecuteChangeSet (Prelude.Maybe Prelude.Text)
executeChangeSet_clientRequestToken = Lens.lens (\ExecuteChangeSet' {clientRequestToken} -> clientRequestToken) (\s@ExecuteChangeSet' {} a -> s {clientRequestToken = a} :: ExecuteChangeSet)

-- | Preserves the state of previously provisioned resources when an
-- operation fails. This parameter can\'t be specified when the
-- @OnStackFailure@ parameter to the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
-- API operation was specified.
--
-- -   @True@ - if the stack creation fails, do nothing. This is equivalent
--     to specifying @DO_NOTHING@ for the @OnStackFailure@ parameter to the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
--     API operation.
--
-- -   @False@ - if the stack creation fails, roll back the stack. This is
--     equivalent to specifying @ROLLBACK@ for the @OnStackFailure@
--     parameter to the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
--     API operation.
--
-- Default: @True@
executeChangeSet_disableRollback :: Lens.Lens' ExecuteChangeSet (Prelude.Maybe Prelude.Bool)
executeChangeSet_disableRollback = Lens.lens (\ExecuteChangeSet' {disableRollback} -> disableRollback) (\s@ExecuteChangeSet' {} a -> s {disableRollback = a} :: ExecuteChangeSet)

-- | If you specified the name of a change set, specify the stack name or
-- Amazon Resource Name (ARN) that\'s associated with the change set you
-- want to execute.
executeChangeSet_stackName :: Lens.Lens' ExecuteChangeSet (Prelude.Maybe Prelude.Text)
executeChangeSet_stackName = Lens.lens (\ExecuteChangeSet' {stackName} -> stackName) (\s@ExecuteChangeSet' {} a -> s {stackName = a} :: ExecuteChangeSet)

-- | The name or Amazon Resource Name (ARN) of the change set that you want
-- use to update the specified stack.
executeChangeSet_changeSetName :: Lens.Lens' ExecuteChangeSet Prelude.Text
executeChangeSet_changeSetName = Lens.lens (\ExecuteChangeSet' {changeSetName} -> changeSetName) (\s@ExecuteChangeSet' {} a -> s {changeSetName = a} :: ExecuteChangeSet)

instance Core.AWSRequest ExecuteChangeSet where
  type
    AWSResponse ExecuteChangeSet =
      ExecuteChangeSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ExecuteChangeSetResult"
      ( \s h x ->
          ExecuteChangeSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteChangeSet where
  hashWithSalt _salt ExecuteChangeSet' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` disableRollback
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` changeSetName

instance Prelude.NFData ExecuteChangeSet where
  rnf ExecuteChangeSet' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf disableRollback
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf changeSetName

instance Data.ToHeaders ExecuteChangeSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ExecuteChangeSet where
  toPath = Prelude.const "/"

instance Data.ToQuery ExecuteChangeSet where
  toQuery ExecuteChangeSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ExecuteChangeSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "ClientRequestToken" Data.=: clientRequestToken,
        "DisableRollback" Data.=: disableRollback,
        "StackName" Data.=: stackName,
        "ChangeSetName" Data.=: changeSetName
      ]

-- | The output for the ExecuteChangeSet action.
--
-- /See:/ 'newExecuteChangeSetResponse' smart constructor.
data ExecuteChangeSetResponse = ExecuteChangeSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'executeChangeSetResponse_httpStatus' - The response's http status code.
newExecuteChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteChangeSetResponse
newExecuteChangeSetResponse pHttpStatus_ =
  ExecuteChangeSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
executeChangeSetResponse_httpStatus :: Lens.Lens' ExecuteChangeSetResponse Prelude.Int
executeChangeSetResponse_httpStatus = Lens.lens (\ExecuteChangeSetResponse' {httpStatus} -> httpStatus) (\s@ExecuteChangeSetResponse' {} a -> s {httpStatus = a} :: ExecuteChangeSetResponse)

instance Prelude.NFData ExecuteChangeSetResponse where
  rnf ExecuteChangeSetResponse' {..} =
    Prelude.rnf httpStatus

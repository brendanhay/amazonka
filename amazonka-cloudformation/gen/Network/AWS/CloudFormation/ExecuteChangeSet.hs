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
-- Module      : Network.AWS.CloudFormation.ExecuteChangeSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack using the input information that was provided when the
-- specified change set was created. After the call successfully completes,
-- AWS CloudFormation starts updating the stack. Use the DescribeStacks
-- action to view the status of the update.
--
-- When you execute a change set, AWS CloudFormation deletes all other
-- change sets associated with the stack because they aren\'t valid for the
-- updated stack.
--
-- If a stack policy is associated with the stack, AWS CloudFormation
-- enforces the policy during the update. You can\'t specify a temporary
-- stack policy that overrides the current policy.
--
-- To create a change set for the entire stack hierachy,
-- @IncludeNestedStacks@ must have been set to @True@.
module Network.AWS.CloudFormation.ExecuteChangeSet
  ( -- * Creating a Request
    ExecuteChangeSet (..),
    newExecuteChangeSet,

    -- * Request Lenses
    executeChangeSet_stackName,
    executeChangeSet_clientRequestToken,
    executeChangeSet_changeSetName,

    -- * Destructuring the Response
    ExecuteChangeSetResponse (..),
    newExecuteChangeSetResponse,

    -- * Response Lenses
    executeChangeSetResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ExecuteChangeSet action.
--
-- /See:/ 'newExecuteChangeSet' smart constructor.
data ExecuteChangeSet = ExecuteChangeSet'
  { -- | If you specified the name of a change set, specify the stack name or ID
    -- (ARN) that is associated with the change set you want to execute.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for this @ExecuteChangeSet@ request. Specify this
    -- token if you plan to retry requests so that AWS CloudFormation knows
    -- that you\'re not attempting to execute a change set to update a stack
    -- with the same name. You might retry @ExecuteChangeSet@ requests to
    -- ensure that AWS CloudFormation successfully received them.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the change set that you want use to update the
    -- specified stack.
    changeSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExecuteChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'executeChangeSet_stackName' - If you specified the name of a change set, specify the stack name or ID
-- (ARN) that is associated with the change set you want to execute.
--
-- 'clientRequestToken', 'executeChangeSet_clientRequestToken' - A unique identifier for this @ExecuteChangeSet@ request. Specify this
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to execute a change set to update a stack
-- with the same name. You might retry @ExecuteChangeSet@ requests to
-- ensure that AWS CloudFormation successfully received them.
--
-- 'changeSetName', 'executeChangeSet_changeSetName' - The name or ARN of the change set that you want use to update the
-- specified stack.
newExecuteChangeSet ::
  -- | 'changeSetName'
  Prelude.Text ->
  ExecuteChangeSet
newExecuteChangeSet pChangeSetName_ =
  ExecuteChangeSet'
    { stackName = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      changeSetName = pChangeSetName_
    }

-- | If you specified the name of a change set, specify the stack name or ID
-- (ARN) that is associated with the change set you want to execute.
executeChangeSet_stackName :: Lens.Lens' ExecuteChangeSet (Prelude.Maybe Prelude.Text)
executeChangeSet_stackName = Lens.lens (\ExecuteChangeSet' {stackName} -> stackName) (\s@ExecuteChangeSet' {} a -> s {stackName = a} :: ExecuteChangeSet)

-- | A unique identifier for this @ExecuteChangeSet@ request. Specify this
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to execute a change set to update a stack
-- with the same name. You might retry @ExecuteChangeSet@ requests to
-- ensure that AWS CloudFormation successfully received them.
executeChangeSet_clientRequestToken :: Lens.Lens' ExecuteChangeSet (Prelude.Maybe Prelude.Text)
executeChangeSet_clientRequestToken = Lens.lens (\ExecuteChangeSet' {clientRequestToken} -> clientRequestToken) (\s@ExecuteChangeSet' {} a -> s {clientRequestToken = a} :: ExecuteChangeSet)

-- | The name or ARN of the change set that you want use to update the
-- specified stack.
executeChangeSet_changeSetName :: Lens.Lens' ExecuteChangeSet Prelude.Text
executeChangeSet_changeSetName = Lens.lens (\ExecuteChangeSet' {changeSetName} -> changeSetName) (\s@ExecuteChangeSet' {} a -> s {changeSetName = a} :: ExecuteChangeSet)

instance Prelude.AWSRequest ExecuteChangeSet where
  type Rs ExecuteChangeSet = ExecuteChangeSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ExecuteChangeSetResult"
      ( \s h x ->
          ExecuteChangeSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteChangeSet

instance Prelude.NFData ExecuteChangeSet

instance Prelude.ToHeaders ExecuteChangeSet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ExecuteChangeSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ExecuteChangeSet where
  toQuery ExecuteChangeSet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ExecuteChangeSet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "StackName" Prelude.=: stackName,
        "ClientRequestToken" Prelude.=: clientRequestToken,
        "ChangeSetName" Prelude.=: changeSetName
      ]

-- | The output for the ExecuteChangeSet action.
--
-- /See:/ 'newExecuteChangeSetResponse' smart constructor.
data ExecuteChangeSetResponse = ExecuteChangeSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ExecuteChangeSetResponse

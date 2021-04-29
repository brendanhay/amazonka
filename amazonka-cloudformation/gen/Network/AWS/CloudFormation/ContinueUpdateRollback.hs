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
-- Module      : Network.AWS.CloudFormation.ContinueUpdateRollback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified stack that is in the @UPDATE_ROLLBACK_FAILED@ state,
-- continues rolling it back to the @UPDATE_ROLLBACK_COMPLETE@ state.
-- Depending on the cause of the failure, you can manually
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed fix the error>
-- and continue the rollback. By continuing the rollback, you can return
-- your stack to a working state (the @UPDATE_ROLLBACK_COMPLETE@ state),
-- and then try to update the stack again.
--
-- A stack goes into the @UPDATE_ROLLBACK_FAILED@ state when AWS
-- CloudFormation cannot roll back all changes after a failed stack update.
-- For example, you might have a stack that is rolling back to an old
-- database instance that was deleted outside of AWS CloudFormation.
-- Because AWS CloudFormation doesn\'t know the database was deleted, it
-- assumes that the database instance still exists and attempts to roll
-- back to it, causing the update rollback to fail.
module Network.AWS.CloudFormation.ContinueUpdateRollback
  ( -- * Creating a Request
    ContinueUpdateRollback (..),
    newContinueUpdateRollback,

    -- * Request Lenses
    continueUpdateRollback_roleARN,
    continueUpdateRollback_resourcesToSkip,
    continueUpdateRollback_clientRequestToken,
    continueUpdateRollback_stackName,

    -- * Destructuring the Response
    ContinueUpdateRollbackResponse (..),
    newContinueUpdateRollbackResponse,

    -- * Response Lenses
    continueUpdateRollbackResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ContinueUpdateRollback action.
--
-- /See:/ 'newContinueUpdateRollback' smart constructor.
data ContinueUpdateRollback = ContinueUpdateRollback'
  { -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
    -- (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS
    -- CloudFormation uses the role\'s credentials to make calls on your
    -- behalf. AWS CloudFormation always uses this role for all future
    -- operations on the stack. As long as users have permission to operate on
    -- the stack, AWS CloudFormation uses this role even if the users don\'t
    -- have permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don\'t specify a value, AWS CloudFormation uses the role that was
    -- previously associated with the stack. If no role is available, AWS
    -- CloudFormation uses a temporary session that is generated from your user
    -- credentials.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | A list of the logical IDs of the resources that AWS CloudFormation skips
    -- during the continue update rollback operation. You can specify only
    -- resources that are in the @UPDATE_FAILED@ state because a rollback
    -- failed. You can\'t specify resources that are in the @UPDATE_FAILED@
    -- state for other reasons, for example, because an update was cancelled.
    -- To check why a resource update failed, use the DescribeStackResources
    -- action, and view the resource status reason.
    --
    -- Specify this property to skip rolling back resources that AWS
    -- CloudFormation can\'t successfully roll back. We recommend that you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot>
    -- resources before skipping them. AWS CloudFormation sets the status of
    -- the specified resources to @UPDATE_COMPLETE@ and continues to roll back
    -- the stack. After the rollback is complete, the state of the skipped
    -- resources will be inconsistent with the state of the resources in the
    -- stack template. Before performing another stack update, you must update
    -- the stack or resources to be consistent with each other. If you don\'t,
    -- subsequent stack updates might fail, and the stack will become
    -- unrecoverable.
    --
    -- Specify the minimum number of resources required to successfully roll
    -- back your stack. For example, a failed resource update might cause
    -- dependent resources to fail. In this case, it might not be necessary to
    -- skip the dependent resources.
    --
    -- To skip resources that are part of nested stacks, use the following
    -- format: @NestedStackName.ResourceLogicalID@. If you want to specify the
    -- logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@) in
    -- the @ResourcesToSkip@ list, then its corresponding embedded stack must
    -- be in one of the following states: @DELETE_IN_PROGRESS@,
    -- @DELETE_COMPLETE@, or @DELETE_FAILED@.
    --
    -- Don\'t confuse a child stack\'s name with its corresponding logical ID
    -- defined in the parent stack. For an example of a continue update
    -- rollback operation with nested stacks, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html#nested-stacks Using ResourcesToSkip to recover a nested stacks hierarchy>.
    resourcesToSkip :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for this @ContinueUpdateRollback@ request. Specify
    -- this token if you plan to retry requests so that AWS CloudFormation
    -- knows that you\'re not attempting to continue the rollback to a stack
    -- with the same name. You might retry @ContinueUpdateRollback@ requests to
    -- ensure that AWS CloudFormation successfully received them.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name or the unique ID of the stack that you want to continue rolling
    -- back.
    --
    -- Don\'t specify the name of a nested stack (a stack that was created by
    -- using the @AWS::CloudFormation::Stack@ resource). Instead, use this
    -- operation on the parent stack (the stack that contains the
    -- @AWS::CloudFormation::Stack@ resource).
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContinueUpdateRollback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'continueUpdateRollback_roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS
-- CloudFormation uses the role\'s credentials to make calls on your
-- behalf. AWS CloudFormation always uses this role for all future
-- operations on the stack. As long as users have permission to operate on
-- the stack, AWS CloudFormation uses this role even if the users don\'t
-- have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, AWS CloudFormation uses the role that was
-- previously associated with the stack. If no role is available, AWS
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
--
-- 'resourcesToSkip', 'continueUpdateRollback_resourcesToSkip' - A list of the logical IDs of the resources that AWS CloudFormation skips
-- during the continue update rollback operation. You can specify only
-- resources that are in the @UPDATE_FAILED@ state because a rollback
-- failed. You can\'t specify resources that are in the @UPDATE_FAILED@
-- state for other reasons, for example, because an update was cancelled.
-- To check why a resource update failed, use the DescribeStackResources
-- action, and view the resource status reason.
--
-- Specify this property to skip rolling back resources that AWS
-- CloudFormation can\'t successfully roll back. We recommend that you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot>
-- resources before skipping them. AWS CloudFormation sets the status of
-- the specified resources to @UPDATE_COMPLETE@ and continues to roll back
-- the stack. After the rollback is complete, the state of the skipped
-- resources will be inconsistent with the state of the resources in the
-- stack template. Before performing another stack update, you must update
-- the stack or resources to be consistent with each other. If you don\'t,
-- subsequent stack updates might fail, and the stack will become
-- unrecoverable.
--
-- Specify the minimum number of resources required to successfully roll
-- back your stack. For example, a failed resource update might cause
-- dependent resources to fail. In this case, it might not be necessary to
-- skip the dependent resources.
--
-- To skip resources that are part of nested stacks, use the following
-- format: @NestedStackName.ResourceLogicalID@. If you want to specify the
-- logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@) in
-- the @ResourcesToSkip@ list, then its corresponding embedded stack must
-- be in one of the following states: @DELETE_IN_PROGRESS@,
-- @DELETE_COMPLETE@, or @DELETE_FAILED@.
--
-- Don\'t confuse a child stack\'s name with its corresponding logical ID
-- defined in the parent stack. For an example of a continue update
-- rollback operation with nested stacks, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html#nested-stacks Using ResourcesToSkip to recover a nested stacks hierarchy>.
--
-- 'clientRequestToken', 'continueUpdateRollback_clientRequestToken' - A unique identifier for this @ContinueUpdateRollback@ request. Specify
-- this token if you plan to retry requests so that AWS CloudFormation
-- knows that you\'re not attempting to continue the rollback to a stack
-- with the same name. You might retry @ContinueUpdateRollback@ requests to
-- ensure that AWS CloudFormation successfully received them.
--
-- 'stackName', 'continueUpdateRollback_stackName' - The name or the unique ID of the stack that you want to continue rolling
-- back.
--
-- Don\'t specify the name of a nested stack (a stack that was created by
-- using the @AWS::CloudFormation::Stack@ resource). Instead, use this
-- operation on the parent stack (the stack that contains the
-- @AWS::CloudFormation::Stack@ resource).
newContinueUpdateRollback ::
  -- | 'stackName'
  Prelude.Text ->
  ContinueUpdateRollback
newContinueUpdateRollback pStackName_ =
  ContinueUpdateRollback'
    { roleARN = Prelude.Nothing,
      resourcesToSkip = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      stackName = pStackName_
    }

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS
-- CloudFormation uses the role\'s credentials to make calls on your
-- behalf. AWS CloudFormation always uses this role for all future
-- operations on the stack. As long as users have permission to operate on
-- the stack, AWS CloudFormation uses this role even if the users don\'t
-- have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, AWS CloudFormation uses the role that was
-- previously associated with the stack. If no role is available, AWS
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
continueUpdateRollback_roleARN :: Lens.Lens' ContinueUpdateRollback (Prelude.Maybe Prelude.Text)
continueUpdateRollback_roleARN = Lens.lens (\ContinueUpdateRollback' {roleARN} -> roleARN) (\s@ContinueUpdateRollback' {} a -> s {roleARN = a} :: ContinueUpdateRollback)

-- | A list of the logical IDs of the resources that AWS CloudFormation skips
-- during the continue update rollback operation. You can specify only
-- resources that are in the @UPDATE_FAILED@ state because a rollback
-- failed. You can\'t specify resources that are in the @UPDATE_FAILED@
-- state for other reasons, for example, because an update was cancelled.
-- To check why a resource update failed, use the DescribeStackResources
-- action, and view the resource status reason.
--
-- Specify this property to skip rolling back resources that AWS
-- CloudFormation can\'t successfully roll back. We recommend that you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot>
-- resources before skipping them. AWS CloudFormation sets the status of
-- the specified resources to @UPDATE_COMPLETE@ and continues to roll back
-- the stack. After the rollback is complete, the state of the skipped
-- resources will be inconsistent with the state of the resources in the
-- stack template. Before performing another stack update, you must update
-- the stack or resources to be consistent with each other. If you don\'t,
-- subsequent stack updates might fail, and the stack will become
-- unrecoverable.
--
-- Specify the minimum number of resources required to successfully roll
-- back your stack. For example, a failed resource update might cause
-- dependent resources to fail. In this case, it might not be necessary to
-- skip the dependent resources.
--
-- To skip resources that are part of nested stacks, use the following
-- format: @NestedStackName.ResourceLogicalID@. If you want to specify the
-- logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@) in
-- the @ResourcesToSkip@ list, then its corresponding embedded stack must
-- be in one of the following states: @DELETE_IN_PROGRESS@,
-- @DELETE_COMPLETE@, or @DELETE_FAILED@.
--
-- Don\'t confuse a child stack\'s name with its corresponding logical ID
-- defined in the parent stack. For an example of a continue update
-- rollback operation with nested stacks, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html#nested-stacks Using ResourcesToSkip to recover a nested stacks hierarchy>.
continueUpdateRollback_resourcesToSkip :: Lens.Lens' ContinueUpdateRollback (Prelude.Maybe [Prelude.Text])
continueUpdateRollback_resourcesToSkip = Lens.lens (\ContinueUpdateRollback' {resourcesToSkip} -> resourcesToSkip) (\s@ContinueUpdateRollback' {} a -> s {resourcesToSkip = a} :: ContinueUpdateRollback) Prelude.. Lens.mapping Prelude._Coerce

-- | A unique identifier for this @ContinueUpdateRollback@ request. Specify
-- this token if you plan to retry requests so that AWS CloudFormation
-- knows that you\'re not attempting to continue the rollback to a stack
-- with the same name. You might retry @ContinueUpdateRollback@ requests to
-- ensure that AWS CloudFormation successfully received them.
continueUpdateRollback_clientRequestToken :: Lens.Lens' ContinueUpdateRollback (Prelude.Maybe Prelude.Text)
continueUpdateRollback_clientRequestToken = Lens.lens (\ContinueUpdateRollback' {clientRequestToken} -> clientRequestToken) (\s@ContinueUpdateRollback' {} a -> s {clientRequestToken = a} :: ContinueUpdateRollback)

-- | The name or the unique ID of the stack that you want to continue rolling
-- back.
--
-- Don\'t specify the name of a nested stack (a stack that was created by
-- using the @AWS::CloudFormation::Stack@ resource). Instead, use this
-- operation on the parent stack (the stack that contains the
-- @AWS::CloudFormation::Stack@ resource).
continueUpdateRollback_stackName :: Lens.Lens' ContinueUpdateRollback Prelude.Text
continueUpdateRollback_stackName = Lens.lens (\ContinueUpdateRollback' {stackName} -> stackName) (\s@ContinueUpdateRollback' {} a -> s {stackName = a} :: ContinueUpdateRollback)

instance Prelude.AWSRequest ContinueUpdateRollback where
  type
    Rs ContinueUpdateRollback =
      ContinueUpdateRollbackResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ContinueUpdateRollbackResult"
      ( \s h x ->
          ContinueUpdateRollbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ContinueUpdateRollback

instance Prelude.NFData ContinueUpdateRollback

instance Prelude.ToHeaders ContinueUpdateRollback where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ContinueUpdateRollback where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ContinueUpdateRollback where
  toQuery ContinueUpdateRollback' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ContinueUpdateRollback" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "RoleARN" Prelude.=: roleARN,
        "ResourcesToSkip"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> resourcesToSkip
            ),
        "ClientRequestToken" Prelude.=: clientRequestToken,
        "StackName" Prelude.=: stackName
      ]

-- | The output for a ContinueUpdateRollback action.
--
-- /See:/ 'newContinueUpdateRollbackResponse' smart constructor.
data ContinueUpdateRollbackResponse = ContinueUpdateRollbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContinueUpdateRollbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'continueUpdateRollbackResponse_httpStatus' - The response's http status code.
newContinueUpdateRollbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ContinueUpdateRollbackResponse
newContinueUpdateRollbackResponse pHttpStatus_ =
  ContinueUpdateRollbackResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
continueUpdateRollbackResponse_httpStatus :: Lens.Lens' ContinueUpdateRollbackResponse Prelude.Int
continueUpdateRollbackResponse_httpStatus = Lens.lens (\ContinueUpdateRollbackResponse' {httpStatus} -> httpStatus) (\s@ContinueUpdateRollbackResponse' {} a -> s {httpStatus = a} :: ContinueUpdateRollbackResponse)

instance
  Prelude.NFData
    ContinueUpdateRollbackResponse

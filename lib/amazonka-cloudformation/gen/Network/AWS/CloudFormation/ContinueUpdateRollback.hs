{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ContinueUpdateRollback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified stack that is in the @UPDATE_ROLLBACK_FAILED@ state, continues rolling it back to the @UPDATE_ROLLBACK_COMPLETE@ state. Depending on the cause of the failure, you can manually <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed fix the error> and continue the rollback. By continuing the rollback, you can return your stack to a working state (the @UPDATE_ROLLBACK_COMPLETE@ state), and then try to update the stack again.
--
-- A stack goes into the @UPDATE_ROLLBACK_FAILED@ state when AWS CloudFormation cannot roll back all changes after a failed stack update. For example, you might have a stack that is rolling back to an old database instance that was deleted outside of AWS CloudFormation. Because AWS CloudFormation doesn't know the database was deleted, it assumes that the database instance still exists and attempts to roll back to it, causing the update rollback to fail.
module Network.AWS.CloudFormation.ContinueUpdateRollback
  ( -- * Creating a request
    ContinueUpdateRollback (..),
    mkContinueUpdateRollback,

    -- ** Request lenses
    curResourcesToSkip,
    curClientRequestToken,
    curRoleARN,
    curStackName,

    -- * Destructuring the response
    ContinueUpdateRollbackResponse (..),
    mkContinueUpdateRollbackResponse,

    -- ** Response lenses
    currsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'ContinueUpdateRollback' action.
--
-- /See:/ 'mkContinueUpdateRollback' smart constructor.
data ContinueUpdateRollback = ContinueUpdateRollback'
  { -- | A list of the logical IDs of the resources that AWS CloudFormation skips during the continue update rollback operation. You can specify only resources that are in the @UPDATE_FAILED@ state because a rollback failed. You can't specify resources that are in the @UPDATE_FAILED@ state for other reasons, for example, because an update was cancelled. To check why a resource update failed, use the 'DescribeStackResources' action, and view the resource status reason.
    --
    -- /Important:/ Specify this property to skip rolling back resources that AWS CloudFormation can't successfully roll back. We recommend that you <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot> resources before skipping them. AWS CloudFormation sets the status of the specified resources to @UPDATE_COMPLETE@ and continues to roll back the stack. After the rollback is complete, the state of the skipped resources will be inconsistent with the state of the resources in the stack template. Before performing another stack update, you must update the stack or resources to be consistent with each other. If you don't, subsequent stack updates might fail, and the stack will become unrecoverable.
    -- Specify the minimum number of resources required to successfully roll back your stack. For example, a failed resource update might cause dependent resources to fail. In this case, it might not be necessary to skip the dependent resources.
    -- To skip resources that are part of nested stacks, use the following format: @NestedStackName.ResourceLogicalID@ . If you want to specify the logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@ ) in the @ResourcesToSkip@ list, then its corresponding embedded stack must be in one of the following states: @DELETE_IN_PROGRESS@ , @DELETE_COMPLETE@ , or @DELETE_FAILED@ .
    resourcesToSkip :: Lude.Maybe [Lude.Text],
    -- | A unique identifier for this @ContinueUpdateRollback@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to continue the rollback to a stack with the same name. You might retry @ContinueUpdateRollback@ requests to ensure that AWS CloudFormation successfully received them.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The name or the unique ID of the stack that you want to continue rolling back.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinueUpdateRollback' with the minimum fields required to make a request.
--
-- * 'resourcesToSkip' - A list of the logical IDs of the resources that AWS CloudFormation skips during the continue update rollback operation. You can specify only resources that are in the @UPDATE_FAILED@ state because a rollback failed. You can't specify resources that are in the @UPDATE_FAILED@ state for other reasons, for example, because an update was cancelled. To check why a resource update failed, use the 'DescribeStackResources' action, and view the resource status reason.
--
-- /Important:/ Specify this property to skip rolling back resources that AWS CloudFormation can't successfully roll back. We recommend that you <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot> resources before skipping them. AWS CloudFormation sets the status of the specified resources to @UPDATE_COMPLETE@ and continues to roll back the stack. After the rollback is complete, the state of the skipped resources will be inconsistent with the state of the resources in the stack template. Before performing another stack update, you must update the stack or resources to be consistent with each other. If you don't, subsequent stack updates might fail, and the stack will become unrecoverable.
-- Specify the minimum number of resources required to successfully roll back your stack. For example, a failed resource update might cause dependent resources to fail. In this case, it might not be necessary to skip the dependent resources.
-- To skip resources that are part of nested stacks, use the following format: @NestedStackName.ResourceLogicalID@ . If you want to specify the logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@ ) in the @ResourcesToSkip@ list, then its corresponding embedded stack must be in one of the following states: @DELETE_IN_PROGRESS@ , @DELETE_COMPLETE@ , or @DELETE_FAILED@ .
-- * 'clientRequestToken' - A unique identifier for this @ContinueUpdateRollback@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to continue the rollback to a stack with the same name. You might retry @ContinueUpdateRollback@ requests to ensure that AWS CloudFormation successfully received them.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
-- * 'stackName' - The name or the unique ID of the stack that you want to continue rolling back.
mkContinueUpdateRollback ::
  -- | 'stackName'
  Lude.Text ->
  ContinueUpdateRollback
mkContinueUpdateRollback pStackName_ =
  ContinueUpdateRollback'
    { resourcesToSkip = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      roleARN = Lude.Nothing,
      stackName = pStackName_
    }

-- | A list of the logical IDs of the resources that AWS CloudFormation skips during the continue update rollback operation. You can specify only resources that are in the @UPDATE_FAILED@ state because a rollback failed. You can't specify resources that are in the @UPDATE_FAILED@ state for other reasons, for example, because an update was cancelled. To check why a resource update failed, use the 'DescribeStackResources' action, and view the resource status reason.
--
-- /Important:/ Specify this property to skip rolling back resources that AWS CloudFormation can't successfully roll back. We recommend that you <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot> resources before skipping them. AWS CloudFormation sets the status of the specified resources to @UPDATE_COMPLETE@ and continues to roll back the stack. After the rollback is complete, the state of the skipped resources will be inconsistent with the state of the resources in the stack template. Before performing another stack update, you must update the stack or resources to be consistent with each other. If you don't, subsequent stack updates might fail, and the stack will become unrecoverable.
-- Specify the minimum number of resources required to successfully roll back your stack. For example, a failed resource update might cause dependent resources to fail. In this case, it might not be necessary to skip the dependent resources.
-- To skip resources that are part of nested stacks, use the following format: @NestedStackName.ResourceLogicalID@ . If you want to specify the logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@ ) in the @ResourcesToSkip@ list, then its corresponding embedded stack must be in one of the following states: @DELETE_IN_PROGRESS@ , @DELETE_COMPLETE@ , or @DELETE_FAILED@ .
--
-- /Note:/ Consider using 'resourcesToSkip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curResourcesToSkip :: Lens.Lens' ContinueUpdateRollback (Lude.Maybe [Lude.Text])
curResourcesToSkip = Lens.lens (resourcesToSkip :: ContinueUpdateRollback -> Lude.Maybe [Lude.Text]) (\s a -> s {resourcesToSkip = a} :: ContinueUpdateRollback)
{-# DEPRECATED curResourcesToSkip "Use generic-lens or generic-optics with 'resourcesToSkip' instead." #-}

-- | A unique identifier for this @ContinueUpdateRollback@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to continue the rollback to a stack with the same name. You might retry @ContinueUpdateRollback@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curClientRequestToken :: Lens.Lens' ContinueUpdateRollback (Lude.Maybe Lude.Text)
curClientRequestToken = Lens.lens (clientRequestToken :: ContinueUpdateRollback -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: ContinueUpdateRollback)
{-# DEPRECATED curClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curRoleARN :: Lens.Lens' ContinueUpdateRollback (Lude.Maybe Lude.Text)
curRoleARN = Lens.lens (roleARN :: ContinueUpdateRollback -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ContinueUpdateRollback)
{-# DEPRECATED curRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name or the unique ID of the stack that you want to continue rolling back.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curStackName :: Lens.Lens' ContinueUpdateRollback Lude.Text
curStackName = Lens.lens (stackName :: ContinueUpdateRollback -> Lude.Text) (\s a -> s {stackName = a} :: ContinueUpdateRollback)
{-# DEPRECATED curStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest ContinueUpdateRollback where
  type Rs ContinueUpdateRollback = ContinueUpdateRollbackResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ContinueUpdateRollbackResult"
      ( \s h x ->
          ContinueUpdateRollbackResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ContinueUpdateRollback where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ContinueUpdateRollback where
  toPath = Lude.const "/"

instance Lude.ToQuery ContinueUpdateRollback where
  toQuery ContinueUpdateRollback' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ContinueUpdateRollback" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "ResourcesToSkip"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> resourcesToSkip),
        "ClientRequestToken" Lude.=: clientRequestToken,
        "RoleARN" Lude.=: roleARN,
        "StackName" Lude.=: stackName
      ]

-- | The output for a 'ContinueUpdateRollback' action.
--
-- /See:/ 'mkContinueUpdateRollbackResponse' smart constructor.
newtype ContinueUpdateRollbackResponse = ContinueUpdateRollbackResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinueUpdateRollbackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkContinueUpdateRollbackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ContinueUpdateRollbackResponse
mkContinueUpdateRollbackResponse pResponseStatus_ =
  ContinueUpdateRollbackResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' ContinueUpdateRollbackResponse Lude.Int
currsResponseStatus = Lens.lens (responseStatus :: ContinueUpdateRollbackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ContinueUpdateRollbackResponse)
{-# DEPRECATED currsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

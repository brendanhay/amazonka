{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ContinueUpdateRollback (..)
    , mkContinueUpdateRollback
    -- ** Request lenses
    , curStackName
    , curClientRequestToken
    , curResourcesToSkip
    , curRoleARN

    -- * Destructuring the response
    , ContinueUpdateRollbackResponse (..)
    , mkContinueUpdateRollbackResponse
    -- ** Response lenses
    , currrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'ContinueUpdateRollback' action.
--
-- /See:/ 'mkContinueUpdateRollback' smart constructor.
data ContinueUpdateRollback = ContinueUpdateRollback'
  { stackName :: Types.StackName
    -- ^ The name or the unique ID of the stack that you want to continue rolling back.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique identifier for this @ContinueUpdateRollback@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to continue the rollback to a stack with the same name. You might retry @ContinueUpdateRollback@ requests to ensure that AWS CloudFormation successfully received them.
  , resourcesToSkip :: Core.Maybe [Types.ResourceToSkip]
    -- ^ A list of the logical IDs of the resources that AWS CloudFormation skips during the continue update rollback operation. You can specify only resources that are in the @UPDATE_FAILED@ state because a rollback failed. You can't specify resources that are in the @UPDATE_FAILED@ state for other reasons, for example, because an update was cancelled. To check why a resource update failed, use the 'DescribeStackResources' action, and view the resource status reason. 
--
-- /Important:/ Specify this property to skip rolling back resources that AWS CloudFormation can't successfully roll back. We recommend that you <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot> resources before skipping them. AWS CloudFormation sets the status of the specified resources to @UPDATE_COMPLETE@ and continues to roll back the stack. After the rollback is complete, the state of the skipped resources will be inconsistent with the state of the resources in the stack template. Before performing another stack update, you must update the stack or resources to be consistent with each other. If you don't, subsequent stack updates might fail, and the stack will become unrecoverable. 
-- Specify the minimum number of resources required to successfully roll back your stack. For example, a failed resource update might cause dependent resources to fail. In this case, it might not be necessary to skip the dependent resources. 
-- To skip resources that are part of nested stacks, use the following format: @NestedStackName.ResourceLogicalID@ . If you want to specify the logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@ ) in the @ResourcesToSkip@ list, then its corresponding embedded stack must be in one of the following states: @DELETE_IN_PROGRESS@ , @DELETE_COMPLETE@ , or @DELETE_FAILED@ . 
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContinueUpdateRollback' value with any optional fields omitted.
mkContinueUpdateRollback
    :: Types.StackName -- ^ 'stackName'
    -> ContinueUpdateRollback
mkContinueUpdateRollback stackName
  = ContinueUpdateRollback'{stackName,
                            clientRequestToken = Core.Nothing, resourcesToSkip = Core.Nothing,
                            roleARN = Core.Nothing}

-- | The name or the unique ID of the stack that you want to continue rolling back.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curStackName :: Lens.Lens' ContinueUpdateRollback Types.StackName
curStackName = Lens.field @"stackName"
{-# INLINEABLE curStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

-- | A unique identifier for this @ContinueUpdateRollback@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to continue the rollback to a stack with the same name. You might retry @ContinueUpdateRollback@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curClientRequestToken :: Lens.Lens' ContinueUpdateRollback (Core.Maybe Types.ClientRequestToken)
curClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE curClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | A list of the logical IDs of the resources that AWS CloudFormation skips during the continue update rollback operation. You can specify only resources that are in the @UPDATE_FAILED@ state because a rollback failed. You can't specify resources that are in the @UPDATE_FAILED@ state for other reasons, for example, because an update was cancelled. To check why a resource update failed, use the 'DescribeStackResources' action, and view the resource status reason. 
--
-- /Important:/ Specify this property to skip rolling back resources that AWS CloudFormation can't successfully roll back. We recommend that you <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot> resources before skipping them. AWS CloudFormation sets the status of the specified resources to @UPDATE_COMPLETE@ and continues to roll back the stack. After the rollback is complete, the state of the skipped resources will be inconsistent with the state of the resources in the stack template. Before performing another stack update, you must update the stack or resources to be consistent with each other. If you don't, subsequent stack updates might fail, and the stack will become unrecoverable. 
-- Specify the minimum number of resources required to successfully roll back your stack. For example, a failed resource update might cause dependent resources to fail. In this case, it might not be necessary to skip the dependent resources. 
-- To skip resources that are part of nested stacks, use the following format: @NestedStackName.ResourceLogicalID@ . If you want to specify the logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@ ) in the @ResourcesToSkip@ list, then its corresponding embedded stack must be in one of the following states: @DELETE_IN_PROGRESS@ , @DELETE_COMPLETE@ , or @DELETE_FAILED@ . 
--
-- /Note:/ Consider using 'resourcesToSkip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curResourcesToSkip :: Lens.Lens' ContinueUpdateRollback (Core.Maybe [Types.ResourceToSkip])
curResourcesToSkip = Lens.field @"resourcesToSkip"
{-# INLINEABLE curResourcesToSkip #-}
{-# DEPRECATED resourcesToSkip "Use generic-lens or generic-optics with 'resourcesToSkip' instead"  #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
curRoleARN :: Lens.Lens' ContinueUpdateRollback (Core.Maybe Types.RoleARN)
curRoleARN = Lens.field @"roleARN"
{-# INLINEABLE curRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.ToQuery ContinueUpdateRollback where
        toQuery ContinueUpdateRollback{..}
          = Core.toQueryPair "Action" ("ContinueUpdateRollback" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackName" stackName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientRequestToken")
                clientRequestToken
              Core.<>
              Core.toQueryPair "ResourcesToSkip"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   resourcesToSkip)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "RoleARN") roleARN

instance Core.ToHeaders ContinueUpdateRollback where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ContinueUpdateRollback where
        type Rs ContinueUpdateRollback = ContinueUpdateRollbackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ContinueUpdateRollbackResult"
              (\ s h x ->
                 ContinueUpdateRollbackResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output for a 'ContinueUpdateRollback' action.
--
-- /See:/ 'mkContinueUpdateRollbackResponse' smart constructor.
newtype ContinueUpdateRollbackResponse = ContinueUpdateRollbackResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ContinueUpdateRollbackResponse' value with any optional fields omitted.
mkContinueUpdateRollbackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ContinueUpdateRollbackResponse
mkContinueUpdateRollbackResponse responseStatus
  = ContinueUpdateRollbackResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currrsResponseStatus :: Lens.Lens' ContinueUpdateRollbackResponse Core.Int
currrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE currrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

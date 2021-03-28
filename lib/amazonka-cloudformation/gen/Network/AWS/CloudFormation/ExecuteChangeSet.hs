{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ExecuteChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack using the input information that was provided when the specified change set was created. After the call successfully completes, AWS CloudFormation starts updating the stack. Use the 'DescribeStacks' action to view the status of the update.
--
-- When you execute a change set, AWS CloudFormation deletes all other change sets associated with the stack because they aren't valid for the updated stack.
-- If a stack policy is associated with the stack, AWS CloudFormation enforces the policy during the update. You can't specify a temporary stack policy that overrides the current policy.
-- To create a change set for the entire stack hierachy, @IncludeNestedStacks@ must have been set to @True@ .
module Network.AWS.CloudFormation.ExecuteChangeSet
    (
    -- * Creating a request
      ExecuteChangeSet (..)
    , mkExecuteChangeSet
    -- ** Request lenses
    , ecsChangeSetName
    , ecsClientRequestToken
    , ecsStackName

    -- * Destructuring the response
    , ExecuteChangeSetResponse (..)
    , mkExecuteChangeSetResponse
    -- ** Response lenses
    , ecsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'ExecuteChangeSet' action.
--
-- /See:/ 'mkExecuteChangeSet' smart constructor.
data ExecuteChangeSet = ExecuteChangeSet'
  { changeSetName :: Types.ChangeSetNameOrId
    -- ^ The name or ARN of the change set that you want use to update the specified stack.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique identifier for this @ExecuteChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to execute a change set to update a stack with the same name. You might retry @ExecuteChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
  , stackName :: Core.Maybe Types.StackName
    -- ^ If you specified the name of a change set, specify the stack name or ID (ARN) that is associated with the change set you want to execute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteChangeSet' value with any optional fields omitted.
mkExecuteChangeSet
    :: Types.ChangeSetNameOrId -- ^ 'changeSetName'
    -> ExecuteChangeSet
mkExecuteChangeSet changeSetName
  = ExecuteChangeSet'{changeSetName,
                      clientRequestToken = Core.Nothing, stackName = Core.Nothing}

-- | The name or ARN of the change set that you want use to update the specified stack.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsChangeSetName :: Lens.Lens' ExecuteChangeSet Types.ChangeSetNameOrId
ecsChangeSetName = Lens.field @"changeSetName"
{-# INLINEABLE ecsChangeSetName #-}
{-# DEPRECATED changeSetName "Use generic-lens or generic-optics with 'changeSetName' instead"  #-}

-- | A unique identifier for this @ExecuteChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to execute a change set to update a stack with the same name. You might retry @ExecuteChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsClientRequestToken :: Lens.Lens' ExecuteChangeSet (Core.Maybe Types.ClientRequestToken)
ecsClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE ecsClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | If you specified the name of a change set, specify the stack name or ID (ARN) that is associated with the change set you want to execute.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsStackName :: Lens.Lens' ExecuteChangeSet (Core.Maybe Types.StackName)
ecsStackName = Lens.field @"stackName"
{-# INLINEABLE ecsStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

instance Core.ToQuery ExecuteChangeSet where
        toQuery ExecuteChangeSet{..}
          = Core.toQueryPair "Action" ("ExecuteChangeSet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "ChangeSetName" changeSetName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientRequestToken")
                clientRequestToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackName") stackName

instance Core.ToHeaders ExecuteChangeSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ExecuteChangeSet where
        type Rs ExecuteChangeSet = ExecuteChangeSetResponse
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
          = Response.receiveXMLWrapper "ExecuteChangeSetResult"
              (\ s h x ->
                 ExecuteChangeSetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'ExecuteChangeSet' action.
--
-- /See:/ 'mkExecuteChangeSetResponse' smart constructor.
newtype ExecuteChangeSetResponse = ExecuteChangeSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteChangeSetResponse' value with any optional fields omitted.
mkExecuteChangeSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExecuteChangeSetResponse
mkExecuteChangeSetResponse responseStatus
  = ExecuteChangeSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsrrsResponseStatus :: Lens.Lens' ExecuteChangeSetResponse Core.Int
ecsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ecsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ExecuteChangeSet (..),
    mkExecuteChangeSet,

    -- ** Request lenses
    ecsChangeSetName,
    ecsClientRequestToken,
    ecsStackName,

    -- * Destructuring the response
    ExecuteChangeSetResponse (..),
    mkExecuteChangeSetResponse,

    -- ** Response lenses
    ecsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'ExecuteChangeSet' action.
--
-- /See:/ 'mkExecuteChangeSet' smart constructor.
data ExecuteChangeSet = ExecuteChangeSet'
  { -- | The name or ARN of the change set that you want use to update the specified stack.
    changeSetName :: Types.ChangeSetNameOrId,
    -- | A unique identifier for this @ExecuteChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to execute a change set to update a stack with the same name. You might retry @ExecuteChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | If you specified the name of a change set, specify the stack name or ID (ARN) that is associated with the change set you want to execute.
    stackName :: Core.Maybe Types.StackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteChangeSet' value with any optional fields omitted.
mkExecuteChangeSet ::
  -- | 'changeSetName'
  Types.ChangeSetNameOrId ->
  ExecuteChangeSet
mkExecuteChangeSet changeSetName =
  ExecuteChangeSet'
    { changeSetName,
      clientRequestToken = Core.Nothing,
      stackName = Core.Nothing
    }

-- | The name or ARN of the change set that you want use to update the specified stack.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsChangeSetName :: Lens.Lens' ExecuteChangeSet Types.ChangeSetNameOrId
ecsChangeSetName = Lens.field @"changeSetName"
{-# DEPRECATED ecsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | A unique identifier for this @ExecuteChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to execute a change set to update a stack with the same name. You might retry @ExecuteChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsClientRequestToken :: Lens.Lens' ExecuteChangeSet (Core.Maybe Types.ClientRequestToken)
ecsClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED ecsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | If you specified the name of a change set, specify the stack name or ID (ARN) that is associated with the change set you want to execute.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsStackName :: Lens.Lens' ExecuteChangeSet (Core.Maybe Types.StackName)
ecsStackName = Lens.field @"stackName"
{-# DEPRECATED ecsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.AWSRequest ExecuteChangeSet where
  type Rs ExecuteChangeSet = ExecuteChangeSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ExecuteChangeSet")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "ChangeSetName" changeSetName)
                Core.<> ( Core.toQueryValue "ClientRequestToken"
                            Core.<$> clientRequestToken
                        )
                Core.<> (Core.toQueryValue "StackName" Core.<$> stackName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ExecuteChangeSetResult"
      ( \s h x ->
          ExecuteChangeSetResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The output for the 'ExecuteChangeSet' action.
--
-- /See:/ 'mkExecuteChangeSetResponse' smart constructor.
newtype ExecuteChangeSetResponse = ExecuteChangeSetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteChangeSetResponse' value with any optional fields omitted.
mkExecuteChangeSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExecuteChangeSetResponse
mkExecuteChangeSetResponse responseStatus =
  ExecuteChangeSetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsrrsResponseStatus :: Lens.Lens' ExecuteChangeSetResponse Core.Int
ecsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ecsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
    ecsrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'ExecuteChangeSet' action.
--
-- /See:/ 'mkExecuteChangeSet' smart constructor.
data ExecuteChangeSet = ExecuteChangeSet'
  { -- | The name or ARN of the change set that you want use to update the specified stack.
    changeSetName :: Lude.Text,
    -- | A unique identifier for this @ExecuteChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to execute a change set to update a stack with the same name. You might retry @ExecuteChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | If you specified the name of a change set, specify the stack name or ID (ARN) that is associated with the change set you want to execute.
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteChangeSet' with the minimum fields required to make a request.
--
-- * 'changeSetName' - The name or ARN of the change set that you want use to update the specified stack.
-- * 'clientRequestToken' - A unique identifier for this @ExecuteChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to execute a change set to update a stack with the same name. You might retry @ExecuteChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
-- * 'stackName' - If you specified the name of a change set, specify the stack name or ID (ARN) that is associated with the change set you want to execute.
mkExecuteChangeSet ::
  -- | 'changeSetName'
  Lude.Text ->
  ExecuteChangeSet
mkExecuteChangeSet pChangeSetName_ =
  ExecuteChangeSet'
    { changeSetName = pChangeSetName_,
      clientRequestToken = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | The name or ARN of the change set that you want use to update the specified stack.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsChangeSetName :: Lens.Lens' ExecuteChangeSet Lude.Text
ecsChangeSetName = Lens.lens (changeSetName :: ExecuteChangeSet -> Lude.Text) (\s a -> s {changeSetName = a} :: ExecuteChangeSet)
{-# DEPRECATED ecsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | A unique identifier for this @ExecuteChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to execute a change set to update a stack with the same name. You might retry @ExecuteChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsClientRequestToken :: Lens.Lens' ExecuteChangeSet (Lude.Maybe Lude.Text)
ecsClientRequestToken = Lens.lens (clientRequestToken :: ExecuteChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: ExecuteChangeSet)
{-# DEPRECATED ecsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | If you specified the name of a change set, specify the stack name or ID (ARN) that is associated with the change set you want to execute.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsStackName :: Lens.Lens' ExecuteChangeSet (Lude.Maybe Lude.Text)
ecsStackName = Lens.lens (stackName :: ExecuteChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: ExecuteChangeSet)
{-# DEPRECATED ecsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest ExecuteChangeSet where
  type Rs ExecuteChangeSet = ExecuteChangeSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ExecuteChangeSetResult"
      ( \s h x ->
          ExecuteChangeSetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExecuteChangeSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ExecuteChangeSet where
  toPath = Lude.const "/"

instance Lude.ToQuery ExecuteChangeSet where
  toQuery ExecuteChangeSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ExecuteChangeSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "ChangeSetName" Lude.=: changeSetName,
        "ClientRequestToken" Lude.=: clientRequestToken,
        "StackName" Lude.=: stackName
      ]

-- | The output for the 'ExecuteChangeSet' action.
--
-- /See:/ 'mkExecuteChangeSetResponse' smart constructor.
newtype ExecuteChangeSetResponse = ExecuteChangeSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteChangeSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkExecuteChangeSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExecuteChangeSetResponse
mkExecuteChangeSetResponse pResponseStatus_ =
  ExecuteChangeSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsrsResponseStatus :: Lens.Lens' ExecuteChangeSetResponse Lude.Int
ecsrsResponseStatus = Lens.lens (responseStatus :: ExecuteChangeSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExecuteChangeSetResponse)
{-# DEPRECATED ecsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

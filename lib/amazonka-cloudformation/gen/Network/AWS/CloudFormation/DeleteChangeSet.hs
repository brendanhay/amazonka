{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DeleteChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified change set. Deleting change sets ensures that no one executes the wrong change set.
--
-- If the call successfully completes, AWS CloudFormation successfully deleted the change set.
-- If @IncludeNestedStacks@ specifies @True@ during the creation of the nested change set, then @DeleteChangeSet@ will delete all change sets that belong to the stacks hierarchy and will also delete all change sets for nested stacks with the status of @REVIEW_IN_PROGRESS@ .
module Network.AWS.CloudFormation.DeleteChangeSet
  ( -- * Creating a request
    DeleteChangeSet (..),
    mkDeleteChangeSet,

    -- ** Request lenses
    dcsStackName,
    dcsChangeSetName,

    -- * Destructuring the response
    DeleteChangeSetResponse (..),
    mkDeleteChangeSetResponse,

    -- ** Response lenses
    dcsrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DeleteChangeSet' action.
--
-- /See:/ 'mkDeleteChangeSet' smart constructor.
data DeleteChangeSet = DeleteChangeSet'
  { stackName ::
      Lude.Maybe Lude.Text,
    changeSetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChangeSet' with the minimum fields required to make a request.
--
-- * 'changeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want to delete.
-- * 'stackName' - If you specified the name of a change set to delete, specify the stack name or ID (ARN) that is associated with it.
mkDeleteChangeSet ::
  -- | 'changeSetName'
  Lude.Text ->
  DeleteChangeSet
mkDeleteChangeSet pChangeSetName_ =
  DeleteChangeSet'
    { stackName = Lude.Nothing,
      changeSetName = pChangeSetName_
    }

-- | If you specified the name of a change set to delete, specify the stack name or ID (ARN) that is associated with it.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStackName :: Lens.Lens' DeleteChangeSet (Lude.Maybe Lude.Text)
dcsStackName = Lens.lens (stackName :: DeleteChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DeleteChangeSet)
{-# DEPRECATED dcsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the change set that you want to delete.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsChangeSetName :: Lens.Lens' DeleteChangeSet Lude.Text
dcsChangeSetName = Lens.lens (changeSetName :: DeleteChangeSet -> Lude.Text) (\s a -> s {changeSetName = a} :: DeleteChangeSet)
{-# DEPRECATED dcsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

instance Lude.AWSRequest DeleteChangeSet where
  type Rs DeleteChangeSet = DeleteChangeSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DeleteChangeSetResult"
      ( \s h x ->
          DeleteChangeSetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteChangeSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteChangeSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteChangeSet where
  toQuery DeleteChangeSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteChangeSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackName" Lude.=: stackName,
        "ChangeSetName" Lude.=: changeSetName
      ]

-- | The output for the 'DeleteChangeSet' action.
--
-- /See:/ 'mkDeleteChangeSetResponse' smart constructor.
newtype DeleteChangeSetResponse = DeleteChangeSetResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChangeSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteChangeSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteChangeSetResponse
mkDeleteChangeSetResponse pResponseStatus_ =
  DeleteChangeSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsResponseStatus :: Lens.Lens' DeleteChangeSetResponse Lude.Int
dcsrsResponseStatus = Lens.lens (responseStatus :: DeleteChangeSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteChangeSetResponse)
{-# DEPRECATED dcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

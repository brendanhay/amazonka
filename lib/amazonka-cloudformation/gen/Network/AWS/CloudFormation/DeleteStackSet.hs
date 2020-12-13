{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DeleteStackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stack set. Before you can delete a stack set, all of its member stack instances must be deleted. For more information about how to do this, see 'DeleteStackInstances' .
module Network.AWS.CloudFormation.DeleteStackSet
  ( -- * Creating a request
    DeleteStackSet (..),
    mkDeleteStackSet,

    -- ** Request lenses
    dssfStackSetName,

    -- * Destructuring the response
    DeleteStackSetResponse (..),
    mkDeleteStackSetResponse,

    -- ** Response lenses
    dssrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStackSet' smart constructor.
newtype DeleteStackSet = DeleteStackSet'
  { -- | The name or unique ID of the stack set that you're deleting. You can obtain this value by running 'ListStackSets' .
    stackSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStackSet' with the minimum fields required to make a request.
--
-- * 'stackSetName' - The name or unique ID of the stack set that you're deleting. You can obtain this value by running 'ListStackSets' .
mkDeleteStackSet ::
  -- | 'stackSetName'
  Lude.Text ->
  DeleteStackSet
mkDeleteStackSet pStackSetName_ =
  DeleteStackSet' {stackSetName = pStackSetName_}

-- | The name or unique ID of the stack set that you're deleting. You can obtain this value by running 'ListStackSets' .
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssfStackSetName :: Lens.Lens' DeleteStackSet Lude.Text
dssfStackSetName = Lens.lens (stackSetName :: DeleteStackSet -> Lude.Text) (\s a -> s {stackSetName = a} :: DeleteStackSet)
{-# DEPRECATED dssfStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Lude.AWSRequest DeleteStackSet where
  type Rs DeleteStackSet = DeleteStackSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DeleteStackSetResult"
      ( \s h x ->
          DeleteStackSetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteStackSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteStackSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStackSet where
  toQuery DeleteStackSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteStackSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackSetName" Lude.=: stackSetName
      ]

-- | /See:/ 'mkDeleteStackSetResponse' smart constructor.
newtype DeleteStackSetResponse = DeleteStackSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStackSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteStackSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteStackSetResponse
mkDeleteStackSetResponse pResponseStatus_ =
  DeleteStackSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DeleteStackSetResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DeleteStackSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteStackSetResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

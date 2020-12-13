{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DeleteParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified parameter group. You cannot delete a parameter group if it is associated with any DAX clusters.
module Network.AWS.DAX.DeleteParameterGroup
  ( -- * Creating a request
    DeleteParameterGroup (..),
    mkDeleteParameterGroup,

    -- ** Request lenses
    dpgParameterGroupName,

    -- * Destructuring the response
    DeleteParameterGroupResponse (..),
    mkDeleteParameterGroupResponse,

    -- ** Response lenses
    dpgrsDeletionMessage,
    dpgrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteParameterGroup' smart constructor.
newtype DeleteParameterGroup = DeleteParameterGroup'
  { -- | The name of the parameter group to delete.
    parameterGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteParameterGroup' with the minimum fields required to make a request.
--
-- * 'parameterGroupName' - The name of the parameter group to delete.
mkDeleteParameterGroup ::
  -- | 'parameterGroupName'
  Lude.Text ->
  DeleteParameterGroup
mkDeleteParameterGroup pParameterGroupName_ =
  DeleteParameterGroup' {parameterGroupName = pParameterGroupName_}

-- | The name of the parameter group to delete.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgParameterGroupName :: Lens.Lens' DeleteParameterGroup Lude.Text
dpgParameterGroupName = Lens.lens (parameterGroupName :: DeleteParameterGroup -> Lude.Text) (\s a -> s {parameterGroupName = a} :: DeleteParameterGroup)
{-# DEPRECATED dpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.AWSRequest DeleteParameterGroup where
  type Rs DeleteParameterGroup = DeleteParameterGroupResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteParameterGroupResponse'
            Lude.<$> (x Lude..?> "DeletionMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteParameterGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.DeleteParameterGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteParameterGroup where
  toJSON DeleteParameterGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ParameterGroupName" Lude..= parameterGroupName)]
      )

instance Lude.ToPath DeleteParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteParameterGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteParameterGroupResponse' smart constructor.
data DeleteParameterGroupResponse = DeleteParameterGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting the parameter group).
    deletionMessage :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'deletionMessage' - A user-specified message for this action (i.e., a reason for deleting the parameter group).
-- * 'responseStatus' - The response status code.
mkDeleteParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteParameterGroupResponse
mkDeleteParameterGroupResponse pResponseStatus_ =
  DeleteParameterGroupResponse'
    { deletionMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-specified message for this action (i.e., a reason for deleting the parameter group).
--
-- /Note:/ Consider using 'deletionMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsDeletionMessage :: Lens.Lens' DeleteParameterGroupResponse (Lude.Maybe Lude.Text)
dpgrsDeletionMessage = Lens.lens (deletionMessage :: DeleteParameterGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {deletionMessage = a} :: DeleteParameterGroupResponse)
{-# DEPRECATED dpgrsDeletionMessage "Use generic-lens or generic-optics with 'deletionMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsResponseStatus :: Lens.Lens' DeleteParameterGroupResponse Lude.Int
dpgrsResponseStatus = Lens.lens (responseStatus :: DeleteParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteParameterGroupResponse)
{-# DEPRECATED dpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DeleteSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
module Network.AWS.DAX.DeleteSubnetGroup
  ( -- * Creating a request
    DeleteSubnetGroup (..),
    mkDeleteSubnetGroup,

    -- ** Request lenses
    dsgSubnetGroupName,

    -- * Destructuring the response
    DeleteSubnetGroupResponse (..),
    mkDeleteSubnetGroupResponse,

    -- ** Response lenses
    dsgrsDeletionMessage,
    dsgrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSubnetGroup' smart constructor.
newtype DeleteSubnetGroup = DeleteSubnetGroup'
  { -- | The name of the subnet group to delete.
    subnetGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubnetGroup' with the minimum fields required to make a request.
--
-- * 'subnetGroupName' - The name of the subnet group to delete.
mkDeleteSubnetGroup ::
  -- | 'subnetGroupName'
  Lude.Text ->
  DeleteSubnetGroup
mkDeleteSubnetGroup pSubnetGroupName_ =
  DeleteSubnetGroup' {subnetGroupName = pSubnetGroupName_}

-- | The name of the subnet group to delete.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSubnetGroupName :: Lens.Lens' DeleteSubnetGroup Lude.Text
dsgSubnetGroupName = Lens.lens (subnetGroupName :: DeleteSubnetGroup -> Lude.Text) (\s a -> s {subnetGroupName = a} :: DeleteSubnetGroup)
{-# DEPRECATED dsgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

instance Lude.AWSRequest DeleteSubnetGroup where
  type Rs DeleteSubnetGroup = DeleteSubnetGroupResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSubnetGroupResponse'
            Lude.<$> (x Lude..?> "DeletionMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSubnetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.DeleteSubnetGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSubnetGroup where
  toJSON DeleteSubnetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SubnetGroupName" Lude..= subnetGroupName)]
      )

instance Lude.ToPath DeleteSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSubnetGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSubnetGroupResponse' smart constructor.
data DeleteSubnetGroupResponse = DeleteSubnetGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting the subnet group).
    deletionMessage :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'deletionMessage' - A user-specified message for this action (i.e., a reason for deleting the subnet group).
-- * 'responseStatus' - The response status code.
mkDeleteSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSubnetGroupResponse
mkDeleteSubnetGroupResponse pResponseStatus_ =
  DeleteSubnetGroupResponse'
    { deletionMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-specified message for this action (i.e., a reason for deleting the subnet group).
--
-- /Note:/ Consider using 'deletionMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrsDeletionMessage :: Lens.Lens' DeleteSubnetGroupResponse (Lude.Maybe Lude.Text)
dsgrsDeletionMessage = Lens.lens (deletionMessage :: DeleteSubnetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {deletionMessage = a} :: DeleteSubnetGroupResponse)
{-# DEPRECATED dsgrsDeletionMessage "Use generic-lens or generic-optics with 'deletionMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrsResponseStatus :: Lens.Lens' DeleteSubnetGroupResponse Lude.Int
dsgrsResponseStatus = Lens.lens (responseStatus :: DeleteSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSubnetGroupResponse)
{-# DEPRECATED dsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

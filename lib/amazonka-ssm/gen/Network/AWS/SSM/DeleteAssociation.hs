{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified Systems Manager document from the specified instance.
--
-- When you disassociate a document from an instance, it does not change the configuration of the instance. To change the configuration state of an instance after you disassociate a document, you must create a new document with the desired configuration and associate it with the instance.
module Network.AWS.SSM.DeleteAssociation
  ( -- * Creating a request
    DeleteAssociation (..),
    mkDeleteAssociation,

    -- ** Request lenses
    dafAssociationId,
    dafInstanceId,
    dafName,

    -- * Destructuring the response
    DeleteAssociationResponse (..),
    mkDeleteAssociationResponse,

    -- ** Response lenses
    dagrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
  { -- | The association ID that you want to delete.
    associationId :: Lude.Maybe Lude.Text,
    -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The name of the Systems Manager document.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID that you want to delete.
-- * 'instanceId' - The ID of the instance.
-- * 'name' - The name of the Systems Manager document.
mkDeleteAssociation ::
  DeleteAssociation
mkDeleteAssociation =
  DeleteAssociation'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The association ID that you want to delete.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafAssociationId :: Lens.Lens' DeleteAssociation (Lude.Maybe Lude.Text)
dafAssociationId = Lens.lens (associationId :: DeleteAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DeleteAssociation)
{-# DEPRECATED dafAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafInstanceId :: Lens.Lens' DeleteAssociation (Lude.Maybe Lude.Text)
dafInstanceId = Lens.lens (instanceId :: DeleteAssociation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DeleteAssociation)
{-# DEPRECATED dafInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafName :: Lens.Lens' DeleteAssociation (Lude.Maybe Lude.Text)
dafName = Lens.lens (name :: DeleteAssociation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteAssociation)
{-# DEPRECATED dafName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteAssociation where
  type Rs DeleteAssociation = DeleteAssociationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAssociationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteAssociation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAssociation where
  toJSON DeleteAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AssociationId" Lude..=) Lude.<$> associationId,
            ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("Name" Lude..=) Lude.<$> name
          ]
      )

instance Lude.ToPath DeleteAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAssociationResponse' smart constructor.
newtype DeleteAssociationResponse = DeleteAssociationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAssociationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAssociationResponse
mkDeleteAssociationResponse pResponseStatus_ =
  DeleteAssociationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dagrsResponseStatus :: Lens.Lens' DeleteAssociationResponse Lude.Int
dagrsResponseStatus = Lens.lens (responseStatus :: DeleteAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAssociationResponse)
{-# DEPRECATED dagrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

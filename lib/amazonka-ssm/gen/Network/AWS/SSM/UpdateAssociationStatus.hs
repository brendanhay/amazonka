{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateAssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the Systems Manager document associated with the specified instance.
module Network.AWS.SSM.UpdateAssociationStatus
  ( -- * Creating a request
    UpdateAssociationStatus (..),
    mkUpdateAssociationStatus,

    -- ** Request lenses
    uasName,
    uasInstanceId,
    uasAssociationStatus,

    -- * Destructuring the response
    UpdateAssociationStatusResponse (..),
    mkUpdateAssociationStatusResponse,

    -- ** Response lenses
    uasrsAssociationDescription,
    uasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateAssociationStatus' smart constructor.
data UpdateAssociationStatus = UpdateAssociationStatus'
  { name ::
      Lude.Text,
    instanceId :: Lude.Text,
    associationStatus :: AssociationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAssociationStatus' with the minimum fields required to make a request.
--
-- * 'associationStatus' - The association status.
-- * 'instanceId' - The ID of the instance.
-- * 'name' - The name of the Systems Manager document.
mkUpdateAssociationStatus ::
  -- | 'name'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  -- | 'associationStatus'
  AssociationStatus ->
  UpdateAssociationStatus
mkUpdateAssociationStatus pName_ pInstanceId_ pAssociationStatus_ =
  UpdateAssociationStatus'
    { name = pName_,
      instanceId = pInstanceId_,
      associationStatus = pAssociationStatus_
    }

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasName :: Lens.Lens' UpdateAssociationStatus Lude.Text
uasName = Lens.lens (name :: UpdateAssociationStatus -> Lude.Text) (\s a -> s {name = a} :: UpdateAssociationStatus)
{-# DEPRECATED uasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasInstanceId :: Lens.Lens' UpdateAssociationStatus Lude.Text
uasInstanceId = Lens.lens (instanceId :: UpdateAssociationStatus -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateAssociationStatus)
{-# DEPRECATED uasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The association status.
--
-- /Note:/ Consider using 'associationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasAssociationStatus :: Lens.Lens' UpdateAssociationStatus AssociationStatus
uasAssociationStatus = Lens.lens (associationStatus :: UpdateAssociationStatus -> AssociationStatus) (\s a -> s {associationStatus = a} :: UpdateAssociationStatus)
{-# DEPRECATED uasAssociationStatus "Use generic-lens or generic-optics with 'associationStatus' instead." #-}

instance Lude.AWSRequest UpdateAssociationStatus where
  type Rs UpdateAssociationStatus = UpdateAssociationStatusResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAssociationStatusResponse'
            Lude.<$> (x Lude..?> "AssociationDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAssociationStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateAssociationStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAssociationStatus where
  toJSON UpdateAssociationStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("AssociationStatus" Lude..= associationStatus)
          ]
      )

instance Lude.ToPath UpdateAssociationStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAssociationStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAssociationStatusResponse' smart constructor.
data UpdateAssociationStatusResponse = UpdateAssociationStatusResponse'
  { associationDescription ::
      Lude.Maybe
        AssociationDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAssociationStatusResponse' with the minimum fields required to make a request.
--
-- * 'associationDescription' - Information about the association.
-- * 'responseStatus' - The response status code.
mkUpdateAssociationStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAssociationStatusResponse
mkUpdateAssociationStatusResponse pResponseStatus_ =
  UpdateAssociationStatusResponse'
    { associationDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsAssociationDescription :: Lens.Lens' UpdateAssociationStatusResponse (Lude.Maybe AssociationDescription)
uasrsAssociationDescription = Lens.lens (associationDescription :: UpdateAssociationStatusResponse -> Lude.Maybe AssociationDescription) (\s a -> s {associationDescription = a} :: UpdateAssociationStatusResponse)
{-# DEPRECATED uasrsAssociationDescription "Use generic-lens or generic-optics with 'associationDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsResponseStatus :: Lens.Lens' UpdateAssociationStatusResponse Lude.Int
uasrsResponseStatus = Lens.lens (responseStatus :: UpdateAssociationStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAssociationStatusResponse)
{-# DEPRECATED uasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

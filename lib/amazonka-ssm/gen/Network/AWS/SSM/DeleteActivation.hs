{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteActivation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activation. You are not required to delete an activation. If you delete an activation, you can no longer use it to register additional managed instances. Deleting an activation does not de-register managed instances. You must manually de-register managed instances.
module Network.AWS.SSM.DeleteActivation
  ( -- * Creating a request
    DeleteActivation (..),
    mkDeleteActivation,

    -- ** Request lenses
    daActivationId,

    -- * Destructuring the response
    DeleteActivationResponse (..),
    mkDeleteActivationResponse,

    -- ** Response lenses
    deleteactivationersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteActivation' smart constructor.
newtype DeleteActivation = DeleteActivation'
  { activationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteActivation' with the minimum fields required to make a request.
--
-- * 'activationId' - The ID of the activation that you want to delete.
mkDeleteActivation ::
  -- | 'activationId'
  Lude.Text ->
  DeleteActivation
mkDeleteActivation pActivationId_ =
  DeleteActivation' {activationId = pActivationId_}

-- | The ID of the activation that you want to delete.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActivationId :: Lens.Lens' DeleteActivation Lude.Text
daActivationId = Lens.lens (activationId :: DeleteActivation -> Lude.Text) (\s a -> s {activationId = a} :: DeleteActivation)
{-# DEPRECATED daActivationId "Use generic-lens or generic-optics with 'activationId' instead." #-}

instance Lude.AWSRequest DeleteActivation where
  type Rs DeleteActivation = DeleteActivationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteActivationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteActivation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteActivation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteActivation where
  toJSON DeleteActivation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ActivationId" Lude..= activationId)])

instance Lude.ToPath DeleteActivation where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteActivation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteActivationResponse' smart constructor.
newtype DeleteActivationResponse = DeleteActivationResponse'
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

-- | Creates a value of 'DeleteActivationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteActivationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteActivationResponse
mkDeleteActivationResponse pResponseStatus_ =
  DeleteActivationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deleteactivationersResponseStatus :: Lens.Lens' DeleteActivationResponse Lude.Int
deleteactivationersResponseStatus = Lens.lens (responseStatus :: DeleteActivationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteActivationResponse)
{-# DEPRECATED deleteactivationersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

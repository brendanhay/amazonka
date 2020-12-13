{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific SSH key pair.
--
-- The @delete key pair@ operation supports tag-based access control via resource tags applied to the resource identified by @key pair name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteKeyPair
  ( -- * Creating a request
    DeleteKeyPair (..),
    mkDeleteKeyPair,

    -- ** Request lenses
    dkpKeyPairName,

    -- * Destructuring the response
    DeleteKeyPairResponse (..),
    mkDeleteKeyPairResponse,

    -- ** Response lenses
    dkprsOperation,
    dkprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteKeyPair' smart constructor.
newtype DeleteKeyPair = DeleteKeyPair'
  { -- | The name of the key pair to delete.
    keyPairName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKeyPair' with the minimum fields required to make a request.
--
-- * 'keyPairName' - The name of the key pair to delete.
mkDeleteKeyPair ::
  -- | 'keyPairName'
  Lude.Text ->
  DeleteKeyPair
mkDeleteKeyPair pKeyPairName_ =
  DeleteKeyPair' {keyPairName = pKeyPairName_}

-- | The name of the key pair to delete.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyPairName :: Lens.Lens' DeleteKeyPair Lude.Text
dkpKeyPairName = Lens.lens (keyPairName :: DeleteKeyPair -> Lude.Text) (\s a -> s {keyPairName = a} :: DeleteKeyPair)
{-# DEPRECATED dkpKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

instance Lude.AWSRequest DeleteKeyPair where
  type Rs DeleteKeyPair = DeleteKeyPairResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteKeyPairResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteKeyPair where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteKeyPair" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteKeyPair where
  toJSON DeleteKeyPair' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("keyPairName" Lude..= keyPairName)])

instance Lude.ToPath DeleteKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteKeyPair where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKeyPairResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteKeyPairResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteKeyPairResponse
mkDeleteKeyPairResponse pResponseStatus_ =
  DeleteKeyPairResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprsOperation :: Lens.Lens' DeleteKeyPairResponse (Lude.Maybe Operation)
dkprsOperation = Lens.lens (operation :: DeleteKeyPairResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: DeleteKeyPairResponse)
{-# DEPRECATED dkprsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprsResponseStatus :: Lens.Lens' DeleteKeyPairResponse Lude.Int
dkprsResponseStatus = Lens.lens (responseStatus :: DeleteKeyPairResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteKeyPairResponse)
{-# DEPRECATED dkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

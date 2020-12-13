{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteKnownHostKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the known host key or certificate used by the Amazon Lightsail browser-based SSH or RDP clients to authenticate an instance. This operation enables the Lightsail browser-based SSH or RDP clients to connect to the instance after a host key mismatch.
--
-- /Important:/ Perform this operation only if you were expecting the host key or certificate mismatch or if you are familiar with the new host key or certificate on the instance. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-troubleshooting-browser-based-ssh-rdp-client-connection Troubleshooting connection issues when using the Amazon Lightsail browser-based SSH or RDP client> .
module Network.AWS.Lightsail.DeleteKnownHostKeys
  ( -- * Creating a request
    DeleteKnownHostKeys (..),
    mkDeleteKnownHostKeys,

    -- ** Request lenses
    dkhkInstanceName,

    -- * Destructuring the response
    DeleteKnownHostKeysResponse (..),
    mkDeleteKnownHostKeysResponse,

    -- ** Response lenses
    dkhkrsOperations,
    dkhkrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteKnownHostKeys' smart constructor.
newtype DeleteKnownHostKeys = DeleteKnownHostKeys'
  { -- | The name of the instance for which you want to reset the host key or certificate.
    instanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKnownHostKeys' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance for which you want to reset the host key or certificate.
mkDeleteKnownHostKeys ::
  -- | 'instanceName'
  Lude.Text ->
  DeleteKnownHostKeys
mkDeleteKnownHostKeys pInstanceName_ =
  DeleteKnownHostKeys' {instanceName = pInstanceName_}

-- | The name of the instance for which you want to reset the host key or certificate.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkhkInstanceName :: Lens.Lens' DeleteKnownHostKeys Lude.Text
dkhkInstanceName = Lens.lens (instanceName :: DeleteKnownHostKeys -> Lude.Text) (\s a -> s {instanceName = a} :: DeleteKnownHostKeys)
{-# DEPRECATED dkhkInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest DeleteKnownHostKeys where
  type Rs DeleteKnownHostKeys = DeleteKnownHostKeysResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteKnownHostKeysResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteKnownHostKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteKnownHostKeys" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteKnownHostKeys where
  toJSON DeleteKnownHostKeys' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath DeleteKnownHostKeys where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteKnownHostKeys where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteKnownHostKeysResponse' smart constructor.
data DeleteKnownHostKeysResponse = DeleteKnownHostKeysResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKnownHostKeysResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteKnownHostKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteKnownHostKeysResponse
mkDeleteKnownHostKeysResponse pResponseStatus_ =
  DeleteKnownHostKeysResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkhkrsOperations :: Lens.Lens' DeleteKnownHostKeysResponse (Lude.Maybe [Operation])
dkhkrsOperations = Lens.lens (operations :: DeleteKnownHostKeysResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteKnownHostKeysResponse)
{-# DEPRECATED dkhkrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkhkrsResponseStatus :: Lens.Lens' DeleteKnownHostKeysResponse Lude.Int
dkhkrsResponseStatus = Lens.lens (responseStatus :: DeleteKnownHostKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteKnownHostKeysResponse)
{-# DEPRECATED dkhkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

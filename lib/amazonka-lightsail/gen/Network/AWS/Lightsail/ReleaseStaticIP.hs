{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ReleaseStaticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific static IP from your account.
module Network.AWS.Lightsail.ReleaseStaticIP
  ( -- * Creating a request
    ReleaseStaticIP (..),
    mkReleaseStaticIP,

    -- ** Request lenses
    rsiStaticIPName,

    -- * Destructuring the response
    ReleaseStaticIPResponse (..),
    mkReleaseStaticIPResponse,

    -- ** Response lenses
    rsirsOperations,
    rsirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReleaseStaticIP' smart constructor.
newtype ReleaseStaticIP = ReleaseStaticIP'
  { -- | The name of the static IP to delete.
    staticIPName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReleaseStaticIP' with the minimum fields required to make a request.
--
-- * 'staticIPName' - The name of the static IP to delete.
mkReleaseStaticIP ::
  -- | 'staticIPName'
  Lude.Text ->
  ReleaseStaticIP
mkReleaseStaticIP pStaticIPName_ =
  ReleaseStaticIP' {staticIPName = pStaticIPName_}

-- | The name of the static IP to delete.
--
-- /Note:/ Consider using 'staticIPName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsiStaticIPName :: Lens.Lens' ReleaseStaticIP Lude.Text
rsiStaticIPName = Lens.lens (staticIPName :: ReleaseStaticIP -> Lude.Text) (\s a -> s {staticIPName = a} :: ReleaseStaticIP)
{-# DEPRECATED rsiStaticIPName "Use generic-lens or generic-optics with 'staticIPName' instead." #-}

instance Lude.AWSRequest ReleaseStaticIP where
  type Rs ReleaseStaticIP = ReleaseStaticIPResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReleaseStaticIPResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReleaseStaticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.ReleaseStaticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ReleaseStaticIP where
  toJSON ReleaseStaticIP' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("staticIpName" Lude..= staticIPName)])

instance Lude.ToPath ReleaseStaticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery ReleaseStaticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkReleaseStaticIPResponse' smart constructor.
data ReleaseStaticIPResponse = ReleaseStaticIPResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReleaseStaticIPResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkReleaseStaticIPResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReleaseStaticIPResponse
mkReleaseStaticIPResponse pResponseStatus_ =
  ReleaseStaticIPResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsirsOperations :: Lens.Lens' ReleaseStaticIPResponse (Lude.Maybe [Operation])
rsirsOperations = Lens.lens (operations :: ReleaseStaticIPResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: ReleaseStaticIPResponse)
{-# DEPRECATED rsirsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsirsResponseStatus :: Lens.Lens' ReleaseStaticIPResponse Lude.Int
rsirsResponseStatus = Lens.lens (responseStatus :: ReleaseStaticIPResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReleaseStaticIPResponse)
{-# DEPRECATED rsirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

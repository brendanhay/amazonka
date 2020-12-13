{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachStaticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a static IP from the Amazon Lightsail instance to which it is attached.
module Network.AWS.Lightsail.DetachStaticIP
  ( -- * Creating a request
    DetachStaticIP (..),
    mkDetachStaticIP,

    -- ** Request lenses
    dsiStaticIPName,

    -- * Destructuring the response
    DetachStaticIPResponse (..),
    mkDetachStaticIPResponse,

    -- ** Response lenses
    dsirsOperations,
    dsirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachStaticIP' smart constructor.
newtype DetachStaticIP = DetachStaticIP'
  { -- | The name of the static IP to detach from the instance.
    staticIPName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachStaticIP' with the minimum fields required to make a request.
--
-- * 'staticIPName' - The name of the static IP to detach from the instance.
mkDetachStaticIP ::
  -- | 'staticIPName'
  Lude.Text ->
  DetachStaticIP
mkDetachStaticIP pStaticIPName_ =
  DetachStaticIP' {staticIPName = pStaticIPName_}

-- | The name of the static IP to detach from the instance.
--
-- /Note:/ Consider using 'staticIPName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiStaticIPName :: Lens.Lens' DetachStaticIP Lude.Text
dsiStaticIPName = Lens.lens (staticIPName :: DetachStaticIP -> Lude.Text) (\s a -> s {staticIPName = a} :: DetachStaticIP)
{-# DEPRECATED dsiStaticIPName "Use generic-lens or generic-optics with 'staticIPName' instead." #-}

instance Lude.AWSRequest DetachStaticIP where
  type Rs DetachStaticIP = DetachStaticIPResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetachStaticIPResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachStaticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DetachStaticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetachStaticIP where
  toJSON DetachStaticIP' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("staticIpName" Lude..= staticIPName)])

instance Lude.ToPath DetachStaticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachStaticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachStaticIPResponse' smart constructor.
data DetachStaticIPResponse = DetachStaticIPResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachStaticIPResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDetachStaticIPResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachStaticIPResponse
mkDetachStaticIPResponse pResponseStatus_ =
  DetachStaticIPResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirsOperations :: Lens.Lens' DetachStaticIPResponse (Lude.Maybe [Operation])
dsirsOperations = Lens.lens (operations :: DetachStaticIPResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DetachStaticIPResponse)
{-# DEPRECATED dsirsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirsResponseStatus :: Lens.Lens' DetachStaticIPResponse Lude.Int
dsirsResponseStatus = Lens.lens (responseStatus :: DetachStaticIPResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachStaticIPResponse)
{-# DEPRECATED dsirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

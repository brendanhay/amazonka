{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachStaticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a static IP address to a specific Amazon Lightsail instance.
module Network.AWS.Lightsail.AttachStaticIP
  ( -- * Creating a request
    AttachStaticIP (..),
    mkAttachStaticIP,

    -- ** Request lenses
    asipStaticIPName,
    asipInstanceName,

    -- * Destructuring the response
    AttachStaticIPResponse (..),
    mkAttachStaticIPResponse,

    -- ** Response lenses
    asiprsOperations,
    asiprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachStaticIP' smart constructor.
data AttachStaticIP = AttachStaticIP'
  { staticIPName :: Lude.Text,
    instanceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachStaticIP' with the minimum fields required to make a request.
--
-- * 'instanceName' - The instance name to which you want to attach the static IP address.
-- * 'staticIPName' - The name of the static IP.
mkAttachStaticIP ::
  -- | 'staticIPName'
  Lude.Text ->
  -- | 'instanceName'
  Lude.Text ->
  AttachStaticIP
mkAttachStaticIP pStaticIPName_ pInstanceName_ =
  AttachStaticIP'
    { staticIPName = pStaticIPName_,
      instanceName = pInstanceName_
    }

-- | The name of the static IP.
--
-- /Note:/ Consider using 'staticIPName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asipStaticIPName :: Lens.Lens' AttachStaticIP Lude.Text
asipStaticIPName = Lens.lens (staticIPName :: AttachStaticIP -> Lude.Text) (\s a -> s {staticIPName = a} :: AttachStaticIP)
{-# DEPRECATED asipStaticIPName "Use generic-lens or generic-optics with 'staticIPName' instead." #-}

-- | The instance name to which you want to attach the static IP address.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asipInstanceName :: Lens.Lens' AttachStaticIP Lude.Text
asipInstanceName = Lens.lens (instanceName :: AttachStaticIP -> Lude.Text) (\s a -> s {instanceName = a} :: AttachStaticIP)
{-# DEPRECATED asipInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest AttachStaticIP where
  type Rs AttachStaticIP = AttachStaticIPResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachStaticIPResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachStaticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.AttachStaticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AttachStaticIP where
  toJSON AttachStaticIP' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("staticIpName" Lude..= staticIPName),
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath AttachStaticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachStaticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachStaticIPResponse' smart constructor.
data AttachStaticIPResponse = AttachStaticIPResponse'
  { operations ::
      Lude.Maybe [Operation],
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

-- | Creates a value of 'AttachStaticIPResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkAttachStaticIPResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachStaticIPResponse
mkAttachStaticIPResponse pResponseStatus_ =
  AttachStaticIPResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiprsOperations :: Lens.Lens' AttachStaticIPResponse (Lude.Maybe [Operation])
asiprsOperations = Lens.lens (operations :: AttachStaticIPResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: AttachStaticIPResponse)
{-# DEPRECATED asiprsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiprsResponseStatus :: Lens.Lens' AttachStaticIPResponse Lude.Int
asiprsResponseStatus = Lens.lens (responseStatus :: AttachStaticIPResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachStaticIPResponse)
{-# DEPRECATED asiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

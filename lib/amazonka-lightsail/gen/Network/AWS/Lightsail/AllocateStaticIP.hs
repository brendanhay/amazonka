{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AllocateStaticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a static IP address.
module Network.AWS.Lightsail.AllocateStaticIP
  ( -- * Creating a request
    AllocateStaticIP (..),
    mkAllocateStaticIP,

    -- ** Request lenses
    asiStaticIPName,

    -- * Destructuring the response
    AllocateStaticIPResponse (..),
    mkAllocateStaticIPResponse,

    -- ** Response lenses
    asirsOperations,
    asirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAllocateStaticIP' smart constructor.
newtype AllocateStaticIP = AllocateStaticIP'
  { -- | The name of the static IP address.
    staticIPName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateStaticIP' with the minimum fields required to make a request.
--
-- * 'staticIPName' - The name of the static IP address.
mkAllocateStaticIP ::
  -- | 'staticIPName'
  Lude.Text ->
  AllocateStaticIP
mkAllocateStaticIP pStaticIPName_ =
  AllocateStaticIP' {staticIPName = pStaticIPName_}

-- | The name of the static IP address.
--
-- /Note:/ Consider using 'staticIPName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiStaticIPName :: Lens.Lens' AllocateStaticIP Lude.Text
asiStaticIPName = Lens.lens (staticIPName :: AllocateStaticIP -> Lude.Text) (\s a -> s {staticIPName = a} :: AllocateStaticIP)
{-# DEPRECATED asiStaticIPName "Use generic-lens or generic-optics with 'staticIPName' instead." #-}

instance Lude.AWSRequest AllocateStaticIP where
  type Rs AllocateStaticIP = AllocateStaticIPResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          AllocateStaticIPResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AllocateStaticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.AllocateStaticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AllocateStaticIP where
  toJSON AllocateStaticIP' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("staticIpName" Lude..= staticIPName)])

instance Lude.ToPath AllocateStaticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery AllocateStaticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAllocateStaticIPResponse' smart constructor.
data AllocateStaticIPResponse = AllocateStaticIPResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateStaticIPResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkAllocateStaticIPResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AllocateStaticIPResponse
mkAllocateStaticIPResponse pResponseStatus_ =
  AllocateStaticIPResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asirsOperations :: Lens.Lens' AllocateStaticIPResponse (Lude.Maybe [Operation])
asirsOperations = Lens.lens (operations :: AllocateStaticIPResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: AllocateStaticIPResponse)
{-# DEPRECATED asirsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asirsResponseStatus :: Lens.Lens' AllocateStaticIPResponse Lude.Int
asirsResponseStatus = Lens.lens (responseStatus :: AllocateStaticIPResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AllocateStaticIPResponse)
{-# DEPRECATED asirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

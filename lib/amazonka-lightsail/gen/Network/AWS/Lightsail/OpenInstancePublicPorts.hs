{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.OpenInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Opens ports for a specific Amazon Lightsail instance, and specifies the IP addresses allowed to connect to the instance through the ports, and the protocol.
--
-- The @OpenInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.OpenInstancePublicPorts
  ( -- * Creating a request
    OpenInstancePublicPorts (..),
    mkOpenInstancePublicPorts,

    -- ** Request lenses
    oippPortInfo,
    oippInstanceName,

    -- * Destructuring the response
    OpenInstancePublicPortsResponse (..),
    mkOpenInstancePublicPortsResponse,

    -- ** Response lenses
    oipprsOperation,
    oipprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkOpenInstancePublicPorts' smart constructor.
data OpenInstancePublicPorts = OpenInstancePublicPorts'
  { portInfo ::
      PortInfo,
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

-- | Creates a value of 'OpenInstancePublicPorts' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance for which to open ports.
-- * 'portInfo' - An object to describe the ports to open for the specified instance.
mkOpenInstancePublicPorts ::
  -- | 'portInfo'
  PortInfo ->
  -- | 'instanceName'
  Lude.Text ->
  OpenInstancePublicPorts
mkOpenInstancePublicPorts pPortInfo_ pInstanceName_ =
  OpenInstancePublicPorts'
    { portInfo = pPortInfo_,
      instanceName = pInstanceName_
    }

-- | An object to describe the ports to open for the specified instance.
--
-- /Note:/ Consider using 'portInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oippPortInfo :: Lens.Lens' OpenInstancePublicPorts PortInfo
oippPortInfo = Lens.lens (portInfo :: OpenInstancePublicPorts -> PortInfo) (\s a -> s {portInfo = a} :: OpenInstancePublicPorts)
{-# DEPRECATED oippPortInfo "Use generic-lens or generic-optics with 'portInfo' instead." #-}

-- | The name of the instance for which to open ports.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oippInstanceName :: Lens.Lens' OpenInstancePublicPorts Lude.Text
oippInstanceName = Lens.lens (instanceName :: OpenInstancePublicPorts -> Lude.Text) (\s a -> s {instanceName = a} :: OpenInstancePublicPorts)
{-# DEPRECATED oippInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest OpenInstancePublicPorts where
  type Rs OpenInstancePublicPorts = OpenInstancePublicPortsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          OpenInstancePublicPortsResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders OpenInstancePublicPorts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.OpenInstancePublicPorts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON OpenInstancePublicPorts where
  toJSON OpenInstancePublicPorts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("portInfo" Lude..= portInfo),
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath OpenInstancePublicPorts where
  toPath = Lude.const "/"

instance Lude.ToQuery OpenInstancePublicPorts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkOpenInstancePublicPortsResponse' smart constructor.
data OpenInstancePublicPortsResponse = OpenInstancePublicPortsResponse'
  { operation ::
      Lude.Maybe Operation,
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

-- | Creates a value of 'OpenInstancePublicPortsResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkOpenInstancePublicPortsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  OpenInstancePublicPortsResponse
mkOpenInstancePublicPortsResponse pResponseStatus_ =
  OpenInstancePublicPortsResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipprsOperation :: Lens.Lens' OpenInstancePublicPortsResponse (Lude.Maybe Operation)
oipprsOperation = Lens.lens (operation :: OpenInstancePublicPortsResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: OpenInstancePublicPortsResponse)
{-# DEPRECATED oipprsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipprsResponseStatus :: Lens.Lens' OpenInstancePublicPortsResponse Lude.Int
oipprsResponseStatus = Lens.lens (responseStatus :: OpenInstancePublicPortsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: OpenInstancePublicPortsResponse)
{-# DEPRECATED oipprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.PutInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Opens ports for a specific Amazon Lightsail instance, and specifies the IP addresses allowed to connect to the instance through the ports, and the protocol. This action also closes all currently open ports that are not included in the request. Include all of the ports and the protocols you want to open in your @PutInstancePublicPorts@ request. Or use the @OpenInstancePublicPorts@ action to open ports without closing currently open ports.
--
-- The @PutInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.PutInstancePublicPorts
  ( -- * Creating a request
    PutInstancePublicPorts (..),
    mkPutInstancePublicPorts,

    -- ** Request lenses
    pippPortInfos,
    pippInstanceName,

    -- * Destructuring the response
    PutInstancePublicPortsResponse (..),
    mkPutInstancePublicPortsResponse,

    -- ** Response lenses
    pipprsOperation,
    pipprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutInstancePublicPorts' smart constructor.
data PutInstancePublicPorts = PutInstancePublicPorts'
  { -- | An array of objects to describe the ports to open for the specified instance.
    portInfos :: [PortInfo],
    -- | The name of the instance for which to open ports.
    instanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInstancePublicPorts' with the minimum fields required to make a request.
--
-- * 'portInfos' - An array of objects to describe the ports to open for the specified instance.
-- * 'instanceName' - The name of the instance for which to open ports.
mkPutInstancePublicPorts ::
  -- | 'instanceName'
  Lude.Text ->
  PutInstancePublicPorts
mkPutInstancePublicPorts pInstanceName_ =
  PutInstancePublicPorts'
    { portInfos = Lude.mempty,
      instanceName = pInstanceName_
    }

-- | An array of objects to describe the ports to open for the specified instance.
--
-- /Note:/ Consider using 'portInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pippPortInfos :: Lens.Lens' PutInstancePublicPorts [PortInfo]
pippPortInfos = Lens.lens (portInfos :: PutInstancePublicPorts -> [PortInfo]) (\s a -> s {portInfos = a} :: PutInstancePublicPorts)
{-# DEPRECATED pippPortInfos "Use generic-lens or generic-optics with 'portInfos' instead." #-}

-- | The name of the instance for which to open ports.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pippInstanceName :: Lens.Lens' PutInstancePublicPorts Lude.Text
pippInstanceName = Lens.lens (instanceName :: PutInstancePublicPorts -> Lude.Text) (\s a -> s {instanceName = a} :: PutInstancePublicPorts)
{-# DEPRECATED pippInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest PutInstancePublicPorts where
  type Rs PutInstancePublicPorts = PutInstancePublicPortsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutInstancePublicPortsResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutInstancePublicPorts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.PutInstancePublicPorts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutInstancePublicPorts where
  toJSON PutInstancePublicPorts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("portInfos" Lude..= portInfos),
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath PutInstancePublicPorts where
  toPath = Lude.const "/"

instance Lude.ToQuery PutInstancePublicPorts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutInstancePublicPortsResponse' smart constructor.
data PutInstancePublicPortsResponse = PutInstancePublicPortsResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInstancePublicPortsResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkPutInstancePublicPortsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutInstancePublicPortsResponse
mkPutInstancePublicPortsResponse pResponseStatus_ =
  PutInstancePublicPortsResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipprsOperation :: Lens.Lens' PutInstancePublicPortsResponse (Lude.Maybe Operation)
pipprsOperation = Lens.lens (operation :: PutInstancePublicPortsResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: PutInstancePublicPortsResponse)
{-# DEPRECATED pipprsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipprsResponseStatus :: Lens.Lens' PutInstancePublicPortsResponse Lude.Int
pipprsResponseStatus = Lens.lens (responseStatus :: PutInstancePublicPortsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutInstancePublicPortsResponse)
{-# DEPRECATED pipprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

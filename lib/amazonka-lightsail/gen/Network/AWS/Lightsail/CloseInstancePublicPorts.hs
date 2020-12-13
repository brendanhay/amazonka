{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CloseInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes ports for a specific Amazon Lightsail instance.
--
-- The @CloseInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CloseInstancePublicPorts
  ( -- * Creating a request
    CloseInstancePublicPorts (..),
    mkCloseInstancePublicPorts,

    -- ** Request lenses
    cippPortInfo,
    cippInstanceName,

    -- * Destructuring the response
    CloseInstancePublicPortsResponse (..),
    mkCloseInstancePublicPortsResponse,

    -- ** Response lenses
    cipprsOperation,
    cipprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCloseInstancePublicPorts' smart constructor.
data CloseInstancePublicPorts = CloseInstancePublicPorts'
  { -- | An object to describe the ports to close for the specified instance.
    portInfo :: PortInfo,
    -- | The name of the instance for which to close ports.
    instanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloseInstancePublicPorts' with the minimum fields required to make a request.
--
-- * 'portInfo' - An object to describe the ports to close for the specified instance.
-- * 'instanceName' - The name of the instance for which to close ports.
mkCloseInstancePublicPorts ::
  -- | 'portInfo'
  PortInfo ->
  -- | 'instanceName'
  Lude.Text ->
  CloseInstancePublicPorts
mkCloseInstancePublicPorts pPortInfo_ pInstanceName_ =
  CloseInstancePublicPorts'
    { portInfo = pPortInfo_,
      instanceName = pInstanceName_
    }

-- | An object to describe the ports to close for the specified instance.
--
-- /Note:/ Consider using 'portInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cippPortInfo :: Lens.Lens' CloseInstancePublicPorts PortInfo
cippPortInfo = Lens.lens (portInfo :: CloseInstancePublicPorts -> PortInfo) (\s a -> s {portInfo = a} :: CloseInstancePublicPorts)
{-# DEPRECATED cippPortInfo "Use generic-lens or generic-optics with 'portInfo' instead." #-}

-- | The name of the instance for which to close ports.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cippInstanceName :: Lens.Lens' CloseInstancePublicPorts Lude.Text
cippInstanceName = Lens.lens (instanceName :: CloseInstancePublicPorts -> Lude.Text) (\s a -> s {instanceName = a} :: CloseInstancePublicPorts)
{-# DEPRECATED cippInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest CloseInstancePublicPorts where
  type Rs CloseInstancePublicPorts = CloseInstancePublicPortsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CloseInstancePublicPortsResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CloseInstancePublicPorts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CloseInstancePublicPorts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CloseInstancePublicPorts where
  toJSON CloseInstancePublicPorts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("portInfo" Lude..= portInfo),
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath CloseInstancePublicPorts where
  toPath = Lude.const "/"

instance Lude.ToQuery CloseInstancePublicPorts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCloseInstancePublicPortsResponse' smart constructor.
data CloseInstancePublicPortsResponse = CloseInstancePublicPortsResponse'
  { -- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloseInstancePublicPortsResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCloseInstancePublicPortsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CloseInstancePublicPortsResponse
mkCloseInstancePublicPortsResponse pResponseStatus_ =
  CloseInstancePublicPortsResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipprsOperation :: Lens.Lens' CloseInstancePublicPortsResponse (Lude.Maybe Operation)
cipprsOperation = Lens.lens (operation :: CloseInstancePublicPortsResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: CloseInstancePublicPortsResponse)
{-# DEPRECATED cipprsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipprsResponseStatus :: Lens.Lens' CloseInstancePublicPortsResponse Lude.Int
cipprsResponseStatus = Lens.lens (responseStatus :: CloseInstancePublicPortsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CloseInstancePublicPortsResponse)
{-# DEPRECATED cipprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint configuration. The @DeleteEndpointConfig@ API deletes only the specified configuration. It does not delete endpoints created using the configuration.
--
-- You must not delete an @EndpointConfig@ in use by an endpoint that is live or while the @UpdateEndpoint@ or @CreateEndpoint@ operations are being performed on the endpoint. If you delete the @EndpointConfig@ of an endpoint that is active or being created or updated you may lose visibility into the instance type the endpoint is using. The endpoint must be deleted in order to stop incurring charges.
module Network.AWS.SageMaker.DeleteEndpointConfig
  ( -- * Creating a request
    DeleteEndpointConfig (..),
    mkDeleteEndpointConfig,

    -- ** Request lenses
    decEndpointConfigName,

    -- * Destructuring the response
    DeleteEndpointConfigResponse (..),
    mkDeleteEndpointConfigResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteEndpointConfig' smart constructor.
newtype DeleteEndpointConfig = DeleteEndpointConfig'
  { -- | The name of the endpoint configuration that you want to delete.
    endpointConfigName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEndpointConfig' with the minimum fields required to make a request.
--
-- * 'endpointConfigName' - The name of the endpoint configuration that you want to delete.
mkDeleteEndpointConfig ::
  -- | 'endpointConfigName'
  Lude.Text ->
  DeleteEndpointConfig
mkDeleteEndpointConfig pEndpointConfigName_ =
  DeleteEndpointConfig' {endpointConfigName = pEndpointConfigName_}

-- | The name of the endpoint configuration that you want to delete.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEndpointConfigName :: Lens.Lens' DeleteEndpointConfig Lude.Text
decEndpointConfigName = Lens.lens (endpointConfigName :: DeleteEndpointConfig -> Lude.Text) (\s a -> s {endpointConfigName = a} :: DeleteEndpointConfig)
{-# DEPRECATED decEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

instance Lude.AWSRequest DeleteEndpointConfig where
  type Rs DeleteEndpointConfig = DeleteEndpointConfigResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteEndpointConfigResponse'

instance Lude.ToHeaders DeleteEndpointConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteEndpointConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEndpointConfig where
  toJSON DeleteEndpointConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EndpointConfigName" Lude..= endpointConfigName)]
      )

instance Lude.ToPath DeleteEndpointConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEndpointConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEndpointConfigResponse' smart constructor.
data DeleteEndpointConfigResponse = DeleteEndpointConfigResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEndpointConfigResponse' with the minimum fields required to make a request.
mkDeleteEndpointConfigResponse ::
  DeleteEndpointConfigResponse
mkDeleteEndpointConfigResponse = DeleteEndpointConfigResponse'

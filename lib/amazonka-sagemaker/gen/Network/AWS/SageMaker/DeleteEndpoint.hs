{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint. Amazon SageMaker frees up all of the resources that were deployed when the endpoint was created.
--
-- Amazon SageMaker retires any custom KMS key grants associated with the endpoint, meaning you don't need to use the <http://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant> API call.
module Network.AWS.SageMaker.DeleteEndpoint
  ( -- * Creating a request
    DeleteEndpoint (..),
    mkDeleteEndpoint,

    -- ** Request lenses
    deEndpointName,

    -- * Destructuring the response
    DeleteEndpointResponse (..),
    mkDeleteEndpointResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint'
  { -- | The name of the endpoint that you want to delete.
    endpointName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name of the endpoint that you want to delete.
mkDeleteEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  DeleteEndpoint
mkDeleteEndpoint pEndpointName_ =
  DeleteEndpoint' {endpointName = pEndpointName_}

-- | The name of the endpoint that you want to delete.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointName :: Lens.Lens' DeleteEndpoint Lude.Text
deEndpointName = Lens.lens (endpointName :: DeleteEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: DeleteEndpoint)
{-# DEPRECATED deEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

instance Lude.AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteEndpointResponse'

instance Lude.ToHeaders DeleteEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEndpoint where
  toJSON DeleteEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EndpointName" Lude..= endpointName)])

instance Lude.ToPath DeleteEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
mkDeleteEndpointResponse ::
  DeleteEndpointResponse
mkDeleteEndpointResponse = DeleteEndpointResponse'

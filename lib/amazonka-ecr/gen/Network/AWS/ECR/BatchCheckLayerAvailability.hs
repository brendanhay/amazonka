{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.BatchCheckLayerAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the availability of one or more image layers in a repository.
--
-- When an image is pushed to a repository, each image layer is checked to verify if it has been uploaded before. If it has been uploaded, then the image layer is skipped.
module Network.AWS.ECR.BatchCheckLayerAvailability
  ( -- * Creating a request
    BatchCheckLayerAvailability (..),
    mkBatchCheckLayerAvailability,

    -- ** Request lenses
    bclaRegistryId,
    bclaRepositoryName,
    bclaLayerDigests,

    -- * Destructuring the response
    BatchCheckLayerAvailabilityResponse (..),
    mkBatchCheckLayerAvailabilityResponse,

    -- ** Response lenses
    bclarsFailures,
    bclarsLayers,
    bclarsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchCheckLayerAvailability' smart constructor.
data BatchCheckLayerAvailability = BatchCheckLayerAvailability'
  { registryId ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    layerDigests ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchCheckLayerAvailability' with the minimum fields required to make a request.
--
-- * 'layerDigests' - The digests of the image layers to check.
-- * 'registryId' - The AWS account ID associated with the registry that contains the image layers to check. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository that is associated with the image layers to check.
mkBatchCheckLayerAvailability ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'layerDigests'
  Lude.NonEmpty Lude.Text ->
  BatchCheckLayerAvailability
mkBatchCheckLayerAvailability pRepositoryName_ pLayerDigests_ =
  BatchCheckLayerAvailability'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_,
      layerDigests = pLayerDigests_
    }

-- | The AWS account ID associated with the registry that contains the image layers to check. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclaRegistryId :: Lens.Lens' BatchCheckLayerAvailability (Lude.Maybe Lude.Text)
bclaRegistryId = Lens.lens (registryId :: BatchCheckLayerAvailability -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: BatchCheckLayerAvailability)
{-# DEPRECATED bclaRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository that is associated with the image layers to check.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclaRepositoryName :: Lens.Lens' BatchCheckLayerAvailability Lude.Text
bclaRepositoryName = Lens.lens (repositoryName :: BatchCheckLayerAvailability -> Lude.Text) (\s a -> s {repositoryName = a} :: BatchCheckLayerAvailability)
{-# DEPRECATED bclaRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The digests of the image layers to check.
--
-- /Note:/ Consider using 'layerDigests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclaLayerDigests :: Lens.Lens' BatchCheckLayerAvailability (Lude.NonEmpty Lude.Text)
bclaLayerDigests = Lens.lens (layerDigests :: BatchCheckLayerAvailability -> Lude.NonEmpty Lude.Text) (\s a -> s {layerDigests = a} :: BatchCheckLayerAvailability)
{-# DEPRECATED bclaLayerDigests "Use generic-lens or generic-optics with 'layerDigests' instead." #-}

instance Lude.AWSRequest BatchCheckLayerAvailability where
  type
    Rs BatchCheckLayerAvailability =
      BatchCheckLayerAvailabilityResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchCheckLayerAvailabilityResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "layers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchCheckLayerAvailability where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchCheckLayerAvailability" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchCheckLayerAvailability where
  toJSON BatchCheckLayerAvailability' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("layerDigests" Lude..= layerDigests)
          ]
      )

instance Lude.ToPath BatchCheckLayerAvailability where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchCheckLayerAvailability where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchCheckLayerAvailabilityResponse' smart constructor.
data BatchCheckLayerAvailabilityResponse = BatchCheckLayerAvailabilityResponse'
  { failures ::
      Lude.Maybe
        [LayerFailure],
    layers ::
      Lude.Maybe [Layer],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchCheckLayerAvailabilityResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'layers' - A list of image layer objects corresponding to the image layer references in the request.
-- * 'responseStatus' - The response status code.
mkBatchCheckLayerAvailabilityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchCheckLayerAvailabilityResponse
mkBatchCheckLayerAvailabilityResponse pResponseStatus_ =
  BatchCheckLayerAvailabilityResponse'
    { failures = Lude.Nothing,
      layers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclarsFailures :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Lude.Maybe [LayerFailure])
bclarsFailures = Lens.lens (failures :: BatchCheckLayerAvailabilityResponse -> Lude.Maybe [LayerFailure]) (\s a -> s {failures = a} :: BatchCheckLayerAvailabilityResponse)
{-# DEPRECATED bclarsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | A list of image layer objects corresponding to the image layer references in the request.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclarsLayers :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Lude.Maybe [Layer])
bclarsLayers = Lens.lens (layers :: BatchCheckLayerAvailabilityResponse -> Lude.Maybe [Layer]) (\s a -> s {layers = a} :: BatchCheckLayerAvailabilityResponse)
{-# DEPRECATED bclarsLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclarsResponseStatus :: Lens.Lens' BatchCheckLayerAvailabilityResponse Lude.Int
bclarsResponseStatus = Lens.lens (responseStatus :: BatchCheckLayerAvailabilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchCheckLayerAvailabilityResponse)
{-# DEPRECATED bclarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

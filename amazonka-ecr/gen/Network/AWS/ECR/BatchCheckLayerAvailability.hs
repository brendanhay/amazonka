{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.BatchCheckLayerAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the availability of one or more image layers in a repository.
--
-- When an image is pushed to a repository, each image layer is checked to
-- verify if it has been uploaded before. If it has been uploaded, then the
-- image layer is skipped.
--
-- This operation is used by the Amazon ECR proxy and is not generally used
-- by customers for pulling and pushing images. In most cases, you should
-- use the @docker@ CLI to pull, tag, and push images.
module Network.AWS.ECR.BatchCheckLayerAvailability
  ( -- * Creating a Request
    BatchCheckLayerAvailability (..),
    newBatchCheckLayerAvailability,

    -- * Request Lenses
    batchCheckLayerAvailability_registryId,
    batchCheckLayerAvailability_repositoryName,
    batchCheckLayerAvailability_layerDigests,

    -- * Destructuring the Response
    BatchCheckLayerAvailabilityResponse (..),
    newBatchCheckLayerAvailabilityResponse,

    -- * Response Lenses
    batchCheckLayerAvailabilityResponse_failures,
    batchCheckLayerAvailabilityResponse_layers,
    batchCheckLayerAvailabilityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchCheckLayerAvailability' smart constructor.
data BatchCheckLayerAvailability = BatchCheckLayerAvailability'
  { -- | The AWS account ID associated with the registry that contains the image
    -- layers to check. If you do not specify a registry, the default registry
    -- is assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository that is associated with the image layers to
    -- check.
    repositoryName :: Core.Text,
    -- | The digests of the image layers to check.
    layerDigests :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchCheckLayerAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'batchCheckLayerAvailability_registryId' - The AWS account ID associated with the registry that contains the image
-- layers to check. If you do not specify a registry, the default registry
-- is assumed.
--
-- 'repositoryName', 'batchCheckLayerAvailability_repositoryName' - The name of the repository that is associated with the image layers to
-- check.
--
-- 'layerDigests', 'batchCheckLayerAvailability_layerDigests' - The digests of the image layers to check.
newBatchCheckLayerAvailability ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'layerDigests'
  Core.NonEmpty Core.Text ->
  BatchCheckLayerAvailability
newBatchCheckLayerAvailability
  pRepositoryName_
  pLayerDigests_ =
    BatchCheckLayerAvailability'
      { registryId =
          Core.Nothing,
        repositoryName = pRepositoryName_,
        layerDigests =
          Lens._Coerce Lens.# pLayerDigests_
      }

-- | The AWS account ID associated with the registry that contains the image
-- layers to check. If you do not specify a registry, the default registry
-- is assumed.
batchCheckLayerAvailability_registryId :: Lens.Lens' BatchCheckLayerAvailability (Core.Maybe Core.Text)
batchCheckLayerAvailability_registryId = Lens.lens (\BatchCheckLayerAvailability' {registryId} -> registryId) (\s@BatchCheckLayerAvailability' {} a -> s {registryId = a} :: BatchCheckLayerAvailability)

-- | The name of the repository that is associated with the image layers to
-- check.
batchCheckLayerAvailability_repositoryName :: Lens.Lens' BatchCheckLayerAvailability Core.Text
batchCheckLayerAvailability_repositoryName = Lens.lens (\BatchCheckLayerAvailability' {repositoryName} -> repositoryName) (\s@BatchCheckLayerAvailability' {} a -> s {repositoryName = a} :: BatchCheckLayerAvailability)

-- | The digests of the image layers to check.
batchCheckLayerAvailability_layerDigests :: Lens.Lens' BatchCheckLayerAvailability (Core.NonEmpty Core.Text)
batchCheckLayerAvailability_layerDigests = Lens.lens (\BatchCheckLayerAvailability' {layerDigests} -> layerDigests) (\s@BatchCheckLayerAvailability' {} a -> s {layerDigests = a} :: BatchCheckLayerAvailability) Core.. Lens._Coerce

instance Core.AWSRequest BatchCheckLayerAvailability where
  type
    AWSResponse BatchCheckLayerAvailability =
      BatchCheckLayerAvailabilityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCheckLayerAvailabilityResponse'
            Core.<$> (x Core..?> "failures" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "layers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchCheckLayerAvailability

instance Core.NFData BatchCheckLayerAvailability

instance Core.ToHeaders BatchCheckLayerAvailability where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchCheckLayerAvailability" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchCheckLayerAvailability where
  toJSON BatchCheckLayerAvailability' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("layerDigests" Core..= layerDigests)
          ]
      )

instance Core.ToPath BatchCheckLayerAvailability where
  toPath = Core.const "/"

instance Core.ToQuery BatchCheckLayerAvailability where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchCheckLayerAvailabilityResponse' smart constructor.
data BatchCheckLayerAvailabilityResponse = BatchCheckLayerAvailabilityResponse'
  { -- | Any failures associated with the call.
    failures :: Core.Maybe [LayerFailure],
    -- | A list of image layer objects corresponding to the image layer
    -- references in the request.
    layers :: Core.Maybe [Layer],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchCheckLayerAvailabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'batchCheckLayerAvailabilityResponse_failures' - Any failures associated with the call.
--
-- 'layers', 'batchCheckLayerAvailabilityResponse_layers' - A list of image layer objects corresponding to the image layer
-- references in the request.
--
-- 'httpStatus', 'batchCheckLayerAvailabilityResponse_httpStatus' - The response's http status code.
newBatchCheckLayerAvailabilityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchCheckLayerAvailabilityResponse
newBatchCheckLayerAvailabilityResponse pHttpStatus_ =
  BatchCheckLayerAvailabilityResponse'
    { failures =
        Core.Nothing,
      layers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
batchCheckLayerAvailabilityResponse_failures :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Core.Maybe [LayerFailure])
batchCheckLayerAvailabilityResponse_failures = Lens.lens (\BatchCheckLayerAvailabilityResponse' {failures} -> failures) (\s@BatchCheckLayerAvailabilityResponse' {} a -> s {failures = a} :: BatchCheckLayerAvailabilityResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of image layer objects corresponding to the image layer
-- references in the request.
batchCheckLayerAvailabilityResponse_layers :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Core.Maybe [Layer])
batchCheckLayerAvailabilityResponse_layers = Lens.lens (\BatchCheckLayerAvailabilityResponse' {layers} -> layers) (\s@BatchCheckLayerAvailabilityResponse' {} a -> s {layers = a} :: BatchCheckLayerAvailabilityResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchCheckLayerAvailabilityResponse_httpStatus :: Lens.Lens' BatchCheckLayerAvailabilityResponse Core.Int
batchCheckLayerAvailabilityResponse_httpStatus = Lens.lens (\BatchCheckLayerAvailabilityResponse' {httpStatus} -> httpStatus) (\s@BatchCheckLayerAvailabilityResponse' {} a -> s {httpStatus = a} :: BatchCheckLayerAvailabilityResponse)

instance
  Core.NFData
    BatchCheckLayerAvailabilityResponse

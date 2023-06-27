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
-- Module      : Amazonka.ECR.BatchCheckLayerAvailability
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ECR.BatchCheckLayerAvailability
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCheckLayerAvailability' smart constructor.
data BatchCheckLayerAvailability = BatchCheckLayerAvailability'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the image layers to check. If you do not specify a registry,
    -- the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that is associated with the image layers to
    -- check.
    repositoryName :: Prelude.Text,
    -- | The digests of the image layers to check.
    layerDigests :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCheckLayerAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'batchCheckLayerAvailability_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the image layers to check. If you do not specify a registry,
-- the default registry is assumed.
--
-- 'repositoryName', 'batchCheckLayerAvailability_repositoryName' - The name of the repository that is associated with the image layers to
-- check.
--
-- 'layerDigests', 'batchCheckLayerAvailability_layerDigests' - The digests of the image layers to check.
newBatchCheckLayerAvailability ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'layerDigests'
  Prelude.NonEmpty Prelude.Text ->
  BatchCheckLayerAvailability
newBatchCheckLayerAvailability
  pRepositoryName_
  pLayerDigests_ =
    BatchCheckLayerAvailability'
      { registryId =
          Prelude.Nothing,
        repositoryName = pRepositoryName_,
        layerDigests =
          Lens.coerced Lens.# pLayerDigests_
      }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the image layers to check. If you do not specify a registry,
-- the default registry is assumed.
batchCheckLayerAvailability_registryId :: Lens.Lens' BatchCheckLayerAvailability (Prelude.Maybe Prelude.Text)
batchCheckLayerAvailability_registryId = Lens.lens (\BatchCheckLayerAvailability' {registryId} -> registryId) (\s@BatchCheckLayerAvailability' {} a -> s {registryId = a} :: BatchCheckLayerAvailability)

-- | The name of the repository that is associated with the image layers to
-- check.
batchCheckLayerAvailability_repositoryName :: Lens.Lens' BatchCheckLayerAvailability Prelude.Text
batchCheckLayerAvailability_repositoryName = Lens.lens (\BatchCheckLayerAvailability' {repositoryName} -> repositoryName) (\s@BatchCheckLayerAvailability' {} a -> s {repositoryName = a} :: BatchCheckLayerAvailability)

-- | The digests of the image layers to check.
batchCheckLayerAvailability_layerDigests :: Lens.Lens' BatchCheckLayerAvailability (Prelude.NonEmpty Prelude.Text)
batchCheckLayerAvailability_layerDigests = Lens.lens (\BatchCheckLayerAvailability' {layerDigests} -> layerDigests) (\s@BatchCheckLayerAvailability' {} a -> s {layerDigests = a} :: BatchCheckLayerAvailability) Prelude.. Lens.coerced

instance Core.AWSRequest BatchCheckLayerAvailability where
  type
    AWSResponse BatchCheckLayerAvailability =
      BatchCheckLayerAvailabilityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCheckLayerAvailabilityResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "layers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchCheckLayerAvailability where
  hashWithSalt _salt BatchCheckLayerAvailability' {..} =
    _salt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` layerDigests

instance Prelude.NFData BatchCheckLayerAvailability where
  rnf BatchCheckLayerAvailability' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf layerDigests

instance Data.ToHeaders BatchCheckLayerAvailability where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchCheckLayerAvailability" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchCheckLayerAvailability where
  toJSON BatchCheckLayerAvailability' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("layerDigests" Data..= layerDigests)
          ]
      )

instance Data.ToPath BatchCheckLayerAvailability where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchCheckLayerAvailability where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCheckLayerAvailabilityResponse' smart constructor.
data BatchCheckLayerAvailabilityResponse = BatchCheckLayerAvailabilityResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [LayerFailure],
    -- | A list of image layer objects corresponding to the image layer
    -- references in the request.
    layers :: Prelude.Maybe [Layer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchCheckLayerAvailabilityResponse
newBatchCheckLayerAvailabilityResponse pHttpStatus_ =
  BatchCheckLayerAvailabilityResponse'
    { failures =
        Prelude.Nothing,
      layers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
batchCheckLayerAvailabilityResponse_failures :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Prelude.Maybe [LayerFailure])
batchCheckLayerAvailabilityResponse_failures = Lens.lens (\BatchCheckLayerAvailabilityResponse' {failures} -> failures) (\s@BatchCheckLayerAvailabilityResponse' {} a -> s {failures = a} :: BatchCheckLayerAvailabilityResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of image layer objects corresponding to the image layer
-- references in the request.
batchCheckLayerAvailabilityResponse_layers :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Prelude.Maybe [Layer])
batchCheckLayerAvailabilityResponse_layers = Lens.lens (\BatchCheckLayerAvailabilityResponse' {layers} -> layers) (\s@BatchCheckLayerAvailabilityResponse' {} a -> s {layers = a} :: BatchCheckLayerAvailabilityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCheckLayerAvailabilityResponse_httpStatus :: Lens.Lens' BatchCheckLayerAvailabilityResponse Prelude.Int
batchCheckLayerAvailabilityResponse_httpStatus = Lens.lens (\BatchCheckLayerAvailabilityResponse' {httpStatus} -> httpStatus) (\s@BatchCheckLayerAvailabilityResponse' {} a -> s {httpStatus = a} :: BatchCheckLayerAvailabilityResponse)

instance
  Prelude.NFData
    BatchCheckLayerAvailabilityResponse
  where
  rnf BatchCheckLayerAvailabilityResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf httpStatus

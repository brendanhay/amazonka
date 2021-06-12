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
-- Module      : Network.AWS.ECR.PutImageTagMutability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image tag mutability settings for the specified repository.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-tag-mutability.html Image Tag Mutability>
-- in the /Amazon Elastic Container Registry User Guide/.
module Network.AWS.ECR.PutImageTagMutability
  ( -- * Creating a Request
    PutImageTagMutability (..),
    newPutImageTagMutability,

    -- * Request Lenses
    putImageTagMutability_registryId,
    putImageTagMutability_repositoryName,
    putImageTagMutability_imageTagMutability,

    -- * Destructuring the Response
    PutImageTagMutabilityResponse (..),
    newPutImageTagMutabilityResponse,

    -- * Response Lenses
    putImageTagMutabilityResponse_registryId,
    putImageTagMutabilityResponse_repositoryName,
    putImageTagMutabilityResponse_imageTagMutability,
    putImageTagMutabilityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutImageTagMutability' smart constructor.
data PutImageTagMutability = PutImageTagMutability'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository in which to update the image tag mutability settings. If you
    -- do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository in which to update the image tag mutability
    -- settings.
    repositoryName :: Core.Text,
    -- | The tag mutability setting for the repository. If @MUTABLE@ is
    -- specified, image tags can be overwritten. If @IMMUTABLE@ is specified,
    -- all image tags within the repository will be immutable which will
    -- prevent them from being overwritten.
    imageTagMutability :: ImageTagMutability
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutImageTagMutability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putImageTagMutability_registryId' - The AWS account ID associated with the registry that contains the
-- repository in which to update the image tag mutability settings. If you
-- do not specify a registry, the default registry is assumed.
--
-- 'repositoryName', 'putImageTagMutability_repositoryName' - The name of the repository in which to update the image tag mutability
-- settings.
--
-- 'imageTagMutability', 'putImageTagMutability_imageTagMutability' - The tag mutability setting for the repository. If @MUTABLE@ is
-- specified, image tags can be overwritten. If @IMMUTABLE@ is specified,
-- all image tags within the repository will be immutable which will
-- prevent them from being overwritten.
newPutImageTagMutability ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'imageTagMutability'
  ImageTagMutability ->
  PutImageTagMutability
newPutImageTagMutability
  pRepositoryName_
  pImageTagMutability_ =
    PutImageTagMutability'
      { registryId = Core.Nothing,
        repositoryName = pRepositoryName_,
        imageTagMutability = pImageTagMutability_
      }

-- | The AWS account ID associated with the registry that contains the
-- repository in which to update the image tag mutability settings. If you
-- do not specify a registry, the default registry is assumed.
putImageTagMutability_registryId :: Lens.Lens' PutImageTagMutability (Core.Maybe Core.Text)
putImageTagMutability_registryId = Lens.lens (\PutImageTagMutability' {registryId} -> registryId) (\s@PutImageTagMutability' {} a -> s {registryId = a} :: PutImageTagMutability)

-- | The name of the repository in which to update the image tag mutability
-- settings.
putImageTagMutability_repositoryName :: Lens.Lens' PutImageTagMutability Core.Text
putImageTagMutability_repositoryName = Lens.lens (\PutImageTagMutability' {repositoryName} -> repositoryName) (\s@PutImageTagMutability' {} a -> s {repositoryName = a} :: PutImageTagMutability)

-- | The tag mutability setting for the repository. If @MUTABLE@ is
-- specified, image tags can be overwritten. If @IMMUTABLE@ is specified,
-- all image tags within the repository will be immutable which will
-- prevent them from being overwritten.
putImageTagMutability_imageTagMutability :: Lens.Lens' PutImageTagMutability ImageTagMutability
putImageTagMutability_imageTagMutability = Lens.lens (\PutImageTagMutability' {imageTagMutability} -> imageTagMutability) (\s@PutImageTagMutability' {} a -> s {imageTagMutability = a} :: PutImageTagMutability)

instance Core.AWSRequest PutImageTagMutability where
  type
    AWSResponse PutImageTagMutability =
      PutImageTagMutabilityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutImageTagMutabilityResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "imageTagMutability")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutImageTagMutability

instance Core.NFData PutImageTagMutability

instance Core.ToHeaders PutImageTagMutability where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutImageTagMutability" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutImageTagMutability where
  toJSON PutImageTagMutability' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ("imageTagMutability" Core..= imageTagMutability)
          ]
      )

instance Core.ToPath PutImageTagMutability where
  toPath = Core.const "/"

instance Core.ToQuery PutImageTagMutability where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutImageTagMutabilityResponse' smart constructor.
data PutImageTagMutabilityResponse = PutImageTagMutabilityResponse'
  { -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The image tag mutability setting for the repository.
    imageTagMutability :: Core.Maybe ImageTagMutability,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutImageTagMutabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putImageTagMutabilityResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'putImageTagMutabilityResponse_repositoryName' - The repository name associated with the request.
--
-- 'imageTagMutability', 'putImageTagMutabilityResponse_imageTagMutability' - The image tag mutability setting for the repository.
--
-- 'httpStatus', 'putImageTagMutabilityResponse_httpStatus' - The response's http status code.
newPutImageTagMutabilityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutImageTagMutabilityResponse
newPutImageTagMutabilityResponse pHttpStatus_ =
  PutImageTagMutabilityResponse'
    { registryId =
        Core.Nothing,
      repositoryName = Core.Nothing,
      imageTagMutability = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
putImageTagMutabilityResponse_registryId :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Core.Text)
putImageTagMutabilityResponse_registryId = Lens.lens (\PutImageTagMutabilityResponse' {registryId} -> registryId) (\s@PutImageTagMutabilityResponse' {} a -> s {registryId = a} :: PutImageTagMutabilityResponse)

-- | The repository name associated with the request.
putImageTagMutabilityResponse_repositoryName :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Core.Text)
putImageTagMutabilityResponse_repositoryName = Lens.lens (\PutImageTagMutabilityResponse' {repositoryName} -> repositoryName) (\s@PutImageTagMutabilityResponse' {} a -> s {repositoryName = a} :: PutImageTagMutabilityResponse)

-- | The image tag mutability setting for the repository.
putImageTagMutabilityResponse_imageTagMutability :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe ImageTagMutability)
putImageTagMutabilityResponse_imageTagMutability = Lens.lens (\PutImageTagMutabilityResponse' {imageTagMutability} -> imageTagMutability) (\s@PutImageTagMutabilityResponse' {} a -> s {imageTagMutability = a} :: PutImageTagMutabilityResponse)

-- | The response's http status code.
putImageTagMutabilityResponse_httpStatus :: Lens.Lens' PutImageTagMutabilityResponse Core.Int
putImageTagMutabilityResponse_httpStatus = Lens.lens (\PutImageTagMutabilityResponse' {httpStatus} -> httpStatus) (\s@PutImageTagMutabilityResponse' {} a -> s {httpStatus = a} :: PutImageTagMutabilityResponse)

instance Core.NFData PutImageTagMutabilityResponse

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
-- Module      : Network.AWS.ECR.CreateRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a repository. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Repositories.html Amazon ECR Repositories>
-- in the /Amazon Elastic Container Registry User Guide/.
module Network.AWS.ECR.CreateRepository
  ( -- * Creating a Request
    CreateRepository (..),
    newCreateRepository,

    -- * Request Lenses
    createRepository_encryptionConfiguration,
    createRepository_tags,
    createRepository_imageScanningConfiguration,
    createRepository_imageTagMutability,
    createRepository_repositoryName,

    -- * Destructuring the Response
    CreateRepositoryResponse (..),
    newCreateRepositoryResponse,

    -- * Response Lenses
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { -- | The encryption configuration for the repository. This determines how the
    -- contents of your repository are encrypted at rest.
    encryptionConfiguration :: Core.Maybe EncryptionConfiguration,
    -- | The metadata that you apply to the repository to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define. Tag keys can have a maximum character length of 128
    -- characters, and tag values can have a maximum length of 256 characters.
    tags :: Core.Maybe [Tag],
    -- | The image scanning configuration for the repository. This determines
    -- whether images are scanned for known vulnerabilities after being pushed
    -- to the repository.
    imageScanningConfiguration :: Core.Maybe ImageScanningConfiguration,
    -- | The tag mutability setting for the repository. If this parameter is
    -- omitted, the default setting of @MUTABLE@ will be used which will allow
    -- image tags to be overwritten. If @IMMUTABLE@ is specified, all image
    -- tags within the repository will be immutable which will prevent them
    -- from being overwritten.
    imageTagMutability :: Core.Maybe ImageTagMutability,
    -- | The name to use for the repository. The repository name may be specified
    -- on its own (such as @nginx-web-app@) or it can be prepended with a
    -- namespace to group the repository into a category (such as
    -- @project-a\/nginx-web-app@).
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfiguration', 'createRepository_encryptionConfiguration' - The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
--
-- 'tags', 'createRepository_tags' - The metadata that you apply to the repository to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. Tag keys can have a maximum character length of 128
-- characters, and tag values can have a maximum length of 256 characters.
--
-- 'imageScanningConfiguration', 'createRepository_imageScanningConfiguration' - The image scanning configuration for the repository. This determines
-- whether images are scanned for known vulnerabilities after being pushed
-- to the repository.
--
-- 'imageTagMutability', 'createRepository_imageTagMutability' - The tag mutability setting for the repository. If this parameter is
-- omitted, the default setting of @MUTABLE@ will be used which will allow
-- image tags to be overwritten. If @IMMUTABLE@ is specified, all image
-- tags within the repository will be immutable which will prevent them
-- from being overwritten.
--
-- 'repositoryName', 'createRepository_repositoryName' - The name to use for the repository. The repository name may be specified
-- on its own (such as @nginx-web-app@) or it can be prepended with a
-- namespace to group the repository into a category (such as
-- @project-a\/nginx-web-app@).
newCreateRepository ::
  -- | 'repositoryName'
  Core.Text ->
  CreateRepository
newCreateRepository pRepositoryName_ =
  CreateRepository'
    { encryptionConfiguration =
        Core.Nothing,
      tags = Core.Nothing,
      imageScanningConfiguration = Core.Nothing,
      imageTagMutability = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
createRepository_encryptionConfiguration :: Lens.Lens' CreateRepository (Core.Maybe EncryptionConfiguration)
createRepository_encryptionConfiguration = Lens.lens (\CreateRepository' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateRepository' {} a -> s {encryptionConfiguration = a} :: CreateRepository)

-- | The metadata that you apply to the repository to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. Tag keys can have a maximum character length of 128
-- characters, and tag values can have a maximum length of 256 characters.
createRepository_tags :: Lens.Lens' CreateRepository (Core.Maybe [Tag])
createRepository_tags = Lens.lens (\CreateRepository' {tags} -> tags) (\s@CreateRepository' {} a -> s {tags = a} :: CreateRepository) Core.. Lens.mapping Lens._Coerce

-- | The image scanning configuration for the repository. This determines
-- whether images are scanned for known vulnerabilities after being pushed
-- to the repository.
createRepository_imageScanningConfiguration :: Lens.Lens' CreateRepository (Core.Maybe ImageScanningConfiguration)
createRepository_imageScanningConfiguration = Lens.lens (\CreateRepository' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@CreateRepository' {} a -> s {imageScanningConfiguration = a} :: CreateRepository)

-- | The tag mutability setting for the repository. If this parameter is
-- omitted, the default setting of @MUTABLE@ will be used which will allow
-- image tags to be overwritten. If @IMMUTABLE@ is specified, all image
-- tags within the repository will be immutable which will prevent them
-- from being overwritten.
createRepository_imageTagMutability :: Lens.Lens' CreateRepository (Core.Maybe ImageTagMutability)
createRepository_imageTagMutability = Lens.lens (\CreateRepository' {imageTagMutability} -> imageTagMutability) (\s@CreateRepository' {} a -> s {imageTagMutability = a} :: CreateRepository)

-- | The name to use for the repository. The repository name may be specified
-- on its own (such as @nginx-web-app@) or it can be prepended with a
-- namespace to group the repository into a category (such as
-- @project-a\/nginx-web-app@).
createRepository_repositoryName :: Lens.Lens' CreateRepository Core.Text
createRepository_repositoryName = Lens.lens (\CreateRepository' {repositoryName} -> repositoryName) (\s@CreateRepository' {} a -> s {repositoryName = a} :: CreateRepository)

instance Core.AWSRequest CreateRepository where
  type
    AWSResponse CreateRepository =
      CreateRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Core.<$> (x Core..?> "repository")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRepository

instance Core.NFData CreateRepository

instance Core.ToHeaders CreateRepository where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.CreateRepository" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    Core.object
      ( Core.catMaybes
          [ ("encryptionConfiguration" Core..=)
              Core.<$> encryptionConfiguration,
            ("tags" Core..=) Core.<$> tags,
            ("imageScanningConfiguration" Core..=)
              Core.<$> imageScanningConfiguration,
            ("imageTagMutability" Core..=)
              Core.<$> imageTagMutability,
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath CreateRepository where
  toPath = Core.const "/"

instance Core.ToQuery CreateRepository where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { -- | The repository that was created.
    repository :: Core.Maybe Repository,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repository', 'createRepositoryResponse_repository' - The repository that was created.
--
-- 'httpStatus', 'createRepositoryResponse_httpStatus' - The response's http status code.
newCreateRepositoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRepositoryResponse
newCreateRepositoryResponse pHttpStatus_ =
  CreateRepositoryResponse'
    { repository =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The repository that was created.
createRepositoryResponse_repository :: Lens.Lens' CreateRepositoryResponse (Core.Maybe Repository)
createRepositoryResponse_repository = Lens.lens (\CreateRepositoryResponse' {repository} -> repository) (\s@CreateRepositoryResponse' {} a -> s {repository = a} :: CreateRepositoryResponse)

-- | The response's http status code.
createRepositoryResponse_httpStatus :: Lens.Lens' CreateRepositoryResponse Core.Int
createRepositoryResponse_httpStatus = Lens.lens (\CreateRepositoryResponse' {httpStatus} -> httpStatus) (\s@CreateRepositoryResponse' {} a -> s {httpStatus = a} :: CreateRepositoryResponse)

instance Core.NFData CreateRepositoryResponse

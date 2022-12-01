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
-- Module      : Amazonka.ECR.CreateRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a repository. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Repositories.html Amazon ECR repositories>
-- in the /Amazon Elastic Container Registry User Guide/.
module Amazonka.ECR.CreateRepository
  ( -- * Creating a Request
    CreateRepository (..),
    newCreateRepository,

    -- * Request Lenses
    createRepository_tags,
    createRepository_imageTagMutability,
    createRepository_encryptionConfiguration,
    createRepository_registryId,
    createRepository_imageScanningConfiguration,
    createRepository_repositoryName,

    -- * Destructuring the Response
    CreateRepositoryResponse (..),
    newCreateRepositoryResponse,

    -- * Response Lenses
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { -- | The metadata that you apply to the repository to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define. Tag keys can have a maximum character length of 128
    -- characters, and tag values can have a maximum length of 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | The tag mutability setting for the repository. If this parameter is
    -- omitted, the default setting of @MUTABLE@ will be used which will allow
    -- image tags to be overwritten. If @IMMUTABLE@ is specified, all image
    -- tags within the repository will be immutable which will prevent them
    -- from being overwritten.
    imageTagMutability :: Prelude.Maybe ImageTagMutability,
    -- | The encryption configuration for the repository. This determines how the
    -- contents of your repository are encrypted at rest.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The Amazon Web Services account ID associated with the registry to
    -- create the repository. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The image scanning configuration for the repository. This determines
    -- whether images are scanned for known vulnerabilities after being pushed
    -- to the repository.
    imageScanningConfiguration :: Prelude.Maybe ImageScanningConfiguration,
    -- | The name to use for the repository. The repository name may be specified
    -- on its own (such as @nginx-web-app@) or it can be prepended with a
    -- namespace to group the repository into a category (such as
    -- @project-a\/nginx-web-app@).
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRepository_tags' - The metadata that you apply to the repository to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. Tag keys can have a maximum character length of 128
-- characters, and tag values can have a maximum length of 256 characters.
--
-- 'imageTagMutability', 'createRepository_imageTagMutability' - The tag mutability setting for the repository. If this parameter is
-- omitted, the default setting of @MUTABLE@ will be used which will allow
-- image tags to be overwritten. If @IMMUTABLE@ is specified, all image
-- tags within the repository will be immutable which will prevent them
-- from being overwritten.
--
-- 'encryptionConfiguration', 'createRepository_encryptionConfiguration' - The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
--
-- 'registryId', 'createRepository_registryId' - The Amazon Web Services account ID associated with the registry to
-- create the repository. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'imageScanningConfiguration', 'createRepository_imageScanningConfiguration' - The image scanning configuration for the repository. This determines
-- whether images are scanned for known vulnerabilities after being pushed
-- to the repository.
--
-- 'repositoryName', 'createRepository_repositoryName' - The name to use for the repository. The repository name may be specified
-- on its own (such as @nginx-web-app@) or it can be prepended with a
-- namespace to group the repository into a category (such as
-- @project-a\/nginx-web-app@).
newCreateRepository ::
  -- | 'repositoryName'
  Prelude.Text ->
  CreateRepository
newCreateRepository pRepositoryName_ =
  CreateRepository'
    { tags = Prelude.Nothing,
      imageTagMutability = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      registryId = Prelude.Nothing,
      imageScanningConfiguration = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The metadata that you apply to the repository to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. Tag keys can have a maximum character length of 128
-- characters, and tag values can have a maximum length of 256 characters.
createRepository_tags :: Lens.Lens' CreateRepository (Prelude.Maybe [Tag])
createRepository_tags = Lens.lens (\CreateRepository' {tags} -> tags) (\s@CreateRepository' {} a -> s {tags = a} :: CreateRepository) Prelude.. Lens.mapping Lens.coerced

-- | The tag mutability setting for the repository. If this parameter is
-- omitted, the default setting of @MUTABLE@ will be used which will allow
-- image tags to be overwritten. If @IMMUTABLE@ is specified, all image
-- tags within the repository will be immutable which will prevent them
-- from being overwritten.
createRepository_imageTagMutability :: Lens.Lens' CreateRepository (Prelude.Maybe ImageTagMutability)
createRepository_imageTagMutability = Lens.lens (\CreateRepository' {imageTagMutability} -> imageTagMutability) (\s@CreateRepository' {} a -> s {imageTagMutability = a} :: CreateRepository)

-- | The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
createRepository_encryptionConfiguration :: Lens.Lens' CreateRepository (Prelude.Maybe EncryptionConfiguration)
createRepository_encryptionConfiguration = Lens.lens (\CreateRepository' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateRepository' {} a -> s {encryptionConfiguration = a} :: CreateRepository)

-- | The Amazon Web Services account ID associated with the registry to
-- create the repository. If you do not specify a registry, the default
-- registry is assumed.
createRepository_registryId :: Lens.Lens' CreateRepository (Prelude.Maybe Prelude.Text)
createRepository_registryId = Lens.lens (\CreateRepository' {registryId} -> registryId) (\s@CreateRepository' {} a -> s {registryId = a} :: CreateRepository)

-- | The image scanning configuration for the repository. This determines
-- whether images are scanned for known vulnerabilities after being pushed
-- to the repository.
createRepository_imageScanningConfiguration :: Lens.Lens' CreateRepository (Prelude.Maybe ImageScanningConfiguration)
createRepository_imageScanningConfiguration = Lens.lens (\CreateRepository' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@CreateRepository' {} a -> s {imageScanningConfiguration = a} :: CreateRepository)

-- | The name to use for the repository. The repository name may be specified
-- on its own (such as @nginx-web-app@) or it can be prepended with a
-- namespace to group the repository into a category (such as
-- @project-a\/nginx-web-app@).
createRepository_repositoryName :: Lens.Lens' CreateRepository Prelude.Text
createRepository_repositoryName = Lens.lens (\CreateRepository' {repositoryName} -> repositoryName) (\s@CreateRepository' {} a -> s {repositoryName = a} :: CreateRepository)

instance Core.AWSRequest CreateRepository where
  type
    AWSResponse CreateRepository =
      CreateRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Prelude.<$> (x Core..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRepository where
  hashWithSalt _salt CreateRepository' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` imageTagMutability
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` imageScanningConfiguration
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData CreateRepository where
  rnf CreateRepository' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf imageTagMutability
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf imageScanningConfiguration
      `Prelude.seq` Prelude.rnf repositoryName

instance Core.ToHeaders CreateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.CreateRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("imageTagMutability" Core..=)
              Prelude.<$> imageTagMutability,
            ("encryptionConfiguration" Core..=)
              Prelude.<$> encryptionConfiguration,
            ("registryId" Core..=) Prelude.<$> registryId,
            ("imageScanningConfiguration" Core..=)
              Prelude.<$> imageScanningConfiguration,
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath CreateRepository where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { -- | The repository that was created.
    repository :: Prelude.Maybe Repository,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateRepositoryResponse
newCreateRepositoryResponse pHttpStatus_ =
  CreateRepositoryResponse'
    { repository =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The repository that was created.
createRepositoryResponse_repository :: Lens.Lens' CreateRepositoryResponse (Prelude.Maybe Repository)
createRepositoryResponse_repository = Lens.lens (\CreateRepositoryResponse' {repository} -> repository) (\s@CreateRepositoryResponse' {} a -> s {repository = a} :: CreateRepositoryResponse)

-- | The response's http status code.
createRepositoryResponse_httpStatus :: Lens.Lens' CreateRepositoryResponse Prelude.Int
createRepositoryResponse_httpStatus = Lens.lens (\CreateRepositoryResponse' {httpStatus} -> httpStatus) (\s@CreateRepositoryResponse' {} a -> s {httpStatus = a} :: CreateRepositoryResponse)

instance Prelude.NFData CreateRepositoryResponse where
  rnf CreateRepositoryResponse' {..} =
    Prelude.rnf repository
      `Prelude.seq` Prelude.rnf httpStatus

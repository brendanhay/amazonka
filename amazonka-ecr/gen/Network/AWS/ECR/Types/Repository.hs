{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Repository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Repository where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.EncryptionConfiguration
import Network.AWS.ECR.Types.ImageScanningConfiguration
import Network.AWS.ECR.Types.ImageTagMutability
import qualified Network.AWS.Lens as Lens

-- | An object representing a repository.
--
-- /See:/ 'newRepository' smart constructor.
data Repository = Repository'
  { -- | The encryption configuration for the repository. This determines how the
    -- contents of your repository are encrypted at rest.
    encryptionConfiguration :: Core.Maybe EncryptionConfiguration,
    -- | The URI for the repository. You can use this URI for container image
    -- @push@ and @pull@ operations.
    repositoryUri :: Core.Maybe Core.Text,
    -- | The AWS account ID associated with the registry that contains the
    -- repository.
    registryId :: Core.Maybe Core.Text,
    -- | The date and time, in JavaScript date format, when the repository was
    -- created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The name of the repository.
    repositoryName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
    -- contains the @arn:aws:ecr@ namespace, followed by the region of the
    -- repository, AWS account ID of the repository owner, repository
    -- namespace, and repository name. For example,
    -- @arn:aws:ecr:region:012345678910:repository\/test@.
    repositoryArn :: Core.Maybe Core.Text,
    imageScanningConfiguration :: Core.Maybe ImageScanningConfiguration,
    -- | The tag mutability setting for the repository.
    imageTagMutability :: Core.Maybe ImageTagMutability
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Repository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfiguration', 'repository_encryptionConfiguration' - The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
--
-- 'repositoryUri', 'repository_repositoryUri' - The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
--
-- 'registryId', 'repository_registryId' - The AWS account ID associated with the registry that contains the
-- repository.
--
-- 'createdAt', 'repository_createdAt' - The date and time, in JavaScript date format, when the repository was
-- created.
--
-- 'repositoryName', 'repository_repositoryName' - The name of the repository.
--
-- 'repositoryArn', 'repository_repositoryArn' - The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, AWS account ID of the repository owner, repository
-- namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
--
-- 'imageScanningConfiguration', 'repository_imageScanningConfiguration' - Undocumented member.
--
-- 'imageTagMutability', 'repository_imageTagMutability' - The tag mutability setting for the repository.
newRepository ::
  Repository
newRepository =
  Repository'
    { encryptionConfiguration = Core.Nothing,
      repositoryUri = Core.Nothing,
      registryId = Core.Nothing,
      createdAt = Core.Nothing,
      repositoryName = Core.Nothing,
      repositoryArn = Core.Nothing,
      imageScanningConfiguration = Core.Nothing,
      imageTagMutability = Core.Nothing
    }

-- | The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
repository_encryptionConfiguration :: Lens.Lens' Repository (Core.Maybe EncryptionConfiguration)
repository_encryptionConfiguration = Lens.lens (\Repository' {encryptionConfiguration} -> encryptionConfiguration) (\s@Repository' {} a -> s {encryptionConfiguration = a} :: Repository)

-- | The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
repository_repositoryUri :: Lens.Lens' Repository (Core.Maybe Core.Text)
repository_repositoryUri = Lens.lens (\Repository' {repositoryUri} -> repositoryUri) (\s@Repository' {} a -> s {repositoryUri = a} :: Repository)

-- | The AWS account ID associated with the registry that contains the
-- repository.
repository_registryId :: Lens.Lens' Repository (Core.Maybe Core.Text)
repository_registryId = Lens.lens (\Repository' {registryId} -> registryId) (\s@Repository' {} a -> s {registryId = a} :: Repository)

-- | The date and time, in JavaScript date format, when the repository was
-- created.
repository_createdAt :: Lens.Lens' Repository (Core.Maybe Core.UTCTime)
repository_createdAt = Lens.lens (\Repository' {createdAt} -> createdAt) (\s@Repository' {} a -> s {createdAt = a} :: Repository) Core.. Lens.mapping Core._Time

-- | The name of the repository.
repository_repositoryName :: Lens.Lens' Repository (Core.Maybe Core.Text)
repository_repositoryName = Lens.lens (\Repository' {repositoryName} -> repositoryName) (\s@Repository' {} a -> s {repositoryName = a} :: Repository)

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, AWS account ID of the repository owner, repository
-- namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
repository_repositoryArn :: Lens.Lens' Repository (Core.Maybe Core.Text)
repository_repositoryArn = Lens.lens (\Repository' {repositoryArn} -> repositoryArn) (\s@Repository' {} a -> s {repositoryArn = a} :: Repository)

-- | Undocumented member.
repository_imageScanningConfiguration :: Lens.Lens' Repository (Core.Maybe ImageScanningConfiguration)
repository_imageScanningConfiguration = Lens.lens (\Repository' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@Repository' {} a -> s {imageScanningConfiguration = a} :: Repository)

-- | The tag mutability setting for the repository.
repository_imageTagMutability :: Lens.Lens' Repository (Core.Maybe ImageTagMutability)
repository_imageTagMutability = Lens.lens (\Repository' {imageTagMutability} -> imageTagMutability) (\s@Repository' {} a -> s {imageTagMutability = a} :: Repository)

instance Core.FromJSON Repository where
  parseJSON =
    Core.withObject
      "Repository"
      ( \x ->
          Repository'
            Core.<$> (x Core..:? "encryptionConfiguration")
            Core.<*> (x Core..:? "repositoryUri")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "repositoryArn")
            Core.<*> (x Core..:? "imageScanningConfiguration")
            Core.<*> (x Core..:? "imageTagMutability")
      )

instance Core.Hashable Repository

instance Core.NFData Repository

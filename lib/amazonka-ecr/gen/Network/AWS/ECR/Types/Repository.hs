{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Repository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.Repository
  ( Repository (..)
  -- * Smart constructor
  , mkRepository
  -- * Lenses
  , rCreatedAt
  , rEncryptionConfiguration
  , rImageScanningConfiguration
  , rImageTagMutability
  , rRegistryId
  , rRepositoryArn
  , rRepositoryName
  , rRepositoryUri
  ) where

import qualified Network.AWS.ECR.Types.EncryptionConfiguration as Types
import qualified Network.AWS.ECR.Types.ImageScanningConfiguration as Types
import qualified Network.AWS.ECR.Types.ImageTagMutability as Types
import qualified Network.AWS.ECR.Types.RegistryId as Types
import qualified Network.AWS.ECR.Types.RepositoryArn as Types
import qualified Network.AWS.ECR.Types.RepositoryName as Types
import qualified Network.AWS.ECR.Types.RepositoryUri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a repository.
--
-- /See:/ 'mkRepository' smart constructor.
data Repository = Repository'
  { createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time, in JavaScript date format, when the repository was created.
  , encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration
    -- ^ The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
  , imageScanningConfiguration :: Core.Maybe Types.ImageScanningConfiguration
  , imageTagMutability :: Core.Maybe Types.ImageTagMutability
    -- ^ The tag mutability setting for the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository.
  , repositoryArn :: Core.Maybe Types.RepositoryArn
    -- ^ The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository.
  , repositoryUri :: Core.Maybe Types.RepositoryUri
    -- ^ The URI for the repository. You can use this URI for container image @push@ and @pull@ operations.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Repository' value with any optional fields omitted.
mkRepository
    :: Repository
mkRepository
  = Repository'{createdAt = Core.Nothing,
                encryptionConfiguration = Core.Nothing,
                imageScanningConfiguration = Core.Nothing,
                imageTagMutability = Core.Nothing, registryId = Core.Nothing,
                repositoryArn = Core.Nothing, repositoryName = Core.Nothing,
                repositoryUri = Core.Nothing}

-- | The date and time, in JavaScript date format, when the repository was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreatedAt :: Lens.Lens' Repository (Core.Maybe Core.NominalDiffTime)
rCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE rCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEncryptionConfiguration :: Lens.Lens' Repository (Core.Maybe Types.EncryptionConfiguration)
rEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE rEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rImageScanningConfiguration :: Lens.Lens' Repository (Core.Maybe Types.ImageScanningConfiguration)
rImageScanningConfiguration = Lens.field @"imageScanningConfiguration"
{-# INLINEABLE rImageScanningConfiguration #-}
{-# DEPRECATED imageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead"  #-}

-- | The tag mutability setting for the repository.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rImageTagMutability :: Lens.Lens' Repository (Core.Maybe Types.ImageTagMutability)
rImageTagMutability = Lens.field @"imageTagMutability"
{-# INLINEABLE rImageTagMutability #-}
{-# DEPRECATED imageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegistryId :: Lens.Lens' Repository (Core.Maybe Types.RegistryId)
rRegistryId = Lens.field @"registryId"
{-# INLINEABLE rRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
--
-- /Note:/ Consider using 'repositoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRepositoryArn :: Lens.Lens' Repository (Core.Maybe Types.RepositoryArn)
rRepositoryArn = Lens.field @"repositoryArn"
{-# INLINEABLE rRepositoryArn #-}
{-# DEPRECATED repositoryArn "Use generic-lens or generic-optics with 'repositoryArn' instead"  #-}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRepositoryName :: Lens.Lens' Repository (Core.Maybe Types.RepositoryName)
rRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE rRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The URI for the repository. You can use this URI for container image @push@ and @pull@ operations.
--
-- /Note:/ Consider using 'repositoryUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRepositoryUri :: Lens.Lens' Repository (Core.Maybe Types.RepositoryUri)
rRepositoryUri = Lens.field @"repositoryUri"
{-# INLINEABLE rRepositoryUri #-}
{-# DEPRECATED repositoryUri "Use generic-lens or generic-optics with 'repositoryUri' instead"  #-}

instance Core.FromJSON Repository where
        parseJSON
          = Core.withObject "Repository" Core.$
              \ x ->
                Repository' Core.<$>
                  (x Core..:? "createdAt") Core.<*>
                    x Core..:? "encryptionConfiguration"
                    Core.<*> x Core..:? "imageScanningConfiguration"
                    Core.<*> x Core..:? "imageTagMutability"
                    Core.<*> x Core..:? "registryId"
                    Core.<*> x Core..:? "repositoryArn"
                    Core.<*> x Core..:? "repositoryName"
                    Core.<*> x Core..:? "repositoryUri"

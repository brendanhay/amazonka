{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Repository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Repository
  ( Repository (..),

    -- * Smart constructor
    mkRepository,

    -- * Lenses
    rRepositoryARN,
    rCreatedAt,
    rRegistryId,
    rImageScanningConfiguration,
    rRepositoryURI,
    rEncryptionConfiguration,
    rRepositoryName,
    rImageTagMutability,
  )
where

import Network.AWS.ECR.Types.EncryptionConfiguration
import Network.AWS.ECR.Types.ImageScanningConfiguration
import Network.AWS.ECR.Types.ImageTagMutability
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a repository.
--
-- /See:/ 'mkRepository' smart constructor.
data Repository = Repository'
  { repositoryARN ::
      Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    registryId :: Lude.Maybe Lude.Text,
    imageScanningConfiguration ::
      Lude.Maybe ImageScanningConfiguration,
    repositoryURI :: Lude.Maybe Lude.Text,
    encryptionConfiguration :: Lude.Maybe EncryptionConfiguration,
    repositoryName :: Lude.Maybe Lude.Text,
    imageTagMutability :: Lude.Maybe ImageTagMutability
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Repository' with the minimum fields required to make a request.
--
-- * 'createdAt' - The date and time, in JavaScript date format, when the repository was created.
-- * 'encryptionConfiguration' - The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
-- * 'imageScanningConfiguration' - Undocumented field.
-- * 'imageTagMutability' - The tag mutability setting for the repository.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository.
-- * 'repositoryARN' - The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
-- * 'repositoryName' - The name of the repository.
-- * 'repositoryURI' - The URI for the repository. You can use this URI for container image @push@ and @pull@ operations.
mkRepository ::
  Repository
mkRepository =
  Repository'
    { repositoryARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      registryId = Lude.Nothing,
      imageScanningConfiguration = Lude.Nothing,
      repositoryURI = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      repositoryName = Lude.Nothing,
      imageTagMutability = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
--
-- /Note:/ Consider using 'repositoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRepositoryARN :: Lens.Lens' Repository (Lude.Maybe Lude.Text)
rRepositoryARN = Lens.lens (repositoryARN :: Repository -> Lude.Maybe Lude.Text) (\s a -> s {repositoryARN = a} :: Repository)
{-# DEPRECATED rRepositoryARN "Use generic-lens or generic-optics with 'repositoryARN' instead." #-}

-- | The date and time, in JavaScript date format, when the repository was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreatedAt :: Lens.Lens' Repository (Lude.Maybe Lude.Timestamp)
rCreatedAt = Lens.lens (createdAt :: Repository -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Repository)
{-# DEPRECATED rCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegistryId :: Lens.Lens' Repository (Lude.Maybe Lude.Text)
rRegistryId = Lens.lens (registryId :: Repository -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: Repository)
{-# DEPRECATED rRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rImageScanningConfiguration :: Lens.Lens' Repository (Lude.Maybe ImageScanningConfiguration)
rImageScanningConfiguration = Lens.lens (imageScanningConfiguration :: Repository -> Lude.Maybe ImageScanningConfiguration) (\s a -> s {imageScanningConfiguration = a} :: Repository)
{-# DEPRECATED rImageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead." #-}

-- | The URI for the repository. You can use this URI for container image @push@ and @pull@ operations.
--
-- /Note:/ Consider using 'repositoryURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRepositoryURI :: Lens.Lens' Repository (Lude.Maybe Lude.Text)
rRepositoryURI = Lens.lens (repositoryURI :: Repository -> Lude.Maybe Lude.Text) (\s a -> s {repositoryURI = a} :: Repository)
{-# DEPRECATED rRepositoryURI "Use generic-lens or generic-optics with 'repositoryURI' instead." #-}

-- | The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEncryptionConfiguration :: Lens.Lens' Repository (Lude.Maybe EncryptionConfiguration)
rEncryptionConfiguration = Lens.lens (encryptionConfiguration :: Repository -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: Repository)
{-# DEPRECATED rEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRepositoryName :: Lens.Lens' Repository (Lude.Maybe Lude.Text)
rRepositoryName = Lens.lens (repositoryName :: Repository -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: Repository)
{-# DEPRECATED rRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The tag mutability setting for the repository.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rImageTagMutability :: Lens.Lens' Repository (Lude.Maybe ImageTagMutability)
rImageTagMutability = Lens.lens (imageTagMutability :: Repository -> Lude.Maybe ImageTagMutability) (\s a -> s {imageTagMutability = a} :: Repository)
{-# DEPRECATED rImageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead." #-}

instance Lude.FromJSON Repository where
  parseJSON =
    Lude.withObject
      "Repository"
      ( \x ->
          Repository'
            Lude.<$> (x Lude..:? "repositoryArn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "registryId")
            Lude.<*> (x Lude..:? "imageScanningConfiguration")
            Lude.<*> (x Lude..:? "repositoryUri")
            Lude.<*> (x Lude..:? "encryptionConfiguration")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "imageTagMutability")
      )

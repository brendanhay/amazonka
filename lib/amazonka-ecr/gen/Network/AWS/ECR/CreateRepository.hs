{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.CreateRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Repositories.html Amazon ECR Repositories> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.CreateRepository
  ( -- * Creating a request
    CreateRepository (..),
    mkCreateRepository,

    -- ** Request lenses
    crImageScanningConfiguration,
    crEncryptionConfiguration,
    crImageTagMutability,
    crTags,
    crRepositoryName,

    -- * Destructuring the response
    CreateRepositoryResponse (..),
    mkCreateRepositoryResponse,

    -- ** Response lenses
    crrsRepository,
    crrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { imageScanningConfiguration ::
      Lude.Maybe ImageScanningConfiguration,
    encryptionConfiguration ::
      Lude.Maybe EncryptionConfiguration,
    imageTagMutability :: Lude.Maybe ImageTagMutability,
    tags :: Lude.Maybe [Tag],
    repositoryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRepository' with the minimum fields required to make a request.
--
-- * 'encryptionConfiguration' - The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
-- * 'imageScanningConfiguration' - The image scanning configuration for the repository. This determines whether images are scanned for known vulnerabilities after being pushed to the repository.
-- * 'imageTagMutability' - The tag mutability setting for the repository. If this parameter is omitted, the default setting of @MUTABLE@ will be used which will allow image tags to be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
-- * 'repositoryName' - The name to use for the repository. The repository name may be specified on its own (such as @nginx-web-app@ ) or it can be prepended with a namespace to group the repository into a category (such as @project-a/nginx-web-app@ ).
-- * 'tags' - The metadata that you apply to the repository to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
mkCreateRepository ::
  -- | 'repositoryName'
  Lude.Text ->
  CreateRepository
mkCreateRepository pRepositoryName_ =
  CreateRepository'
    { imageScanningConfiguration = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      imageTagMutability = Lude.Nothing,
      tags = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The image scanning configuration for the repository. This determines whether images are scanned for known vulnerabilities after being pushed to the repository.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crImageScanningConfiguration :: Lens.Lens' CreateRepository (Lude.Maybe ImageScanningConfiguration)
crImageScanningConfiguration = Lens.lens (imageScanningConfiguration :: CreateRepository -> Lude.Maybe ImageScanningConfiguration) (\s a -> s {imageScanningConfiguration = a} :: CreateRepository)
{-# DEPRECATED crImageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead." #-}

-- | The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEncryptionConfiguration :: Lens.Lens' CreateRepository (Lude.Maybe EncryptionConfiguration)
crEncryptionConfiguration = Lens.lens (encryptionConfiguration :: CreateRepository -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: CreateRepository)
{-# DEPRECATED crEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The tag mutability setting for the repository. If this parameter is omitted, the default setting of @MUTABLE@ will be used which will allow image tags to be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crImageTagMutability :: Lens.Lens' CreateRepository (Lude.Maybe ImageTagMutability)
crImageTagMutability = Lens.lens (imageTagMutability :: CreateRepository -> Lude.Maybe ImageTagMutability) (\s a -> s {imageTagMutability = a} :: CreateRepository)
{-# DEPRECATED crImageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead." #-}

-- | The metadata that you apply to the repository to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRepository (Lude.Maybe [Tag])
crTags = Lens.lens (tags :: CreateRepository -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateRepository)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name to use for the repository. The repository name may be specified on its own (such as @nginx-web-app@ ) or it can be prepended with a namespace to group the repository into a category (such as @project-a/nginx-web-app@ ).
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRepositoryName :: Lens.Lens' CreateRepository Lude.Text
crRepositoryName = Lens.lens (repositoryName :: CreateRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: CreateRepository)
{-# DEPRECATED crRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest CreateRepository where
  type Rs CreateRepository = CreateRepositoryResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Lude.<$> (x Lude..?> "repository") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.CreateRepository" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("imageScanningConfiguration" Lude..=)
              Lude.<$> imageScanningConfiguration,
            ("encryptionConfiguration" Lude..=)
              Lude.<$> encryptionConfiguration,
            ("imageTagMutability" Lude..=) Lude.<$> imageTagMutability,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath CreateRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRepository where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { repository ::
      Lude.Maybe Repository,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'repository' - The repository that was created.
-- * 'responseStatus' - The response status code.
mkCreateRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRepositoryResponse
mkCreateRepositoryResponse pResponseStatus_ =
  CreateRepositoryResponse'
    { repository = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The repository that was created.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRepository :: Lens.Lens' CreateRepositoryResponse (Lude.Maybe Repository)
crrsRepository = Lens.lens (repository :: CreateRepositoryResponse -> Lude.Maybe Repository) (\s a -> s {repository = a} :: CreateRepositoryResponse)
{-# DEPRECATED crrsRepository "Use generic-lens or generic-optics with 'repository' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRepositoryResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRepositoryResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

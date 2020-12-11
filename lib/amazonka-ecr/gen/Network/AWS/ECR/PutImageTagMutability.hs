{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutImageTagMutability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image tag mutability settings for the specified repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-tag-mutability.html Image Tag Mutability> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.PutImageTagMutability
  ( -- * Creating a request
    PutImageTagMutability (..),
    mkPutImageTagMutability,

    -- ** Request lenses
    pitmRegistryId,
    pitmRepositoryName,
    pitmImageTagMutability,

    -- * Destructuring the response
    PutImageTagMutabilityResponse (..),
    mkPutImageTagMutabilityResponse,

    -- ** Response lenses
    pitmrsRegistryId,
    pitmrsRepositoryName,
    pitmrsImageTagMutability,
    pitmrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutImageTagMutability' smart constructor.
data PutImageTagMutability = PutImageTagMutability'
  { registryId ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    imageTagMutability :: ImageTagMutability
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutImageTagMutability' with the minimum fields required to make a request.
--
-- * 'imageTagMutability' - The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository in which to update the image tag mutability settings.
mkPutImageTagMutability ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'imageTagMutability'
  ImageTagMutability ->
  PutImageTagMutability
mkPutImageTagMutability pRepositoryName_ pImageTagMutability_ =
  PutImageTagMutability'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_,
      imageTagMutability = pImageTagMutability_
    }

-- | The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmRegistryId :: Lens.Lens' PutImageTagMutability (Lude.Maybe Lude.Text)
pitmRegistryId = Lens.lens (registryId :: PutImageTagMutability -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: PutImageTagMutability)
{-# DEPRECATED pitmRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository in which to update the image tag mutability settings.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmRepositoryName :: Lens.Lens' PutImageTagMutability Lude.Text
pitmRepositoryName = Lens.lens (repositoryName :: PutImageTagMutability -> Lude.Text) (\s a -> s {repositoryName = a} :: PutImageTagMutability)
{-# DEPRECATED pitmRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmImageTagMutability :: Lens.Lens' PutImageTagMutability ImageTagMutability
pitmImageTagMutability = Lens.lens (imageTagMutability :: PutImageTagMutability -> ImageTagMutability) (\s a -> s {imageTagMutability = a} :: PutImageTagMutability)
{-# DEPRECATED pitmImageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead." #-}

instance Lude.AWSRequest PutImageTagMutability where
  type Rs PutImageTagMutability = PutImageTagMutabilityResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutImageTagMutabilityResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "imageTagMutability")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutImageTagMutability where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.PutImageTagMutability" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutImageTagMutability where
  toJSON PutImageTagMutability' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("imageTagMutability" Lude..= imageTagMutability)
          ]
      )

instance Lude.ToPath PutImageTagMutability where
  toPath = Lude.const "/"

instance Lude.ToQuery PutImageTagMutability where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutImageTagMutabilityResponse' smart constructor.
data PutImageTagMutabilityResponse = PutImageTagMutabilityResponse'
  { registryId ::
      Lude.Maybe Lude.Text,
    repositoryName ::
      Lude.Maybe Lude.Text,
    imageTagMutability ::
      Lude.Maybe ImageTagMutability,
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

-- | Creates a value of 'PutImageTagMutabilityResponse' with the minimum fields required to make a request.
--
-- * 'imageTagMutability' - The image tag mutability setting for the repository.
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkPutImageTagMutabilityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutImageTagMutabilityResponse
mkPutImageTagMutabilityResponse pResponseStatus_ =
  PutImageTagMutabilityResponse'
    { registryId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      imageTagMutability = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrsRegistryId :: Lens.Lens' PutImageTagMutabilityResponse (Lude.Maybe Lude.Text)
pitmrsRegistryId = Lens.lens (registryId :: PutImageTagMutabilityResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: PutImageTagMutabilityResponse)
{-# DEPRECATED pitmrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrsRepositoryName :: Lens.Lens' PutImageTagMutabilityResponse (Lude.Maybe Lude.Text)
pitmrsRepositoryName = Lens.lens (repositoryName :: PutImageTagMutabilityResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PutImageTagMutabilityResponse)
{-# DEPRECATED pitmrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The image tag mutability setting for the repository.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrsImageTagMutability :: Lens.Lens' PutImageTagMutabilityResponse (Lude.Maybe ImageTagMutability)
pitmrsImageTagMutability = Lens.lens (imageTagMutability :: PutImageTagMutabilityResponse -> Lude.Maybe ImageTagMutability) (\s a -> s {imageTagMutability = a} :: PutImageTagMutabilityResponse)
{-# DEPRECATED pitmrsImageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrsResponseStatus :: Lens.Lens' PutImageTagMutabilityResponse Lude.Int
pitmrsResponseStatus = Lens.lens (responseStatus :: PutImageTagMutabilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutImageTagMutabilityResponse)
{-# DEPRECATED pitmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

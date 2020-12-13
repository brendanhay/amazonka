{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.StartLifecyclePolicyPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a preview of a lifecycle policy for the specified repository. This allows you to see the results before associating the lifecycle policy with the repository.
module Network.AWS.ECR.StartLifecyclePolicyPreview
  ( -- * Creating a request
    StartLifecyclePolicyPreview (..),
    mkStartLifecyclePolicyPreview,

    -- ** Request lenses
    slppRegistryId,
    slppLifecyclePolicyText,
    slppRepositoryName,

    -- * Destructuring the response
    StartLifecyclePolicyPreviewResponse (..),
    mkStartLifecyclePolicyPreviewResponse,

    -- ** Response lenses
    slpprsStatus,
    slpprsRegistryId,
    slpprsLifecyclePolicyText,
    slpprsRepositoryName,
    slpprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartLifecyclePolicyPreview' smart constructor.
data StartLifecyclePolicyPreview = StartLifecyclePolicyPreview'
  { -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
    lifecyclePolicyText :: Lude.Maybe Lude.Text,
    -- | The name of the repository to be evaluated.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartLifecyclePolicyPreview' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
-- * 'lifecyclePolicyText' - The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
-- * 'repositoryName' - The name of the repository to be evaluated.
mkStartLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Lude.Text ->
  StartLifecyclePolicyPreview
mkStartLifecyclePolicyPreview pRepositoryName_ =
  StartLifecyclePolicyPreview'
    { registryId = Lude.Nothing,
      lifecyclePolicyText = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppRegistryId :: Lens.Lens' StartLifecyclePolicyPreview (Lude.Maybe Lude.Text)
slppRegistryId = Lens.lens (registryId :: StartLifecyclePolicyPreview -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: StartLifecyclePolicyPreview)
{-# DEPRECATED slppRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppLifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreview (Lude.Maybe Lude.Text)
slppLifecyclePolicyText = Lens.lens (lifecyclePolicyText :: StartLifecyclePolicyPreview -> Lude.Maybe Lude.Text) (\s a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreview)
{-# DEPRECATED slppLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The name of the repository to be evaluated.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppRepositoryName :: Lens.Lens' StartLifecyclePolicyPreview Lude.Text
slppRepositoryName = Lens.lens (repositoryName :: StartLifecyclePolicyPreview -> Lude.Text) (\s a -> s {repositoryName = a} :: StartLifecyclePolicyPreview)
{-# DEPRECATED slppRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest StartLifecyclePolicyPreview where
  type
    Rs StartLifecyclePolicyPreview =
      StartLifecyclePolicyPreviewResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartLifecyclePolicyPreviewResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "lifecyclePolicyText")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartLifecyclePolicyPreview where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.StartLifecyclePolicyPreview" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartLifecyclePolicyPreview where
  toJSON StartLifecyclePolicyPreview' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            ("lifecyclePolicyText" Lude..=) Lude.<$> lifecyclePolicyText,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath StartLifecyclePolicyPreview where
  toPath = Lude.const "/"

instance Lude.ToQuery StartLifecyclePolicyPreview where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartLifecyclePolicyPreviewResponse' smart constructor.
data StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse'
  { -- | The status of the lifecycle policy preview request.
    status :: Lude.Maybe LifecyclePolicyPreviewStatus,
    -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The JSON repository policy text.
    lifecyclePolicyText :: Lude.Maybe Lude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartLifecyclePolicyPreviewResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the lifecycle policy preview request.
-- * 'registryId' - The registry ID associated with the request.
-- * 'lifecyclePolicyText' - The JSON repository policy text.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkStartLifecyclePolicyPreviewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartLifecyclePolicyPreviewResponse
mkStartLifecyclePolicyPreviewResponse pResponseStatus_ =
  StartLifecyclePolicyPreviewResponse'
    { status = Lude.Nothing,
      registryId = Lude.Nothing,
      lifecyclePolicyText = Lude.Nothing,
      repositoryName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprsStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Lude.Maybe LifecyclePolicyPreviewStatus)
slpprsStatus = Lens.lens (status :: StartLifecyclePolicyPreviewResponse -> Lude.Maybe LifecyclePolicyPreviewStatus) (\s a -> s {status = a} :: StartLifecyclePolicyPreviewResponse)
{-# DEPRECATED slpprsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprsRegistryId :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Lude.Maybe Lude.Text)
slpprsRegistryId = Lens.lens (registryId :: StartLifecyclePolicyPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: StartLifecyclePolicyPreviewResponse)
{-# DEPRECATED slpprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The JSON repository policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprsLifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Lude.Maybe Lude.Text)
slpprsLifecyclePolicyText = Lens.lens (lifecyclePolicyText :: StartLifecyclePolicyPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreviewResponse)
{-# DEPRECATED slpprsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprsRepositoryName :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Lude.Maybe Lude.Text)
slpprsRepositoryName = Lens.lens (repositoryName :: StartLifecyclePolicyPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: StartLifecyclePolicyPreviewResponse)
{-# DEPRECATED slpprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprsResponseStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse Lude.Int
slpprsResponseStatus = Lens.lens (responseStatus :: StartLifecyclePolicyPreviewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartLifecyclePolicyPreviewResponse)
{-# DEPRECATED slpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the lifecycle policy for the specified repository.
module Network.AWS.ECR.GetLifecyclePolicy
  ( -- * Creating a request
    GetLifecyclePolicy (..),
    mkGetLifecyclePolicy,

    -- ** Request lenses
    glpRegistryId,
    glpRepositoryName,

    -- * Destructuring the response
    GetLifecyclePolicyResponse (..),
    mkGetLifecyclePolicyResponse,

    -- ** Response lenses
    glprsRegistryId,
    glprsLastEvaluatedAt,
    glprsLifecyclePolicyText,
    glprsRepositoryName,
    glprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The name of the repository.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLifecyclePolicy' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository.
mkGetLifecyclePolicy ::
  -- | 'repositoryName'
  Lude.Text ->
  GetLifecyclePolicy
mkGetLifecyclePolicy pRepositoryName_ =
  GetLifecyclePolicy'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpRegistryId :: Lens.Lens' GetLifecyclePolicy (Lude.Maybe Lude.Text)
glpRegistryId = Lens.lens (registryId :: GetLifecyclePolicy -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: GetLifecyclePolicy)
{-# DEPRECATED glpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpRepositoryName :: Lens.Lens' GetLifecyclePolicy Lude.Text
glpRepositoryName = Lens.lens (repositoryName :: GetLifecyclePolicy -> Lude.Text) (\s a -> s {repositoryName = a} :: GetLifecyclePolicy)
{-# DEPRECATED glpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetLifecyclePolicy where
  type Rs GetLifecyclePolicy = GetLifecyclePolicyResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "lastEvaluatedAt")
            Lude.<*> (x Lude..?> "lifecyclePolicyText")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath GetLifecyclePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLifecyclePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The time stamp of the last time that the lifecycle policy was run.
    lastEvaluatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Lude.Maybe Lude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- * 'registryId' - The registry ID associated with the request.
-- * 'lastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
-- * 'lifecyclePolicyText' - The JSON lifecycle policy text.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkGetLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLifecyclePolicyResponse
mkGetLifecyclePolicyResponse pResponseStatus_ =
  GetLifecyclePolicyResponse'
    { registryId = Lude.Nothing,
      lastEvaluatedAt = Lude.Nothing,
      lifecyclePolicyText = Lude.Nothing,
      repositoryName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsRegistryId :: Lens.Lens' GetLifecyclePolicyResponse (Lude.Maybe Lude.Text)
glprsRegistryId = Lens.lens (registryId :: GetLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: GetLifecyclePolicyResponse)
{-# DEPRECATED glprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The time stamp of the last time that the lifecycle policy was run.
--
-- /Note:/ Consider using 'lastEvaluatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsLastEvaluatedAt :: Lens.Lens' GetLifecyclePolicyResponse (Lude.Maybe Lude.Timestamp)
glprsLastEvaluatedAt = Lens.lens (lastEvaluatedAt :: GetLifecyclePolicyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastEvaluatedAt = a} :: GetLifecyclePolicyResponse)
{-# DEPRECATED glprsLastEvaluatedAt "Use generic-lens or generic-optics with 'lastEvaluatedAt' instead." #-}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsLifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyResponse (Lude.Maybe Lude.Text)
glprsLifecyclePolicyText = Lens.lens (lifecyclePolicyText :: GetLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {lifecyclePolicyText = a} :: GetLifecyclePolicyResponse)
{-# DEPRECATED glprsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsRepositoryName :: Lens.Lens' GetLifecyclePolicyResponse (Lude.Maybe Lude.Text)
glprsRepositoryName = Lens.lens (repositoryName :: GetLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: GetLifecyclePolicyResponse)
{-# DEPRECATED glprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsResponseStatus :: Lens.Lens' GetLifecyclePolicyResponse Lude.Int
glprsResponseStatus = Lens.lens (responseStatus :: GetLifecyclePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLifecyclePolicyResponse)
{-# DEPRECATED glprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

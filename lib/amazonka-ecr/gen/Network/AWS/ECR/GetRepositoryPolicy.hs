{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetRepositoryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the repository policy for the specified repository.
module Network.AWS.ECR.GetRepositoryPolicy
  ( -- * Creating a request
    GetRepositoryPolicy (..),
    mkGetRepositoryPolicy,

    -- ** Request lenses
    grpRegistryId,
    grpRepositoryName,

    -- * Destructuring the response
    GetRepositoryPolicyResponse (..),
    mkGetRepositoryPolicyResponse,

    -- ** Response lenses
    grprsRegistryId,
    grprsRepositoryName,
    grprsPolicyText,
    grprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRepositoryPolicy' smart constructor.
data GetRepositoryPolicy = GetRepositoryPolicy'
  { -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The name of the repository with the policy to retrieve.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRepositoryPolicy' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository with the policy to retrieve.
mkGetRepositoryPolicy ::
  -- | 'repositoryName'
  Lude.Text ->
  GetRepositoryPolicy
mkGetRepositoryPolicy pRepositoryName_ =
  GetRepositoryPolicy'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRegistryId :: Lens.Lens' GetRepositoryPolicy (Lude.Maybe Lude.Text)
grpRegistryId = Lens.lens (registryId :: GetRepositoryPolicy -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: GetRepositoryPolicy)
{-# DEPRECATED grpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository with the policy to retrieve.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRepositoryName :: Lens.Lens' GetRepositoryPolicy Lude.Text
grpRepositoryName = Lens.lens (repositoryName :: GetRepositoryPolicy -> Lude.Text) (\s a -> s {repositoryName = a} :: GetRepositoryPolicy)
{-# DEPRECATED grpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetRepositoryPolicy where
  type Rs GetRepositoryPolicy = GetRepositoryPolicyResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRepositoryPolicyResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "policyText")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRepositoryPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetRepositoryPolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRepositoryPolicy where
  toJSON GetRepositoryPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath GetRepositoryPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRepositoryPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRepositoryPolicyResponse' smart constructor.
data GetRepositoryPolicyResponse = GetRepositoryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The JSON repository policy text associated with the repository.
    policyText :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRepositoryPolicyResponse' with the minimum fields required to make a request.
--
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'policyText' - The JSON repository policy text associated with the repository.
-- * 'responseStatus' - The response status code.
mkGetRepositoryPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRepositoryPolicyResponse
mkGetRepositoryPolicyResponse pResponseStatus_ =
  GetRepositoryPolicyResponse'
    { registryId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      policyText = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsRegistryId :: Lens.Lens' GetRepositoryPolicyResponse (Lude.Maybe Lude.Text)
grprsRegistryId = Lens.lens (registryId :: GetRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: GetRepositoryPolicyResponse)
{-# DEPRECATED grprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsRepositoryName :: Lens.Lens' GetRepositoryPolicyResponse (Lude.Maybe Lude.Text)
grprsRepositoryName = Lens.lens (repositoryName :: GetRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: GetRepositoryPolicyResponse)
{-# DEPRECATED grprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON repository policy text associated with the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsPolicyText :: Lens.Lens' GetRepositoryPolicyResponse (Lude.Maybe Lude.Text)
grprsPolicyText = Lens.lens (policyText :: GetRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyText = a} :: GetRepositoryPolicyResponse)
{-# DEPRECATED grprsPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsResponseStatus :: Lens.Lens' GetRepositoryPolicyResponse Lude.Int
grprsResponseStatus = Lens.lens (responseStatus :: GetRepositoryPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRepositoryPolicyResponse)
{-# DEPRECATED grprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.SetRepositoryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a repository policy to the specified repository to control access permissions. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policies.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.SetRepositoryPolicy
  ( -- * Creating a request
    SetRepositoryPolicy (..),
    mkSetRepositoryPolicy,

    -- ** Request lenses
    srpForce,
    srpRegistryId,
    srpRepositoryName,
    srpPolicyText,

    -- * Destructuring the response
    SetRepositoryPolicyResponse (..),
    mkSetRepositoryPolicyResponse,

    -- ** Response lenses
    srprsRegistryId,
    srprsRepositoryName,
    srprsPolicyText,
    srprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetRepositoryPolicy' smart constructor.
data SetRepositoryPolicy = SetRepositoryPolicy'
  { -- | If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
    force :: Lude.Maybe Lude.Bool,
    -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The name of the repository to receive the policy.
    repositoryName :: Lude.Text,
    -- | The JSON repository policy text to apply to the repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
    policyText :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetRepositoryPolicy' with the minimum fields required to make a request.
--
-- * 'force' - If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository to receive the policy.
-- * 'policyText' - The JSON repository policy text to apply to the repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
mkSetRepositoryPolicy ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'policyText'
  Lude.Text ->
  SetRepositoryPolicy
mkSetRepositoryPolicy pRepositoryName_ pPolicyText_ =
  SetRepositoryPolicy'
    { force = Lude.Nothing,
      registryId = Lude.Nothing,
      repositoryName = pRepositoryName_,
      policyText = pPolicyText_
    }

-- | If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpForce :: Lens.Lens' SetRepositoryPolicy (Lude.Maybe Lude.Bool)
srpForce = Lens.lens (force :: SetRepositoryPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: SetRepositoryPolicy)
{-# DEPRECATED srpForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRegistryId :: Lens.Lens' SetRepositoryPolicy (Lude.Maybe Lude.Text)
srpRegistryId = Lens.lens (registryId :: SetRepositoryPolicy -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: SetRepositoryPolicy)
{-# DEPRECATED srpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository to receive the policy.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRepositoryName :: Lens.Lens' SetRepositoryPolicy Lude.Text
srpRepositoryName = Lens.lens (repositoryName :: SetRepositoryPolicy -> Lude.Text) (\s a -> s {repositoryName = a} :: SetRepositoryPolicy)
{-# DEPRECATED srpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON repository policy text to apply to the repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpPolicyText :: Lens.Lens' SetRepositoryPolicy Lude.Text
srpPolicyText = Lens.lens (policyText :: SetRepositoryPolicy -> Lude.Text) (\s a -> s {policyText = a} :: SetRepositoryPolicy)
{-# DEPRECATED srpPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

instance Lude.AWSRequest SetRepositoryPolicy where
  type Rs SetRepositoryPolicy = SetRepositoryPolicyResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          SetRepositoryPolicyResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "policyText")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetRepositoryPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.SetRepositoryPolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetRepositoryPolicy where
  toJSON SetRepositoryPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("force" Lude..=) Lude.<$> force,
            ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("policyText" Lude..= policyText)
          ]
      )

instance Lude.ToPath SetRepositoryPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery SetRepositoryPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetRepositoryPolicyResponse' smart constructor.
data SetRepositoryPolicyResponse = SetRepositoryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The JSON repository policy text applied to the repository.
    policyText :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetRepositoryPolicyResponse' with the minimum fields required to make a request.
--
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'policyText' - The JSON repository policy text applied to the repository.
-- * 'responseStatus' - The response status code.
mkSetRepositoryPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetRepositoryPolicyResponse
mkSetRepositoryPolicyResponse pResponseStatus_ =
  SetRepositoryPolicyResponse'
    { registryId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      policyText = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprsRegistryId :: Lens.Lens' SetRepositoryPolicyResponse (Lude.Maybe Lude.Text)
srprsRegistryId = Lens.lens (registryId :: SetRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: SetRepositoryPolicyResponse)
{-# DEPRECATED srprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprsRepositoryName :: Lens.Lens' SetRepositoryPolicyResponse (Lude.Maybe Lude.Text)
srprsRepositoryName = Lens.lens (repositoryName :: SetRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: SetRepositoryPolicyResponse)
{-# DEPRECATED srprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON repository policy text applied to the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprsPolicyText :: Lens.Lens' SetRepositoryPolicyResponse (Lude.Maybe Lude.Text)
srprsPolicyText = Lens.lens (policyText :: SetRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyText = a} :: SetRepositoryPolicyResponse)
{-# DEPRECATED srprsPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprsResponseStatus :: Lens.Lens' SetRepositoryPolicyResponse Lude.Int
srprsResponseStatus = Lens.lens (responseStatus :: SetRepositoryPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetRepositoryPolicyResponse)
{-# DEPRECATED srprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

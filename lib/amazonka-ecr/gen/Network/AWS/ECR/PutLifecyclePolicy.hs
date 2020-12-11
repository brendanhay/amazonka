{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the lifecycle policy for the specified repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/LifecyclePolicies.html Lifecycle Policy Template> .
module Network.AWS.ECR.PutLifecyclePolicy
  ( -- * Creating a request
    PutLifecyclePolicy (..),
    mkPutLifecyclePolicy,

    -- ** Request lenses
    plpRegistryId,
    plpRepositoryName,
    plpLifecyclePolicyText,

    -- * Destructuring the response
    PutLifecyclePolicyResponse (..),
    mkPutLifecyclePolicyResponse,

    -- ** Response lenses
    plprsRegistryId,
    plprsLifecyclePolicyText,
    plprsRepositoryName,
    plprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { registryId ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    lifecyclePolicyText :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLifecyclePolicy' with the minimum fields required to make a request.
--
-- * 'lifecyclePolicyText' - The JSON repository policy text to apply to the repository.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository to receive the policy.
mkPutLifecyclePolicy ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'lifecyclePolicyText'
  Lude.Text ->
  PutLifecyclePolicy
mkPutLifecyclePolicy pRepositoryName_ pLifecyclePolicyText_ =
  PutLifecyclePolicy'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_,
      lifecyclePolicyText = pLifecyclePolicyText_
    }

-- | The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpRegistryId :: Lens.Lens' PutLifecyclePolicy (Lude.Maybe Lude.Text)
plpRegistryId = Lens.lens (registryId :: PutLifecyclePolicy -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: PutLifecyclePolicy)
{-# DEPRECATED plpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository to receive the policy.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpRepositoryName :: Lens.Lens' PutLifecyclePolicy Lude.Text
plpRepositoryName = Lens.lens (repositoryName :: PutLifecyclePolicy -> Lude.Text) (\s a -> s {repositoryName = a} :: PutLifecyclePolicy)
{-# DEPRECATED plpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON repository policy text to apply to the repository.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpLifecyclePolicyText :: Lens.Lens' PutLifecyclePolicy Lude.Text
plpLifecyclePolicyText = Lens.lens (lifecyclePolicyText :: PutLifecyclePolicy -> Lude.Text) (\s a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicy)
{-# DEPRECATED plpLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

instance Lude.AWSRequest PutLifecyclePolicy where
  type Rs PutLifecyclePolicy = PutLifecyclePolicyResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutLifecyclePolicyResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "lifecyclePolicyText")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutLifecyclePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.PutLifecyclePolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("lifecyclePolicyText" Lude..= lifecyclePolicyText)
          ]
      )

instance Lude.ToPath PutLifecyclePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutLifecyclePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { registryId ::
      Lude.Maybe Lude.Text,
    lifecyclePolicyText ::
      Lude.Maybe Lude.Text,
    repositoryName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PutLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- * 'lifecyclePolicyText' - The JSON repository policy text.
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkPutLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutLifecyclePolicyResponse
mkPutLifecyclePolicyResponse pResponseStatus_ =
  PutLifecyclePolicyResponse'
    { registryId = Lude.Nothing,
      lifecyclePolicyText = Lude.Nothing,
      repositoryName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprsRegistryId :: Lens.Lens' PutLifecyclePolicyResponse (Lude.Maybe Lude.Text)
plprsRegistryId = Lens.lens (registryId :: PutLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: PutLifecyclePolicyResponse)
{-# DEPRECATED plprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The JSON repository policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprsLifecyclePolicyText :: Lens.Lens' PutLifecyclePolicyResponse (Lude.Maybe Lude.Text)
plprsLifecyclePolicyText = Lens.lens (lifecyclePolicyText :: PutLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicyResponse)
{-# DEPRECATED plprsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprsRepositoryName :: Lens.Lens' PutLifecyclePolicyResponse (Lude.Maybe Lude.Text)
plprsRepositoryName = Lens.lens (repositoryName :: PutLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PutLifecyclePolicyResponse)
{-# DEPRECATED plprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprsResponseStatus :: Lens.Lens' PutLifecyclePolicyResponse Lude.Int
plprsResponseStatus = Lens.lens (responseStatus :: PutLifecyclePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutLifecyclePolicyResponse)
{-# DEPRECATED plprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

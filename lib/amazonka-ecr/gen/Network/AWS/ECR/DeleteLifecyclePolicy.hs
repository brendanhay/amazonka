{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle policy associated with the specified repository.
module Network.AWS.ECR.DeleteLifecyclePolicy
  ( -- * Creating a request
    DeleteLifecyclePolicy (..),
    mkDeleteLifecyclePolicy,

    -- ** Request lenses
    dlpRegistryId,
    dlpRepositoryName,

    -- * Destructuring the response
    DeleteLifecyclePolicyResponse (..),
    mkDeleteLifecyclePolicyResponse,

    -- ** Response lenses
    dlprsRegistryId,
    dlprsLastEvaluatedAt,
    dlprsLifecyclePolicyText,
    dlprsRepositoryName,
    dlprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLifecyclePolicy' smart constructor.
data DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { registryId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteLifecyclePolicy' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository.
mkDeleteLifecyclePolicy ::
  -- | 'repositoryName'
  Lude.Text ->
  DeleteLifecyclePolicy
mkDeleteLifecyclePolicy pRepositoryName_ =
  DeleteLifecyclePolicy'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpRegistryId :: Lens.Lens' DeleteLifecyclePolicy (Lude.Maybe Lude.Text)
dlpRegistryId = Lens.lens (registryId :: DeleteLifecyclePolicy -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DeleteLifecyclePolicy)
{-# DEPRECATED dlpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpRepositoryName :: Lens.Lens' DeleteLifecyclePolicy Lude.Text
dlpRepositoryName = Lens.lens (repositoryName :: DeleteLifecyclePolicy -> Lude.Text) (\s a -> s {repositoryName = a} :: DeleteLifecyclePolicy)
{-# DEPRECATED dlpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest DeleteLifecyclePolicy where
  type Rs DeleteLifecyclePolicy = DeleteLifecyclePolicyResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteLifecyclePolicyResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "lastEvaluatedAt")
            Lude.<*> (x Lude..?> "lifecyclePolicyText")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLifecyclePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteLifecyclePolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLifecyclePolicy where
  toJSON DeleteLifecyclePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath DeleteLifecyclePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLifecyclePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLifecyclePolicyResponse' smart constructor.
data DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { registryId ::
      Lude.Maybe Lude.Text,
    lastEvaluatedAt ::
      Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'DeleteLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- * 'lastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
-- * 'lifecyclePolicyText' - The JSON lifecycle policy text.
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkDeleteLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLifecyclePolicyResponse
mkDeleteLifecyclePolicyResponse pResponseStatus_ =
  DeleteLifecyclePolicyResponse'
    { registryId = Lude.Nothing,
      lastEvaluatedAt = Lude.Nothing,
      lifecyclePolicyText = Lude.Nothing,
      repositoryName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprsRegistryId :: Lens.Lens' DeleteLifecyclePolicyResponse (Lude.Maybe Lude.Text)
dlprsRegistryId = Lens.lens (registryId :: DeleteLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DeleteLifecyclePolicyResponse)
{-# DEPRECATED dlprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The time stamp of the last time that the lifecycle policy was run.
--
-- /Note:/ Consider using 'lastEvaluatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprsLastEvaluatedAt :: Lens.Lens' DeleteLifecyclePolicyResponse (Lude.Maybe Lude.Timestamp)
dlprsLastEvaluatedAt = Lens.lens (lastEvaluatedAt :: DeleteLifecyclePolicyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastEvaluatedAt = a} :: DeleteLifecyclePolicyResponse)
{-# DEPRECATED dlprsLastEvaluatedAt "Use generic-lens or generic-optics with 'lastEvaluatedAt' instead." #-}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprsLifecyclePolicyText :: Lens.Lens' DeleteLifecyclePolicyResponse (Lude.Maybe Lude.Text)
dlprsLifecyclePolicyText = Lens.lens (lifecyclePolicyText :: DeleteLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {lifecyclePolicyText = a} :: DeleteLifecyclePolicyResponse)
{-# DEPRECATED dlprsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprsRepositoryName :: Lens.Lens' DeleteLifecyclePolicyResponse (Lude.Maybe Lude.Text)
dlprsRepositoryName = Lens.lens (repositoryName :: DeleteLifecyclePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: DeleteLifecyclePolicyResponse)
{-# DEPRECATED dlprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprsResponseStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Lude.Int
dlprsResponseStatus = Lens.lens (responseStatus :: DeleteLifecyclePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLifecyclePolicyResponse)
{-# DEPRECATED dlprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

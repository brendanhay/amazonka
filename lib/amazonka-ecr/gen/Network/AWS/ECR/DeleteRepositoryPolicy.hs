{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DeleteRepositoryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the repository policy associated with the specified repository.
module Network.AWS.ECR.DeleteRepositoryPolicy
  ( -- * Creating a request
    DeleteRepositoryPolicy (..),
    mkDeleteRepositoryPolicy,

    -- ** Request lenses
    drpRegistryId,
    drpRepositoryName,

    -- * Destructuring the response
    DeleteRepositoryPolicyResponse (..),
    mkDeleteRepositoryPolicyResponse,

    -- ** Response lenses
    drprsRegistryId,
    drprsRepositoryName,
    drprsPolicyText,
    drprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRepositoryPolicy' smart constructor.
data DeleteRepositoryPolicy = DeleteRepositoryPolicy'
  { -- | The AWS account ID associated with the registry that contains the repository policy to delete. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The name of the repository that is associated with the repository policy to delete.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRepositoryPolicy' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository policy to delete. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository that is associated with the repository policy to delete.
mkDeleteRepositoryPolicy ::
  -- | 'repositoryName'
  Lude.Text ->
  DeleteRepositoryPolicy
mkDeleteRepositoryPolicy pRepositoryName_ =
  DeleteRepositoryPolicy'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the repository policy to delete. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRegistryId :: Lens.Lens' DeleteRepositoryPolicy (Lude.Maybe Lude.Text)
drpRegistryId = Lens.lens (registryId :: DeleteRepositoryPolicy -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DeleteRepositoryPolicy)
{-# DEPRECATED drpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository that is associated with the repository policy to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRepositoryName :: Lens.Lens' DeleteRepositoryPolicy Lude.Text
drpRepositoryName = Lens.lens (repositoryName :: DeleteRepositoryPolicy -> Lude.Text) (\s a -> s {repositoryName = a} :: DeleteRepositoryPolicy)
{-# DEPRECATED drpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest DeleteRepositoryPolicy where
  type Rs DeleteRepositoryPolicy = DeleteRepositoryPolicyResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRepositoryPolicyResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "policyText")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRepositoryPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteRepositoryPolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRepositoryPolicy where
  toJSON DeleteRepositoryPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath DeleteRepositoryPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRepositoryPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRepositoryPolicyResponse' smart constructor.
data DeleteRepositoryPolicyResponse = DeleteRepositoryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The JSON repository policy that was deleted from the repository.
    policyText :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRepositoryPolicyResponse' with the minimum fields required to make a request.
--
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'policyText' - The JSON repository policy that was deleted from the repository.
-- * 'responseStatus' - The response status code.
mkDeleteRepositoryPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRepositoryPolicyResponse
mkDeleteRepositoryPolicyResponse pResponseStatus_ =
  DeleteRepositoryPolicyResponse'
    { registryId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      policyText = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsRegistryId :: Lens.Lens' DeleteRepositoryPolicyResponse (Lude.Maybe Lude.Text)
drprsRegistryId = Lens.lens (registryId :: DeleteRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DeleteRepositoryPolicyResponse)
{-# DEPRECATED drprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsRepositoryName :: Lens.Lens' DeleteRepositoryPolicyResponse (Lude.Maybe Lude.Text)
drprsRepositoryName = Lens.lens (repositoryName :: DeleteRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: DeleteRepositoryPolicyResponse)
{-# DEPRECATED drprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON repository policy that was deleted from the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsPolicyText :: Lens.Lens' DeleteRepositoryPolicyResponse (Lude.Maybe Lude.Text)
drprsPolicyText = Lens.lens (policyText :: DeleteRepositoryPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyText = a} :: DeleteRepositoryPolicyResponse)
{-# DEPRECATED drprsPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DeleteRepositoryPolicyResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DeleteRepositoryPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRepositoryPolicyResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

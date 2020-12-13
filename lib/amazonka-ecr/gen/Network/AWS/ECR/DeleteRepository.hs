{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DeleteRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If the repository contains images, you must either delete all images in the repository or use the @force@ option to delete the repository.
module Network.AWS.ECR.DeleteRepository
  ( -- * Creating a request
    DeleteRepository (..),
    mkDeleteRepository,

    -- ** Request lenses
    dForce,
    dRegistryId,
    dRepositoryName,

    -- * Destructuring the response
    DeleteRepositoryResponse (..),
    mkDeleteRepositoryResponse,

    -- ** Response lenses
    drsRepository,
    drsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | If a repository contains images, forces the deletion.
    force :: Lude.Maybe Lude.Bool,
    -- | The AWS account ID associated with the registry that contains the repository to delete. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The name of the repository to delete.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRepository' with the minimum fields required to make a request.
--
-- * 'force' - If a repository contains images, forces the deletion.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository to delete. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository to delete.
mkDeleteRepository ::
  -- | 'repositoryName'
  Lude.Text ->
  DeleteRepository
mkDeleteRepository pRepositoryName_ =
  DeleteRepository'
    { force = Lude.Nothing,
      registryId = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | If a repository contains images, forces the deletion.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dForce :: Lens.Lens' DeleteRepository (Lude.Maybe Lude.Bool)
dForce = Lens.lens (force :: DeleteRepository -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteRepository)
{-# DEPRECATED dForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository to delete. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRegistryId :: Lens.Lens' DeleteRepository (Lude.Maybe Lude.Text)
dRegistryId = Lens.lens (registryId :: DeleteRepository -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DeleteRepository)
{-# DEPRECATED dRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRepositoryName :: Lens.Lens' DeleteRepository Lude.Text
dRepositoryName = Lens.lens (repositoryName :: DeleteRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: DeleteRepository)
{-# DEPRECATED dRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest DeleteRepository where
  type Rs DeleteRepository = DeleteRepositoryResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRepositoryResponse'
            Lude.<$> (x Lude..?> "repository") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteRepository" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRepository where
  toJSON DeleteRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("force" Lude..=) Lude.<$> force,
            ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath DeleteRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRepository where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { -- | The repository that was deleted.
    repository :: Lude.Maybe Repository,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'repository' - The repository that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRepositoryResponse
mkDeleteRepositoryResponse pResponseStatus_ =
  DeleteRepositoryResponse'
    { repository = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The repository that was deleted.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRepository :: Lens.Lens' DeleteRepositoryResponse (Lude.Maybe Repository)
drsRepository = Lens.lens (repository :: DeleteRepositoryResponse -> Lude.Maybe Repository) (\s a -> s {repository = a} :: DeleteRepositoryResponse)
{-# DEPRECATED drsRepository "Use generic-lens or generic-optics with 'repository' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteRepositoryResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRepositoryResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

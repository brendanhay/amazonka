{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DeleteRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If the repository contains images, you must either
-- delete all images in the repository or use the @force@ option to delete
-- the repository.
module Network.AWS.ECR.DeleteRepository
  ( -- * Creating a Request
    DeleteRepository (..),
    newDeleteRepository,

    -- * Request Lenses
    deleteRepository_registryId,
    deleteRepository_force,
    deleteRepository_repositoryName,

    -- * Destructuring the Response
    DeleteRepositoryResponse (..),
    newDeleteRepositoryResponse,

    -- * Response Lenses
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository to delete. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | If a repository contains images, forces the deletion.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name of the repository to delete.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteRepository_registryId' - The AWS account ID associated with the registry that contains the
-- repository to delete. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'force', 'deleteRepository_force' - If a repository contains images, forces the deletion.
--
-- 'repositoryName', 'deleteRepository_repositoryName' - The name of the repository to delete.
newDeleteRepository ::
  -- | 'repositoryName'
  Prelude.Text ->
  DeleteRepository
newDeleteRepository pRepositoryName_ =
  DeleteRepository'
    { registryId = Prelude.Nothing,
      force = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository to delete. If you do not specify a registry, the default
-- registry is assumed.
deleteRepository_registryId :: Lens.Lens' DeleteRepository (Prelude.Maybe Prelude.Text)
deleteRepository_registryId = Lens.lens (\DeleteRepository' {registryId} -> registryId) (\s@DeleteRepository' {} a -> s {registryId = a} :: DeleteRepository)

-- | If a repository contains images, forces the deletion.
deleteRepository_force :: Lens.Lens' DeleteRepository (Prelude.Maybe Prelude.Bool)
deleteRepository_force = Lens.lens (\DeleteRepository' {force} -> force) (\s@DeleteRepository' {} a -> s {force = a} :: DeleteRepository)

-- | The name of the repository to delete.
deleteRepository_repositoryName :: Lens.Lens' DeleteRepository Prelude.Text
deleteRepository_repositoryName = Lens.lens (\DeleteRepository' {repositoryName} -> repositoryName) (\s@DeleteRepository' {} a -> s {repositoryName = a} :: DeleteRepository)

instance Core.AWSRequest DeleteRepository where
  type
    AWSResponse DeleteRepository =
      DeleteRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryResponse'
            Prelude.<$> (x Core..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRepository

instance Prelude.NFData DeleteRepository

instance Core.ToHeaders DeleteRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteRepository where
  toJSON DeleteRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            ("force" Core..=) Prelude.<$> force,
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath DeleteRepository where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { -- | The repository that was deleted.
    repository :: Prelude.Maybe Repository,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repository', 'deleteRepositoryResponse_repository' - The repository that was deleted.
--
-- 'httpStatus', 'deleteRepositoryResponse_httpStatus' - The response's http status code.
newDeleteRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRepositoryResponse
newDeleteRepositoryResponse pHttpStatus_ =
  DeleteRepositoryResponse'
    { repository =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The repository that was deleted.
deleteRepositoryResponse_repository :: Lens.Lens' DeleteRepositoryResponse (Prelude.Maybe Repository)
deleteRepositoryResponse_repository = Lens.lens (\DeleteRepositoryResponse' {repository} -> repository) (\s@DeleteRepositoryResponse' {} a -> s {repository = a} :: DeleteRepositoryResponse)

-- | The response's http status code.
deleteRepositoryResponse_httpStatus :: Lens.Lens' DeleteRepositoryResponse Prelude.Int
deleteRepositoryResponse_httpStatus = Lens.lens (\DeleteRepositoryResponse' {httpStatus} -> httpStatus) (\s@DeleteRepositoryResponse' {} a -> s {httpStatus = a} :: DeleteRepositoryResponse)

instance Prelude.NFData DeleteRepositoryResponse

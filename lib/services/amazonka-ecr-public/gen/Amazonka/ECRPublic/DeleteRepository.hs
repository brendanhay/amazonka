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
-- Module      : Amazonka.ECRPublic.DeleteRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository in a public registry. If the repository contains
-- images, you must either delete all images in the repository or use the
-- @force@ option which deletes all images on your behalf before deleting
-- the repository.
module Amazonka.ECRPublic.DeleteRepository
  ( -- * Creating a Request
    DeleteRepository (..),
    newDeleteRepository,

    -- * Request Lenses
    deleteRepository_force,
    deleteRepository_registryId,
    deleteRepository_repositoryName,

    -- * Destructuring the Response
    DeleteRepositoryResponse (..),
    newDeleteRepositoryResponse,

    -- * Response Lenses
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | If a repository contains images, forces the deletion.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The AWS account ID associated with the public registry that contains the
    -- repository to delete. If you do not specify a registry, the default
    -- public registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
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
-- 'force', 'deleteRepository_force' - If a repository contains images, forces the deletion.
--
-- 'registryId', 'deleteRepository_registryId' - The AWS account ID associated with the public registry that contains the
-- repository to delete. If you do not specify a registry, the default
-- public registry is assumed.
--
-- 'repositoryName', 'deleteRepository_repositoryName' - The name of the repository to delete.
newDeleteRepository ::
  -- | 'repositoryName'
  Prelude.Text ->
  DeleteRepository
newDeleteRepository pRepositoryName_ =
  DeleteRepository'
    { force = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | If a repository contains images, forces the deletion.
deleteRepository_force :: Lens.Lens' DeleteRepository (Prelude.Maybe Prelude.Bool)
deleteRepository_force = Lens.lens (\DeleteRepository' {force} -> force) (\s@DeleteRepository' {} a -> s {force = a} :: DeleteRepository)

-- | The AWS account ID associated with the public registry that contains the
-- repository to delete. If you do not specify a registry, the default
-- public registry is assumed.
deleteRepository_registryId :: Lens.Lens' DeleteRepository (Prelude.Maybe Prelude.Text)
deleteRepository_registryId = Lens.lens (\DeleteRepository' {registryId} -> registryId) (\s@DeleteRepository' {} a -> s {registryId = a} :: DeleteRepository)

-- | The name of the repository to delete.
deleteRepository_repositoryName :: Lens.Lens' DeleteRepository Prelude.Text
deleteRepository_repositoryName = Lens.lens (\DeleteRepository' {repositoryName} -> repositoryName) (\s@DeleteRepository' {} a -> s {repositoryName = a} :: DeleteRepository)

instance Core.AWSRequest DeleteRepository where
  type
    AWSResponse DeleteRepository =
      DeleteRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryResponse'
            Prelude.<$> (x Data..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRepository where
  hashWithSalt _salt DeleteRepository' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData DeleteRepository where
  rnf DeleteRepository' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders DeleteRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SpencerFrontendService.DeleteRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRepository where
  toJSON DeleteRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("force" Data..=) Prelude.<$> force,
            ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath DeleteRepository where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRepository where
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

instance Prelude.NFData DeleteRepositoryResponse where
  rnf DeleteRepositoryResponse' {..} =
    Prelude.rnf repository
      `Prelude.seq` Prelude.rnf httpStatus

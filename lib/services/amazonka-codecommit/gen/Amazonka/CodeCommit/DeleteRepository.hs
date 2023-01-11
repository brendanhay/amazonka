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
-- Module      : Amazonka.CodeCommit.DeleteRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If a specified repository was already deleted, a
-- null repository ID is returned.
--
-- Deleting a repository also deletes all associated objects and metadata.
-- After a repository is deleted, all future push calls to the deleted
-- repository fail.
module Amazonka.CodeCommit.DeleteRepository
  ( -- * Creating a Request
    DeleteRepository (..),
    newDeleteRepository,

    -- * Request Lenses
    deleteRepository_repositoryName,

    -- * Destructuring the Response
    DeleteRepositoryResponse (..),
    newDeleteRepositoryResponse,

    -- * Response Lenses
    deleteRepositoryResponse_repositoryId,
    deleteRepositoryResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a delete repository operation.
--
-- /See:/ 'newDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | The name of the repository to delete.
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
-- 'repositoryName', 'deleteRepository_repositoryName' - The name of the repository to delete.
newDeleteRepository ::
  -- | 'repositoryName'
  Prelude.Text ->
  DeleteRepository
newDeleteRepository pRepositoryName_ =
  DeleteRepository'
    { repositoryName =
        pRepositoryName_
    }

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
            Prelude.<$> (x Data..?> "repositoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRepository where
  hashWithSalt _salt DeleteRepository' {..} =
    _salt `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData DeleteRepository where
  rnf DeleteRepository' {..} =
    Prelude.rnf repositoryName

instance Data.ToHeaders DeleteRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.DeleteRepository" ::
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
          [ Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath DeleteRepository where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRepository where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a delete repository operation.
--
-- /See:/ 'newDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { -- | The ID of the repository that was deleted.
    repositoryId :: Prelude.Maybe Prelude.Text,
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
-- 'repositoryId', 'deleteRepositoryResponse_repositoryId' - The ID of the repository that was deleted.
--
-- 'httpStatus', 'deleteRepositoryResponse_httpStatus' - The response's http status code.
newDeleteRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRepositoryResponse
newDeleteRepositoryResponse pHttpStatus_ =
  DeleteRepositoryResponse'
    { repositoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the repository that was deleted.
deleteRepositoryResponse_repositoryId :: Lens.Lens' DeleteRepositoryResponse (Prelude.Maybe Prelude.Text)
deleteRepositoryResponse_repositoryId = Lens.lens (\DeleteRepositoryResponse' {repositoryId} -> repositoryId) (\s@DeleteRepositoryResponse' {} a -> s {repositoryId = a} :: DeleteRepositoryResponse)

-- | The response's http status code.
deleteRepositoryResponse_httpStatus :: Lens.Lens' DeleteRepositoryResponse Prelude.Int
deleteRepositoryResponse_httpStatus = Lens.lens (\DeleteRepositoryResponse' {httpStatus} -> httpStatus) (\s@DeleteRepositoryResponse' {} a -> s {httpStatus = a} :: DeleteRepositoryResponse)

instance Prelude.NFData DeleteRepositoryResponse where
  rnf DeleteRepositoryResponse' {..} =
    Prelude.rnf repositoryId
      `Prelude.seq` Prelude.rnf httpStatus

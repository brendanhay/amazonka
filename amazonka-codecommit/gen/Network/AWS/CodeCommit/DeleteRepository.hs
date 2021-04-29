{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.DeleteRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CodeCommit.DeleteRepository
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a delete repository operation.
--
-- /See:/ 'newDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | The name of the repository to delete.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteRepository where
  type Rs DeleteRepository = DeleteRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryResponse'
            Prelude.<$> (x Prelude..?> "repositoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRepository

instance Prelude.NFData DeleteRepository

instance Prelude.ToHeaders DeleteRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.DeleteRepository" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRepository where
  toJSON DeleteRepository' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath DeleteRepository where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRepository where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteRepositoryResponse

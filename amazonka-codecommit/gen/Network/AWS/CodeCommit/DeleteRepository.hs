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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a delete repository operation.
--
-- /See:/ 'newDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | The name of the repository to delete.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteRepository
newDeleteRepository pRepositoryName_ =
  DeleteRepository'
    { repositoryName =
        pRepositoryName_
    }

-- | The name of the repository to delete.
deleteRepository_repositoryName :: Lens.Lens' DeleteRepository Core.Text
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
            Core.<$> (x Core..?> "repositoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRepository

instance Core.NFData DeleteRepository

instance Core.ToHeaders DeleteRepository where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.DeleteRepository" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRepository where
  toJSON DeleteRepository' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath DeleteRepository where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRepository where
  toQuery = Core.const Core.mempty

-- | Represents the output of a delete repository operation.
--
-- /See:/ 'newDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { -- | The ID of the repository that was deleted.
    repositoryId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteRepositoryResponse
newDeleteRepositoryResponse pHttpStatus_ =
  DeleteRepositoryResponse'
    { repositoryId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the repository that was deleted.
deleteRepositoryResponse_repositoryId :: Lens.Lens' DeleteRepositoryResponse (Core.Maybe Core.Text)
deleteRepositoryResponse_repositoryId = Lens.lens (\DeleteRepositoryResponse' {repositoryId} -> repositoryId) (\s@DeleteRepositoryResponse' {} a -> s {repositoryId = a} :: DeleteRepositoryResponse)

-- | The response's http status code.
deleteRepositoryResponse_httpStatus :: Lens.Lens' DeleteRepositoryResponse Core.Int
deleteRepositoryResponse_httpStatus = Lens.lens (\DeleteRepositoryResponse' {httpStatus} -> httpStatus) (\s@DeleteRepositoryResponse' {} a -> s {httpStatus = a} :: DeleteRepositoryResponse)

instance Core.NFData DeleteRepositoryResponse

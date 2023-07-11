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
-- Module      : Amazonka.MediaStore.DeleteContainer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified container. Before you make a @DeleteContainer@
-- request, delete any objects in the container or in any folders in the
-- container. You can delete only empty containers.
module Amazonka.MediaStore.DeleteContainer
  ( -- * Creating a Request
    DeleteContainer (..),
    newDeleteContainer,

    -- * Request Lenses
    deleteContainer_containerName,

    -- * Destructuring the Response
    DeleteContainerResponse (..),
    newDeleteContainerResponse,

    -- * Response Lenses
    deleteContainerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContainer' smart constructor.
data DeleteContainer = DeleteContainer'
  { -- | The name of the container to delete.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'deleteContainer_containerName' - The name of the container to delete.
newDeleteContainer ::
  -- | 'containerName'
  Prelude.Text ->
  DeleteContainer
newDeleteContainer pContainerName_ =
  DeleteContainer' {containerName = pContainerName_}

-- | The name of the container to delete.
deleteContainer_containerName :: Lens.Lens' DeleteContainer Prelude.Text
deleteContainer_containerName = Lens.lens (\DeleteContainer' {containerName} -> containerName) (\s@DeleteContainer' {} a -> s {containerName = a} :: DeleteContainer)

instance Core.AWSRequest DeleteContainer where
  type
    AWSResponse DeleteContainer =
      DeleteContainerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContainer where
  hashWithSalt _salt DeleteContainer' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData DeleteContainer where
  rnf DeleteContainer' {..} = Prelude.rnf containerName

instance Data.ToHeaders DeleteContainer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.DeleteContainer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteContainer where
  toJSON DeleteContainer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName)
          ]
      )

instance Data.ToPath DeleteContainer where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteContainer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContainerResponse' smart constructor.
data DeleteContainerResponse = DeleteContainerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContainerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContainerResponse_httpStatus' - The response's http status code.
newDeleteContainerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContainerResponse
newDeleteContainerResponse pHttpStatus_ =
  DeleteContainerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteContainerResponse_httpStatus :: Lens.Lens' DeleteContainerResponse Prelude.Int
deleteContainerResponse_httpStatus = Lens.lens (\DeleteContainerResponse' {httpStatus} -> httpStatus) (\s@DeleteContainerResponse' {} a -> s {httpStatus = a} :: DeleteContainerResponse)

instance Prelude.NFData DeleteContainerResponse where
  rnf DeleteContainerResponse' {..} =
    Prelude.rnf httpStatus

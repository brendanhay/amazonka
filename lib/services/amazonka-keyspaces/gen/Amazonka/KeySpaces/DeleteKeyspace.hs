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
-- Module      : Amazonka.KeySpaces.DeleteKeyspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteKeyspace@ operation deletes a keyspace and all of its tables.
module Amazonka.KeySpaces.DeleteKeyspace
  ( -- * Creating a Request
    DeleteKeyspace (..),
    newDeleteKeyspace,

    -- * Request Lenses
    deleteKeyspace_keyspaceName,

    -- * Destructuring the Response
    DeleteKeyspaceResponse (..),
    newDeleteKeyspaceResponse,

    -- * Response Lenses
    deleteKeyspaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKeyspace' smart constructor.
data DeleteKeyspace = DeleteKeyspace'
  { -- | The name of the keyspace to be deleted.
    keyspaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyspaceName', 'deleteKeyspace_keyspaceName' - The name of the keyspace to be deleted.
newDeleteKeyspace ::
  -- | 'keyspaceName'
  Prelude.Text ->
  DeleteKeyspace
newDeleteKeyspace pKeyspaceName_ =
  DeleteKeyspace' {keyspaceName = pKeyspaceName_}

-- | The name of the keyspace to be deleted.
deleteKeyspace_keyspaceName :: Lens.Lens' DeleteKeyspace Prelude.Text
deleteKeyspace_keyspaceName = Lens.lens (\DeleteKeyspace' {keyspaceName} -> keyspaceName) (\s@DeleteKeyspace' {} a -> s {keyspaceName = a} :: DeleteKeyspace)

instance Core.AWSRequest DeleteKeyspace where
  type
    AWSResponse DeleteKeyspace =
      DeleteKeyspaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteKeyspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKeyspace where
  hashWithSalt _salt DeleteKeyspace' {..} =
    _salt `Prelude.hashWithSalt` keyspaceName

instance Prelude.NFData DeleteKeyspace where
  rnf DeleteKeyspace' {..} = Prelude.rnf keyspaceName

instance Data.ToHeaders DeleteKeyspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KeyspacesService.DeleteKeyspace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteKeyspace where
  toJSON DeleteKeyspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("keyspaceName" Data..= keyspaceName)]
      )

instance Data.ToPath DeleteKeyspace where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteKeyspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKeyspaceResponse' smart constructor.
data DeleteKeyspaceResponse = DeleteKeyspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKeyspaceResponse_httpStatus' - The response's http status code.
newDeleteKeyspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKeyspaceResponse
newDeleteKeyspaceResponse pHttpStatus_ =
  DeleteKeyspaceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteKeyspaceResponse_httpStatus :: Lens.Lens' DeleteKeyspaceResponse Prelude.Int
deleteKeyspaceResponse_httpStatus = Lens.lens (\DeleteKeyspaceResponse' {httpStatus} -> httpStatus) (\s@DeleteKeyspaceResponse' {} a -> s {httpStatus = a} :: DeleteKeyspaceResponse)

instance Prelude.NFData DeleteKeyspaceResponse where
  rnf DeleteKeyspaceResponse' {..} =
    Prelude.rnf httpStatus

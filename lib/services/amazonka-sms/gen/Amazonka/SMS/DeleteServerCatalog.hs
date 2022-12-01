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
-- Module      : Amazonka.SMS.DeleteServerCatalog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all servers from your server catalog.
module Amazonka.SMS.DeleteServerCatalog
  ( -- * Creating a Request
    DeleteServerCatalog (..),
    newDeleteServerCatalog,

    -- * Destructuring the Response
    DeleteServerCatalogResponse (..),
    newDeleteServerCatalogResponse,

    -- * Response Lenses
    deleteServerCatalogResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newDeleteServerCatalog' smart constructor.
data DeleteServerCatalog = DeleteServerCatalog'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServerCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteServerCatalog ::
  DeleteServerCatalog
newDeleteServerCatalog = DeleteServerCatalog'

instance Core.AWSRequest DeleteServerCatalog where
  type
    AWSResponse DeleteServerCatalog =
      DeleteServerCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteServerCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteServerCatalog where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeleteServerCatalog where
  rnf _ = ()

instance Core.ToHeaders DeleteServerCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.DeleteServerCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteServerCatalog where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DeleteServerCatalog where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteServerCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServerCatalogResponse' smart constructor.
data DeleteServerCatalogResponse = DeleteServerCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServerCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteServerCatalogResponse_httpStatus' - The response's http status code.
newDeleteServerCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServerCatalogResponse
newDeleteServerCatalogResponse pHttpStatus_ =
  DeleteServerCatalogResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteServerCatalogResponse_httpStatus :: Lens.Lens' DeleteServerCatalogResponse Prelude.Int
deleteServerCatalogResponse_httpStatus = Lens.lens (\DeleteServerCatalogResponse' {httpStatus} -> httpStatus) (\s@DeleteServerCatalogResponse' {} a -> s {httpStatus = a} :: DeleteServerCatalogResponse)

instance Prelude.NFData DeleteServerCatalogResponse where
  rnf DeleteServerCatalogResponse' {..} =
    Prelude.rnf httpStatus

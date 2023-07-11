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
-- Module      : Amazonka.SMS.ImportServerCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gathers a complete list of on-premises servers. Connectors must be
-- installed and monitoring all servers to import.
--
-- This call returns immediately, but might take additional time to
-- retrieve all the servers.
module Amazonka.SMS.ImportServerCatalog
  ( -- * Creating a Request
    ImportServerCatalog (..),
    newImportServerCatalog,

    -- * Destructuring the Response
    ImportServerCatalogResponse (..),
    newImportServerCatalogResponse,

    -- * Response Lenses
    importServerCatalogResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newImportServerCatalog' smart constructor.
data ImportServerCatalog = ImportServerCatalog'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportServerCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newImportServerCatalog ::
  ImportServerCatalog
newImportServerCatalog = ImportServerCatalog'

instance Core.AWSRequest ImportServerCatalog where
  type
    AWSResponse ImportServerCatalog =
      ImportServerCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportServerCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportServerCatalog where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ImportServerCatalog where
  rnf _ = ()

instance Data.ToHeaders ImportServerCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.ImportServerCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportServerCatalog where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ImportServerCatalog where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportServerCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportServerCatalogResponse' smart constructor.
data ImportServerCatalogResponse = ImportServerCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportServerCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importServerCatalogResponse_httpStatus' - The response's http status code.
newImportServerCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportServerCatalogResponse
newImportServerCatalogResponse pHttpStatus_ =
  ImportServerCatalogResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
importServerCatalogResponse_httpStatus :: Lens.Lens' ImportServerCatalogResponse Prelude.Int
importServerCatalogResponse_httpStatus = Lens.lens (\ImportServerCatalogResponse' {httpStatus} -> httpStatus) (\s@ImportServerCatalogResponse' {} a -> s {httpStatus = a} :: ImportServerCatalogResponse)

instance Prelude.NFData ImportServerCatalogResponse where
  rnf ImportServerCatalogResponse' {..} =
    Prelude.rnf httpStatus

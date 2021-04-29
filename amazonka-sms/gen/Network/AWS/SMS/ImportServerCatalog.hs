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
-- Module      : Network.AWS.SMS.ImportServerCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gathers a complete list of on-premises servers. Connectors must be
-- installed and monitoring all servers to import.
--
-- This call returns immediately, but might take additional time to
-- retrieve all the servers.
module Network.AWS.SMS.ImportServerCatalog
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newImportServerCatalog' smart constructor.
data ImportServerCatalog = ImportServerCatalog'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportServerCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newImportServerCatalog ::
  ImportServerCatalog
newImportServerCatalog = ImportServerCatalog'

instance Prelude.AWSRequest ImportServerCatalog where
  type
    Rs ImportServerCatalog =
      ImportServerCatalogResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportServerCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportServerCatalog

instance Prelude.NFData ImportServerCatalog

instance Prelude.ToHeaders ImportServerCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.ImportServerCatalog" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ImportServerCatalog where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath ImportServerCatalog where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ImportServerCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportServerCatalogResponse' smart constructor.
data ImportServerCatalogResponse = ImportServerCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ImportServerCatalogResponse

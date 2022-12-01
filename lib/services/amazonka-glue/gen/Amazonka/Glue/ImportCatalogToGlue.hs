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
-- Module      : Amazonka.Glue.ImportCatalogToGlue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing Amazon Athena Data Catalog to Glue.
module Amazonka.Glue.ImportCatalogToGlue
  ( -- * Creating a Request
    ImportCatalogToGlue (..),
    newImportCatalogToGlue,

    -- * Request Lenses
    importCatalogToGlue_catalogId,

    -- * Destructuring the Response
    ImportCatalogToGlueResponse (..),
    newImportCatalogToGlueResponse,

    -- * Response Lenses
    importCatalogToGlueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportCatalogToGlue' smart constructor.
data ImportCatalogToGlue = ImportCatalogToGlue'
  { -- | The ID of the catalog to import. Currently, this should be the Amazon
    -- Web Services account ID.
    catalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportCatalogToGlue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'importCatalogToGlue_catalogId' - The ID of the catalog to import. Currently, this should be the Amazon
-- Web Services account ID.
newImportCatalogToGlue ::
  ImportCatalogToGlue
newImportCatalogToGlue =
  ImportCatalogToGlue' {catalogId = Prelude.Nothing}

-- | The ID of the catalog to import. Currently, this should be the Amazon
-- Web Services account ID.
importCatalogToGlue_catalogId :: Lens.Lens' ImportCatalogToGlue (Prelude.Maybe Prelude.Text)
importCatalogToGlue_catalogId = Lens.lens (\ImportCatalogToGlue' {catalogId} -> catalogId) (\s@ImportCatalogToGlue' {} a -> s {catalogId = a} :: ImportCatalogToGlue)

instance Core.AWSRequest ImportCatalogToGlue where
  type
    AWSResponse ImportCatalogToGlue =
      ImportCatalogToGlueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportCatalogToGlueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportCatalogToGlue where
  hashWithSalt _salt ImportCatalogToGlue' {..} =
    _salt `Prelude.hashWithSalt` catalogId

instance Prelude.NFData ImportCatalogToGlue where
  rnf ImportCatalogToGlue' {..} = Prelude.rnf catalogId

instance Core.ToHeaders ImportCatalogToGlue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.ImportCatalogToGlue" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ImportCatalogToGlue where
  toJSON ImportCatalogToGlue' {..} =
    Core.object
      ( Prelude.catMaybes
          [("CatalogId" Core..=) Prelude.<$> catalogId]
      )

instance Core.ToPath ImportCatalogToGlue where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportCatalogToGlue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportCatalogToGlueResponse' smart constructor.
data ImportCatalogToGlueResponse = ImportCatalogToGlueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportCatalogToGlueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importCatalogToGlueResponse_httpStatus' - The response's http status code.
newImportCatalogToGlueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportCatalogToGlueResponse
newImportCatalogToGlueResponse pHttpStatus_ =
  ImportCatalogToGlueResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
importCatalogToGlueResponse_httpStatus :: Lens.Lens' ImportCatalogToGlueResponse Prelude.Int
importCatalogToGlueResponse_httpStatus = Lens.lens (\ImportCatalogToGlueResponse' {httpStatus} -> httpStatus) (\s@ImportCatalogToGlueResponse' {} a -> s {httpStatus = a} :: ImportCatalogToGlueResponse)

instance Prelude.NFData ImportCatalogToGlueResponse where
  rnf ImportCatalogToGlueResponse' {..} =
    Prelude.rnf httpStatus

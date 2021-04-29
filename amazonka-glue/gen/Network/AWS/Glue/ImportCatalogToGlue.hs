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
-- Module      : Network.AWS.Glue.ImportCatalogToGlue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing Amazon Athena Data Catalog to AWS Glue
module Network.AWS.Glue.ImportCatalogToGlue
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportCatalogToGlue' smart constructor.
data ImportCatalogToGlue = ImportCatalogToGlue'
  { -- | The ID of the catalog to import. Currently, this should be the AWS
    -- account ID.
    catalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportCatalogToGlue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'importCatalogToGlue_catalogId' - The ID of the catalog to import. Currently, this should be the AWS
-- account ID.
newImportCatalogToGlue ::
  ImportCatalogToGlue
newImportCatalogToGlue =
  ImportCatalogToGlue' {catalogId = Prelude.Nothing}

-- | The ID of the catalog to import. Currently, this should be the AWS
-- account ID.
importCatalogToGlue_catalogId :: Lens.Lens' ImportCatalogToGlue (Prelude.Maybe Prelude.Text)
importCatalogToGlue_catalogId = Lens.lens (\ImportCatalogToGlue' {catalogId} -> catalogId) (\s@ImportCatalogToGlue' {} a -> s {catalogId = a} :: ImportCatalogToGlue)

instance Prelude.AWSRequest ImportCatalogToGlue where
  type
    Rs ImportCatalogToGlue =
      ImportCatalogToGlueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportCatalogToGlueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportCatalogToGlue

instance Prelude.NFData ImportCatalogToGlue

instance Prelude.ToHeaders ImportCatalogToGlue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.ImportCatalogToGlue" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ImportCatalogToGlue where
  toJSON ImportCatalogToGlue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("CatalogId" Prelude..=) Prelude.<$> catalogId]
      )

instance Prelude.ToPath ImportCatalogToGlue where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ImportCatalogToGlue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportCatalogToGlueResponse' smart constructor.
data ImportCatalogToGlueResponse = ImportCatalogToGlueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ImportCatalogToGlueResponse

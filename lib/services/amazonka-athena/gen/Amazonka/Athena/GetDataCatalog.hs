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
-- Module      : Amazonka.Athena.GetDataCatalog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified data catalog.
module Amazonka.Athena.GetDataCatalog
  ( -- * Creating a Request
    GetDataCatalog (..),
    newGetDataCatalog,

    -- * Request Lenses
    getDataCatalog_name,

    -- * Destructuring the Response
    GetDataCatalogResponse (..),
    newGetDataCatalogResponse,

    -- * Response Lenses
    getDataCatalogResponse_dataCatalog,
    getDataCatalogResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataCatalog' smart constructor.
data GetDataCatalog = GetDataCatalog'
  { -- | The name of the data catalog to return.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getDataCatalog_name' - The name of the data catalog to return.
newGetDataCatalog ::
  -- | 'name'
  Prelude.Text ->
  GetDataCatalog
newGetDataCatalog pName_ =
  GetDataCatalog' {name = pName_}

-- | The name of the data catalog to return.
getDataCatalog_name :: Lens.Lens' GetDataCatalog Prelude.Text
getDataCatalog_name = Lens.lens (\GetDataCatalog' {name} -> name) (\s@GetDataCatalog' {} a -> s {name = a} :: GetDataCatalog)

instance Core.AWSRequest GetDataCatalog where
  type
    AWSResponse GetDataCatalog =
      GetDataCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataCatalogResponse'
            Prelude.<$> (x Data..?> "DataCatalog")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataCatalog where
  hashWithSalt _salt GetDataCatalog' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetDataCatalog where
  rnf GetDataCatalog' {..} = Prelude.rnf name

instance Data.ToHeaders GetDataCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetDataCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataCatalog where
  toJSON GetDataCatalog' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetDataCatalog where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDataCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataCatalogResponse' smart constructor.
data GetDataCatalogResponse = GetDataCatalogResponse'
  { -- | The data catalog returned.
    dataCatalog :: Prelude.Maybe DataCatalog,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCatalog', 'getDataCatalogResponse_dataCatalog' - The data catalog returned.
--
-- 'httpStatus', 'getDataCatalogResponse_httpStatus' - The response's http status code.
newGetDataCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataCatalogResponse
newGetDataCatalogResponse pHttpStatus_ =
  GetDataCatalogResponse'
    { dataCatalog =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data catalog returned.
getDataCatalogResponse_dataCatalog :: Lens.Lens' GetDataCatalogResponse (Prelude.Maybe DataCatalog)
getDataCatalogResponse_dataCatalog = Lens.lens (\GetDataCatalogResponse' {dataCatalog} -> dataCatalog) (\s@GetDataCatalogResponse' {} a -> s {dataCatalog = a} :: GetDataCatalogResponse)

-- | The response's http status code.
getDataCatalogResponse_httpStatus :: Lens.Lens' GetDataCatalogResponse Prelude.Int
getDataCatalogResponse_httpStatus = Lens.lens (\GetDataCatalogResponse' {httpStatus} -> httpStatus) (\s@GetDataCatalogResponse' {} a -> s {httpStatus = a} :: GetDataCatalogResponse)

instance Prelude.NFData GetDataCatalogResponse where
  rnf GetDataCatalogResponse' {..} =
    Prelude.rnf dataCatalog
      `Prelude.seq` Prelude.rnf httpStatus

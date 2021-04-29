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
-- Module      : Network.AWS.Athena.GetDataCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified data catalog.
module Network.AWS.Athena.GetDataCatalog
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

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDataCatalog' smart constructor.
data GetDataCatalog = GetDataCatalog'
  { -- | The name of the data catalog to return.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetDataCatalog where
  type Rs GetDataCatalog = GetDataCatalogResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataCatalogResponse'
            Prelude.<$> (x Prelude..?> "DataCatalog")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataCatalog

instance Prelude.NFData GetDataCatalog

instance Prelude.ToHeaders GetDataCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonAthena.GetDataCatalog" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetDataCatalog where
  toJSON GetDataCatalog' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath GetDataCatalog where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetDataCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataCatalogResponse' smart constructor.
data GetDataCatalogResponse = GetDataCatalogResponse'
  { -- | The data catalog returned.
    dataCatalog :: Prelude.Maybe DataCatalog,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetDataCatalogResponse

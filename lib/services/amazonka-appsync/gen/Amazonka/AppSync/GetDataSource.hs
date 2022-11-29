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
-- Module      : Amazonka.AppSync.GetDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @DataSource@ object.
module Amazonka.AppSync.GetDataSource
  ( -- * Creating a Request
    GetDataSource (..),
    newGetDataSource,

    -- * Request Lenses
    getDataSource_apiId,
    getDataSource_name,

    -- * Destructuring the Response
    GetDataSourceResponse (..),
    newGetDataSourceResponse,

    -- * Response Lenses
    getDataSourceResponse_dataSource,
    getDataSourceResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataSource' smart constructor.
data GetDataSource = GetDataSource'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The name of the data source.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getDataSource_apiId' - The API ID.
--
-- 'name', 'getDataSource_name' - The name of the data source.
newGetDataSource ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetDataSource
newGetDataSource pApiId_ pName_ =
  GetDataSource' {apiId = pApiId_, name = pName_}

-- | The API ID.
getDataSource_apiId :: Lens.Lens' GetDataSource Prelude.Text
getDataSource_apiId = Lens.lens (\GetDataSource' {apiId} -> apiId) (\s@GetDataSource' {} a -> s {apiId = a} :: GetDataSource)

-- | The name of the data source.
getDataSource_name :: Lens.Lens' GetDataSource Prelude.Text
getDataSource_name = Lens.lens (\GetDataSource' {name} -> name) (\s@GetDataSource' {} a -> s {name = a} :: GetDataSource)

instance Core.AWSRequest GetDataSource where
  type
    AWSResponse GetDataSource =
      GetDataSourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataSourceResponse'
            Prelude.<$> (x Core..?> "dataSource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataSource where
  hashWithSalt _salt GetDataSource' {..} =
    _salt `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetDataSource where
  rnf GetDataSource' {..} =
    Prelude.rnf apiId `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders GetDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDataSource where
  toPath GetDataSource' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/datasources/",
        Core.toBS name
      ]

instance Core.ToQuery GetDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataSourceResponse' smart constructor.
data GetDataSourceResponse = GetDataSourceResponse'
  { -- | The @DataSource@ object.
    dataSource :: Prelude.Maybe DataSource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'getDataSourceResponse_dataSource' - The @DataSource@ object.
--
-- 'httpStatus', 'getDataSourceResponse_httpStatus' - The response's http status code.
newGetDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataSourceResponse
newGetDataSourceResponse pHttpStatus_ =
  GetDataSourceResponse'
    { dataSource =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DataSource@ object.
getDataSourceResponse_dataSource :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe DataSource)
getDataSourceResponse_dataSource = Lens.lens (\GetDataSourceResponse' {dataSource} -> dataSource) (\s@GetDataSourceResponse' {} a -> s {dataSource = a} :: GetDataSourceResponse)

-- | The response's http status code.
getDataSourceResponse_httpStatus :: Lens.Lens' GetDataSourceResponse Prelude.Int
getDataSourceResponse_httpStatus = Lens.lens (\GetDataSourceResponse' {httpStatus} -> httpStatus) (\s@GetDataSourceResponse' {} a -> s {httpStatus = a} :: GetDataSourceResponse)

instance Prelude.NFData GetDataSourceResponse where
  rnf GetDataSourceResponse' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf httpStatus

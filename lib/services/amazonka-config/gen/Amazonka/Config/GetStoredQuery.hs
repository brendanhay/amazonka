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
-- Module      : Amazonka.Config.GetStoredQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a specific stored query.
module Amazonka.Config.GetStoredQuery
  ( -- * Creating a Request
    GetStoredQuery (..),
    newGetStoredQuery,

    -- * Request Lenses
    getStoredQuery_queryName,

    -- * Destructuring the Response
    GetStoredQueryResponse (..),
    newGetStoredQueryResponse,

    -- * Response Lenses
    getStoredQueryResponse_storedQuery,
    getStoredQueryResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStoredQuery' smart constructor.
data GetStoredQuery = GetStoredQuery'
  { -- | The name of the query.
    queryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStoredQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryName', 'getStoredQuery_queryName' - The name of the query.
newGetStoredQuery ::
  -- | 'queryName'
  Prelude.Text ->
  GetStoredQuery
newGetStoredQuery pQueryName_ =
  GetStoredQuery' {queryName = pQueryName_}

-- | The name of the query.
getStoredQuery_queryName :: Lens.Lens' GetStoredQuery Prelude.Text
getStoredQuery_queryName = Lens.lens (\GetStoredQuery' {queryName} -> queryName) (\s@GetStoredQuery' {} a -> s {queryName = a} :: GetStoredQuery)

instance Core.AWSRequest GetStoredQuery where
  type
    AWSResponse GetStoredQuery =
      GetStoredQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStoredQueryResponse'
            Prelude.<$> (x Data..?> "StoredQuery")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStoredQuery where
  hashWithSalt _salt GetStoredQuery' {..} =
    _salt `Prelude.hashWithSalt` queryName

instance Prelude.NFData GetStoredQuery where
  rnf GetStoredQuery' {..} = Prelude.rnf queryName

instance Data.ToHeaders GetStoredQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetStoredQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetStoredQuery where
  toJSON GetStoredQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("QueryName" Data..= queryName)]
      )

instance Data.ToPath GetStoredQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery GetStoredQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStoredQueryResponse' smart constructor.
data GetStoredQueryResponse = GetStoredQueryResponse'
  { -- | Returns a @StoredQuery@ object.
    storedQuery :: Prelude.Maybe StoredQuery,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStoredQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storedQuery', 'getStoredQueryResponse_storedQuery' - Returns a @StoredQuery@ object.
--
-- 'httpStatus', 'getStoredQueryResponse_httpStatus' - The response's http status code.
newGetStoredQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStoredQueryResponse
newGetStoredQueryResponse pHttpStatus_ =
  GetStoredQueryResponse'
    { storedQuery =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a @StoredQuery@ object.
getStoredQueryResponse_storedQuery :: Lens.Lens' GetStoredQueryResponse (Prelude.Maybe StoredQuery)
getStoredQueryResponse_storedQuery = Lens.lens (\GetStoredQueryResponse' {storedQuery} -> storedQuery) (\s@GetStoredQueryResponse' {} a -> s {storedQuery = a} :: GetStoredQueryResponse)

-- | The response's http status code.
getStoredQueryResponse_httpStatus :: Lens.Lens' GetStoredQueryResponse Prelude.Int
getStoredQueryResponse_httpStatus = Lens.lens (\GetStoredQueryResponse' {httpStatus} -> httpStatus) (\s@GetStoredQueryResponse' {} a -> s {httpStatus = a} :: GetStoredQueryResponse)

instance Prelude.NFData GetStoredQueryResponse where
  rnf GetStoredQueryResponse' {..} =
    Prelude.rnf storedQuery
      `Prelude.seq` Prelude.rnf httpStatus

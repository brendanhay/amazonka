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
-- Module      : Amazonka.Athena.GetNamedQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single query. Requires that you have access
-- to the workgroup in which the query was saved.
module Amazonka.Athena.GetNamedQuery
  ( -- * Creating a Request
    GetNamedQuery (..),
    newGetNamedQuery,

    -- * Request Lenses
    getNamedQuery_namedQueryId,

    -- * Destructuring the Response
    GetNamedQueryResponse (..),
    newGetNamedQueryResponse,

    -- * Response Lenses
    getNamedQueryResponse_namedQuery,
    getNamedQueryResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNamedQuery' smart constructor.
data GetNamedQuery = GetNamedQuery'
  { -- | The unique ID of the query. Use ListNamedQueries to get query IDs.
    namedQueryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNamedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQueryId', 'getNamedQuery_namedQueryId' - The unique ID of the query. Use ListNamedQueries to get query IDs.
newGetNamedQuery ::
  -- | 'namedQueryId'
  Prelude.Text ->
  GetNamedQuery
newGetNamedQuery pNamedQueryId_ =
  GetNamedQuery' {namedQueryId = pNamedQueryId_}

-- | The unique ID of the query. Use ListNamedQueries to get query IDs.
getNamedQuery_namedQueryId :: Lens.Lens' GetNamedQuery Prelude.Text
getNamedQuery_namedQueryId = Lens.lens (\GetNamedQuery' {namedQueryId} -> namedQueryId) (\s@GetNamedQuery' {} a -> s {namedQueryId = a} :: GetNamedQuery)

instance Core.AWSRequest GetNamedQuery where
  type
    AWSResponse GetNamedQuery =
      GetNamedQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNamedQueryResponse'
            Prelude.<$> (x Data..?> "NamedQuery")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNamedQuery where
  hashWithSalt _salt GetNamedQuery' {..} =
    _salt `Prelude.hashWithSalt` namedQueryId

instance Prelude.NFData GetNamedQuery where
  rnf GetNamedQuery' {..} = Prelude.rnf namedQueryId

instance Data.ToHeaders GetNamedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonAthena.GetNamedQuery" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetNamedQuery where
  toJSON GetNamedQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NamedQueryId" Data..= namedQueryId)]
      )

instance Data.ToPath GetNamedQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery GetNamedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNamedQueryResponse' smart constructor.
data GetNamedQueryResponse = GetNamedQueryResponse'
  { -- | Information about the query.
    namedQuery :: Prelude.Maybe NamedQuery,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNamedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQuery', 'getNamedQueryResponse_namedQuery' - Information about the query.
--
-- 'httpStatus', 'getNamedQueryResponse_httpStatus' - The response's http status code.
newGetNamedQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNamedQueryResponse
newGetNamedQueryResponse pHttpStatus_ =
  GetNamedQueryResponse'
    { namedQuery =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the query.
getNamedQueryResponse_namedQuery :: Lens.Lens' GetNamedQueryResponse (Prelude.Maybe NamedQuery)
getNamedQueryResponse_namedQuery = Lens.lens (\GetNamedQueryResponse' {namedQuery} -> namedQuery) (\s@GetNamedQueryResponse' {} a -> s {namedQuery = a} :: GetNamedQueryResponse)

-- | The response's http status code.
getNamedQueryResponse_httpStatus :: Lens.Lens' GetNamedQueryResponse Prelude.Int
getNamedQueryResponse_httpStatus = Lens.lens (\GetNamedQueryResponse' {httpStatus} -> httpStatus) (\s@GetNamedQueryResponse' {} a -> s {httpStatus = a} :: GetNamedQueryResponse)

instance Prelude.NFData GetNamedQueryResponse where
  rnf GetNamedQueryResponse' {..} =
    Prelude.rnf namedQuery
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.Athena.UpdateNamedQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a NamedQuery object. The database or workgroup cannot be
-- updated.
module Amazonka.Athena.UpdateNamedQuery
  ( -- * Creating a Request
    UpdateNamedQuery (..),
    newUpdateNamedQuery,

    -- * Request Lenses
    updateNamedQuery_description,
    updateNamedQuery_namedQueryId,
    updateNamedQuery_name,
    updateNamedQuery_queryString,

    -- * Destructuring the Response
    UpdateNamedQueryResponse (..),
    newUpdateNamedQueryResponse,

    -- * Response Lenses
    updateNamedQueryResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNamedQuery' smart constructor.
data UpdateNamedQuery = UpdateNamedQuery'
  { -- | The query description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (UUID) of the query.
    namedQueryId :: Prelude.Text,
    -- | The name of the query.
    name :: Prelude.Text,
    -- | The contents of the query with all query statements.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNamedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateNamedQuery_description' - The query description.
--
-- 'namedQueryId', 'updateNamedQuery_namedQueryId' - The unique identifier (UUID) of the query.
--
-- 'name', 'updateNamedQuery_name' - The name of the query.
--
-- 'queryString', 'updateNamedQuery_queryString' - The contents of the query with all query statements.
newUpdateNamedQuery ::
  -- | 'namedQueryId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  UpdateNamedQuery
newUpdateNamedQuery
  pNamedQueryId_
  pName_
  pQueryString_ =
    UpdateNamedQuery'
      { description = Prelude.Nothing,
        namedQueryId = pNamedQueryId_,
        name = pName_,
        queryString = pQueryString_
      }

-- | The query description.
updateNamedQuery_description :: Lens.Lens' UpdateNamedQuery (Prelude.Maybe Prelude.Text)
updateNamedQuery_description = Lens.lens (\UpdateNamedQuery' {description} -> description) (\s@UpdateNamedQuery' {} a -> s {description = a} :: UpdateNamedQuery)

-- | The unique identifier (UUID) of the query.
updateNamedQuery_namedQueryId :: Lens.Lens' UpdateNamedQuery Prelude.Text
updateNamedQuery_namedQueryId = Lens.lens (\UpdateNamedQuery' {namedQueryId} -> namedQueryId) (\s@UpdateNamedQuery' {} a -> s {namedQueryId = a} :: UpdateNamedQuery)

-- | The name of the query.
updateNamedQuery_name :: Lens.Lens' UpdateNamedQuery Prelude.Text
updateNamedQuery_name = Lens.lens (\UpdateNamedQuery' {name} -> name) (\s@UpdateNamedQuery' {} a -> s {name = a} :: UpdateNamedQuery)

-- | The contents of the query with all query statements.
updateNamedQuery_queryString :: Lens.Lens' UpdateNamedQuery Prelude.Text
updateNamedQuery_queryString = Lens.lens (\UpdateNamedQuery' {queryString} -> queryString) (\s@UpdateNamedQuery' {} a -> s {queryString = a} :: UpdateNamedQuery)

instance Core.AWSRequest UpdateNamedQuery where
  type
    AWSResponse UpdateNamedQuery =
      UpdateNamedQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNamedQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNamedQuery where
  hashWithSalt _salt UpdateNamedQuery' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` namedQueryId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData UpdateNamedQuery where
  rnf UpdateNamedQuery' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf namedQueryId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders UpdateNamedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.UpdateNamedQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNamedQuery where
  toJSON UpdateNamedQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("NamedQueryId" Data..= namedQueryId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("QueryString" Data..= queryString)
          ]
      )

instance Data.ToPath UpdateNamedQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNamedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNamedQueryResponse' smart constructor.
data UpdateNamedQueryResponse = UpdateNamedQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNamedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNamedQueryResponse_httpStatus' - The response's http status code.
newUpdateNamedQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNamedQueryResponse
newUpdateNamedQueryResponse pHttpStatus_ =
  UpdateNamedQueryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNamedQueryResponse_httpStatus :: Lens.Lens' UpdateNamedQueryResponse Prelude.Int
updateNamedQueryResponse_httpStatus = Lens.lens (\UpdateNamedQueryResponse' {httpStatus} -> httpStatus) (\s@UpdateNamedQueryResponse' {} a -> s {httpStatus = a} :: UpdateNamedQueryResponse)

instance Prelude.NFData UpdateNamedQueryResponse where
  rnf UpdateNamedQueryResponse' {..} =
    Prelude.rnf httpStatus

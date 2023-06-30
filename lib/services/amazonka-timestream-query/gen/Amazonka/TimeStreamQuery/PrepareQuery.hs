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
-- Module      : Amazonka.TimeStreamQuery.PrepareQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A synchronous operation that allows you to submit a query with
-- parameters to be stored by Timestream for later running. Timestream only
-- supports using this operation with the
-- @PrepareQueryRequest$ValidateOnly@ set to @true@.
module Amazonka.TimeStreamQuery.PrepareQuery
  ( -- * Creating a Request
    PrepareQuery (..),
    newPrepareQuery,

    -- * Request Lenses
    prepareQuery_validateOnly,
    prepareQuery_queryString,

    -- * Destructuring the Response
    PrepareQueryResponse (..),
    newPrepareQueryResponse,

    -- * Response Lenses
    prepareQueryResponse_httpStatus,
    prepareQueryResponse_queryString,
    prepareQueryResponse_columns,
    prepareQueryResponse_parameters,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newPrepareQuery' smart constructor.
data PrepareQuery = PrepareQuery'
  { -- | By setting this value to @true@, Timestream will only validate that the
    -- query string is a valid Timestream query, and not store the prepared
    -- query for later use.
    validateOnly :: Prelude.Maybe Prelude.Bool,
    -- | The Timestream query string that you want to use as a prepared
    -- statement. Parameter names can be specified in the query string @\@@
    -- character followed by an identifier.
    queryString :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrepareQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validateOnly', 'prepareQuery_validateOnly' - By setting this value to @true@, Timestream will only validate that the
-- query string is a valid Timestream query, and not store the prepared
-- query for later use.
--
-- 'queryString', 'prepareQuery_queryString' - The Timestream query string that you want to use as a prepared
-- statement. Parameter names can be specified in the query string @\@@
-- character followed by an identifier.
newPrepareQuery ::
  -- | 'queryString'
  Prelude.Text ->
  PrepareQuery
newPrepareQuery pQueryString_ =
  PrepareQuery'
    { validateOnly = Prelude.Nothing,
      queryString = Data._Sensitive Lens.# pQueryString_
    }

-- | By setting this value to @true@, Timestream will only validate that the
-- query string is a valid Timestream query, and not store the prepared
-- query for later use.
prepareQuery_validateOnly :: Lens.Lens' PrepareQuery (Prelude.Maybe Prelude.Bool)
prepareQuery_validateOnly = Lens.lens (\PrepareQuery' {validateOnly} -> validateOnly) (\s@PrepareQuery' {} a -> s {validateOnly = a} :: PrepareQuery)

-- | The Timestream query string that you want to use as a prepared
-- statement. Parameter names can be specified in the query string @\@@
-- character followed by an identifier.
prepareQuery_queryString :: Lens.Lens' PrepareQuery Prelude.Text
prepareQuery_queryString = Lens.lens (\PrepareQuery' {queryString} -> queryString) (\s@PrepareQuery' {} a -> s {queryString = a} :: PrepareQuery) Prelude.. Data._Sensitive

instance Core.AWSRequest PrepareQuery where
  type AWSResponse PrepareQuery = PrepareQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PrepareQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "QueryString")
            Prelude.<*> (x Data..?> "Columns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Parameters" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable PrepareQuery where
  hashWithSalt _salt PrepareQuery' {..} =
    _salt
      `Prelude.hashWithSalt` validateOnly
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData PrepareQuery where
  rnf PrepareQuery' {..} =
    Prelude.rnf validateOnly
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders PrepareQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.PrepareQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PrepareQuery where
  toJSON PrepareQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ValidateOnly" Data..=) Prelude.<$> validateOnly,
            Prelude.Just ("QueryString" Data..= queryString)
          ]
      )

instance Data.ToPath PrepareQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery PrepareQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPrepareQueryResponse' smart constructor.
data PrepareQueryResponse = PrepareQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The query string that you want prepare.
    queryString :: Data.Sensitive Prelude.Text,
    -- | A list of SELECT clause columns of the submitted query string.
    columns :: [SelectColumn],
    -- | A list of parameters used in the submitted query string.
    parameters :: [ParameterMapping]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrepareQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'prepareQueryResponse_httpStatus' - The response's http status code.
--
-- 'queryString', 'prepareQueryResponse_queryString' - The query string that you want prepare.
--
-- 'columns', 'prepareQueryResponse_columns' - A list of SELECT clause columns of the submitted query string.
--
-- 'parameters', 'prepareQueryResponse_parameters' - A list of parameters used in the submitted query string.
newPrepareQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'queryString'
  Prelude.Text ->
  PrepareQueryResponse
newPrepareQueryResponse pHttpStatus_ pQueryString_ =
  PrepareQueryResponse'
    { httpStatus = pHttpStatus_,
      queryString = Data._Sensitive Lens.# pQueryString_,
      columns = Prelude.mempty,
      parameters = Prelude.mempty
    }

-- | The response's http status code.
prepareQueryResponse_httpStatus :: Lens.Lens' PrepareQueryResponse Prelude.Int
prepareQueryResponse_httpStatus = Lens.lens (\PrepareQueryResponse' {httpStatus} -> httpStatus) (\s@PrepareQueryResponse' {} a -> s {httpStatus = a} :: PrepareQueryResponse)

-- | The query string that you want prepare.
prepareQueryResponse_queryString :: Lens.Lens' PrepareQueryResponse Prelude.Text
prepareQueryResponse_queryString = Lens.lens (\PrepareQueryResponse' {queryString} -> queryString) (\s@PrepareQueryResponse' {} a -> s {queryString = a} :: PrepareQueryResponse) Prelude.. Data._Sensitive

-- | A list of SELECT clause columns of the submitted query string.
prepareQueryResponse_columns :: Lens.Lens' PrepareQueryResponse [SelectColumn]
prepareQueryResponse_columns = Lens.lens (\PrepareQueryResponse' {columns} -> columns) (\s@PrepareQueryResponse' {} a -> s {columns = a} :: PrepareQueryResponse) Prelude.. Lens.coerced

-- | A list of parameters used in the submitted query string.
prepareQueryResponse_parameters :: Lens.Lens' PrepareQueryResponse [ParameterMapping]
prepareQueryResponse_parameters = Lens.lens (\PrepareQueryResponse' {parameters} -> parameters) (\s@PrepareQueryResponse' {} a -> s {parameters = a} :: PrepareQueryResponse) Prelude.. Lens.coerced

instance Prelude.NFData PrepareQueryResponse where
  rnf PrepareQueryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf columns
      `Prelude.seq` Prelude.rnf parameters

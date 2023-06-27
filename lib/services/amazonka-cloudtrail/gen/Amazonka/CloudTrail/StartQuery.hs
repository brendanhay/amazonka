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
-- Module      : Amazonka.CloudTrail.StartQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a CloudTrail Lake query. Use the @QueryStatement@ parameter to
-- provide your SQL query, enclosed in single quotation marks. Use the
-- optional @DeliveryS3Uri@ parameter to deliver the query results to an S3
-- bucket.
--
-- @StartQuery@ requires you specify either the @QueryStatement@ parameter,
-- or a @QueryAlias@ and any @QueryParameters@. In the current release, the
-- @QueryAlias@ and @QueryParameters@ parameters are used only for the
-- queries that populate the CloudTrail Lake dashboards.
module Amazonka.CloudTrail.StartQuery
  ( -- * Creating a Request
    StartQuery (..),
    newStartQuery,

    -- * Request Lenses
    startQuery_deliveryS3Uri,
    startQuery_queryAlias,
    startQuery_queryParameters,
    startQuery_queryStatement,

    -- * Destructuring the Response
    StartQueryResponse (..),
    newStartQueryResponse,

    -- * Response Lenses
    startQueryResponse_queryId,
    startQueryResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartQuery' smart constructor.
data StartQuery = StartQuery'
  { -- | The URI for the S3 bucket where CloudTrail delivers the query results.
    deliveryS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The alias that identifies a query template.
    queryAlias :: Prelude.Maybe Prelude.Text,
    -- | The query parameters for the specified @QueryAlias@.
    queryParameters :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The SQL code of your query.
    queryStatement :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryS3Uri', 'startQuery_deliveryS3Uri' - The URI for the S3 bucket where CloudTrail delivers the query results.
--
-- 'queryAlias', 'startQuery_queryAlias' - The alias that identifies a query template.
--
-- 'queryParameters', 'startQuery_queryParameters' - The query parameters for the specified @QueryAlias@.
--
-- 'queryStatement', 'startQuery_queryStatement' - The SQL code of your query.
newStartQuery ::
  StartQuery
newStartQuery =
  StartQuery'
    { deliveryS3Uri = Prelude.Nothing,
      queryAlias = Prelude.Nothing,
      queryParameters = Prelude.Nothing,
      queryStatement = Prelude.Nothing
    }

-- | The URI for the S3 bucket where CloudTrail delivers the query results.
startQuery_deliveryS3Uri :: Lens.Lens' StartQuery (Prelude.Maybe Prelude.Text)
startQuery_deliveryS3Uri = Lens.lens (\StartQuery' {deliveryS3Uri} -> deliveryS3Uri) (\s@StartQuery' {} a -> s {deliveryS3Uri = a} :: StartQuery)

-- | The alias that identifies a query template.
startQuery_queryAlias :: Lens.Lens' StartQuery (Prelude.Maybe Prelude.Text)
startQuery_queryAlias = Lens.lens (\StartQuery' {queryAlias} -> queryAlias) (\s@StartQuery' {} a -> s {queryAlias = a} :: StartQuery)

-- | The query parameters for the specified @QueryAlias@.
startQuery_queryParameters :: Lens.Lens' StartQuery (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
startQuery_queryParameters = Lens.lens (\StartQuery' {queryParameters} -> queryParameters) (\s@StartQuery' {} a -> s {queryParameters = a} :: StartQuery) Prelude.. Lens.mapping Lens.coerced

-- | The SQL code of your query.
startQuery_queryStatement :: Lens.Lens' StartQuery (Prelude.Maybe Prelude.Text)
startQuery_queryStatement = Lens.lens (\StartQuery' {queryStatement} -> queryStatement) (\s@StartQuery' {} a -> s {queryStatement = a} :: StartQuery)

instance Core.AWSRequest StartQuery where
  type AWSResponse StartQuery = StartQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryResponse'
            Prelude.<$> (x Data..?> "QueryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartQuery where
  hashWithSalt _salt StartQuery' {..} =
    _salt
      `Prelude.hashWithSalt` deliveryS3Uri
      `Prelude.hashWithSalt` queryAlias
      `Prelude.hashWithSalt` queryParameters
      `Prelude.hashWithSalt` queryStatement

instance Prelude.NFData StartQuery where
  rnf StartQuery' {..} =
    Prelude.rnf deliveryS3Uri
      `Prelude.seq` Prelude.rnf queryAlias
      `Prelude.seq` Prelude.rnf queryParameters
      `Prelude.seq` Prelude.rnf queryStatement

instance Data.ToHeaders StartQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StartQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartQuery where
  toJSON StartQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeliveryS3Uri" Data..=) Prelude.<$> deliveryS3Uri,
            ("QueryAlias" Data..=) Prelude.<$> queryAlias,
            ("QueryParameters" Data..=)
              Prelude.<$> queryParameters,
            ("QueryStatement" Data..=)
              Prelude.<$> queryStatement
          ]
      )

instance Data.ToPath StartQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery StartQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartQueryResponse' smart constructor.
data StartQueryResponse = StartQueryResponse'
  { -- | The ID of the started query.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'startQueryResponse_queryId' - The ID of the started query.
--
-- 'httpStatus', 'startQueryResponse_httpStatus' - The response's http status code.
newStartQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartQueryResponse
newStartQueryResponse pHttpStatus_ =
  StartQueryResponse'
    { queryId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the started query.
startQueryResponse_queryId :: Lens.Lens' StartQueryResponse (Prelude.Maybe Prelude.Text)
startQueryResponse_queryId = Lens.lens (\StartQueryResponse' {queryId} -> queryId) (\s@StartQueryResponse' {} a -> s {queryId = a} :: StartQueryResponse)

-- | The response's http status code.
startQueryResponse_httpStatus :: Lens.Lens' StartQueryResponse Prelude.Int
startQueryResponse_httpStatus = Lens.lens (\StartQueryResponse' {httpStatus} -> httpStatus) (\s@StartQueryResponse' {} a -> s {httpStatus = a} :: StartQueryResponse)

instance Prelude.NFData StartQueryResponse where
  rnf StartQueryResponse' {..} =
    Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf httpStatus

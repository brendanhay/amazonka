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
-- Module      : Amazonka.DMS.DescribeSchemas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the schema for the specified endpoint.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeSchemas
  ( -- * Creating a Request
    DescribeSchemas (..),
    newDescribeSchemas,

    -- * Request Lenses
    describeSchemas_marker,
    describeSchemas_maxRecords,
    describeSchemas_endpointArn,

    -- * Destructuring the Response
    DescribeSchemasResponse (..),
    newDescribeSchemasResponse,

    -- * Response Lenses
    describeSchemasResponse_marker,
    describeSchemasResponse_schemas,
    describeSchemasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeSchemas' smart constructor.
data DescribeSchemas = DescribeSchemas'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchemas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeSchemas_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeSchemas_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'endpointArn', 'describeSchemas_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
newDescribeSchemas ::
  -- | 'endpointArn'
  Prelude.Text ->
  DescribeSchemas
newDescribeSchemas pEndpointArn_ =
  DescribeSchemas'
    { marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      endpointArn = pEndpointArn_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeSchemas_marker :: Lens.Lens' DescribeSchemas (Prelude.Maybe Prelude.Text)
describeSchemas_marker = Lens.lens (\DescribeSchemas' {marker} -> marker) (\s@DescribeSchemas' {} a -> s {marker = a} :: DescribeSchemas)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeSchemas_maxRecords :: Lens.Lens' DescribeSchemas (Prelude.Maybe Prelude.Int)
describeSchemas_maxRecords = Lens.lens (\DescribeSchemas' {maxRecords} -> maxRecords) (\s@DescribeSchemas' {} a -> s {maxRecords = a} :: DescribeSchemas)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
describeSchemas_endpointArn :: Lens.Lens' DescribeSchemas Prelude.Text
describeSchemas_endpointArn = Lens.lens (\DescribeSchemas' {endpointArn} -> endpointArn) (\s@DescribeSchemas' {} a -> s {endpointArn = a} :: DescribeSchemas)

instance Core.AWSPager DescribeSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSchemasResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSchemasResponse_schemas Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSchemas_marker
          Lens..~ rs
          Lens.^? describeSchemasResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest DescribeSchemas where
  type
    AWSResponse DescribeSchemas =
      DescribeSchemasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSchemasResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "Schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSchemas where
  hashWithSalt _salt DescribeSchemas' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData DescribeSchemas where
  rnf DescribeSchemas' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf endpointArn

instance Data.ToHeaders DescribeSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeSchemas" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSchemas where
  toJSON DescribeSchemas' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords,
            Prelude.Just ("EndpointArn" Data..= endpointArn)
          ]
      )

instance Data.ToPath DescribeSchemas where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSchemas where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeSchemasResponse' smart constructor.
data DescribeSchemasResponse = DescribeSchemasResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The described schema.
    schemas :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchemasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeSchemasResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'schemas', 'describeSchemasResponse_schemas' - The described schema.
--
-- 'httpStatus', 'describeSchemasResponse_httpStatus' - The response's http status code.
newDescribeSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSchemasResponse
newDescribeSchemasResponse pHttpStatus_ =
  DescribeSchemasResponse'
    { marker = Prelude.Nothing,
      schemas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeSchemasResponse_marker :: Lens.Lens' DescribeSchemasResponse (Prelude.Maybe Prelude.Text)
describeSchemasResponse_marker = Lens.lens (\DescribeSchemasResponse' {marker} -> marker) (\s@DescribeSchemasResponse' {} a -> s {marker = a} :: DescribeSchemasResponse)

-- | The described schema.
describeSchemasResponse_schemas :: Lens.Lens' DescribeSchemasResponse (Prelude.Maybe [Prelude.Text])
describeSchemasResponse_schemas = Lens.lens (\DescribeSchemasResponse' {schemas} -> schemas) (\s@DescribeSchemasResponse' {} a -> s {schemas = a} :: DescribeSchemasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSchemasResponse_httpStatus :: Lens.Lens' DescribeSchemasResponse Prelude.Int
describeSchemasResponse_httpStatus = Lens.lens (\DescribeSchemasResponse' {httpStatus} -> httpStatus) (\s@DescribeSchemasResponse' {} a -> s {httpStatus = a} :: DescribeSchemasResponse)

instance Prelude.NFData DescribeSchemasResponse where
  rnf DescribeSchemasResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf httpStatus

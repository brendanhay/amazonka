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
-- Module      : Network.AWS.DMS.DescribeSchemas
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the schema for the specified endpoint.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeSchemas
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
    describeSchemasResponse_schemas,
    describeSchemasResponse_marker,
    describeSchemasResponse_httpStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager DescribeSchemas where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeSchemasResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeSchemasResponse_schemas Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeSchemas_marker
          Lens..~ rs
          Lens.^? describeSchemasResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeSchemas where
  type Rs DescribeSchemas = DescribeSchemasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSchemasResponse'
            Prelude.<$> (x Prelude..?> "Schemas" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSchemas

instance Prelude.NFData DescribeSchemas

instance Prelude.ToHeaders DescribeSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.DescribeSchemas" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeSchemas where
  toJSON DescribeSchemas' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Marker" Prelude..=) Prelude.<$> marker,
            ("MaxRecords" Prelude..=) Prelude.<$> maxRecords,
            Prelude.Just ("EndpointArn" Prelude..= endpointArn)
          ]
      )

instance Prelude.ToPath DescribeSchemas where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeSchemas where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeSchemasResponse' smart constructor.
data DescribeSchemasResponse = DescribeSchemasResponse'
  { -- | The described schema.
    schemas :: Prelude.Maybe [Prelude.Text],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchemasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemas', 'describeSchemasResponse_schemas' - The described schema.
--
-- 'marker', 'describeSchemasResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeSchemasResponse_httpStatus' - The response's http status code.
newDescribeSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSchemasResponse
newDescribeSchemasResponse pHttpStatus_ =
  DescribeSchemasResponse'
    { schemas = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The described schema.
describeSchemasResponse_schemas :: Lens.Lens' DescribeSchemasResponse (Prelude.Maybe [Prelude.Text])
describeSchemasResponse_schemas = Lens.lens (\DescribeSchemasResponse' {schemas} -> schemas) (\s@DescribeSchemasResponse' {} a -> s {schemas = a} :: DescribeSchemasResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeSchemasResponse_marker :: Lens.Lens' DescribeSchemasResponse (Prelude.Maybe Prelude.Text)
describeSchemasResponse_marker = Lens.lens (\DescribeSchemasResponse' {marker} -> marker) (\s@DescribeSchemasResponse' {} a -> s {marker = a} :: DescribeSchemasResponse)

-- | The response's http status code.
describeSchemasResponse_httpStatus :: Lens.Lens' DescribeSchemasResponse Prelude.Int
describeSchemasResponse_httpStatus = Lens.lens (\DescribeSchemasResponse' {httpStatus} -> httpStatus) (\s@DescribeSchemasResponse' {} a -> s {httpStatus = a} :: DescribeSchemasResponse)

instance Prelude.NFData DescribeSchemasResponse

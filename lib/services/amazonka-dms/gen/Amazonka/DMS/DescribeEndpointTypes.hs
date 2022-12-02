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
-- Module      : Amazonka.DMS.DescribeEndpointTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the type of endpoints available.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeEndpointTypes
  ( -- * Creating a Request
    DescribeEndpointTypes (..),
    newDescribeEndpointTypes,

    -- * Request Lenses
    describeEndpointTypes_marker,
    describeEndpointTypes_filters,
    describeEndpointTypes_maxRecords,

    -- * Destructuring the Response
    DescribeEndpointTypesResponse (..),
    newDescribeEndpointTypesResponse,

    -- * Response Lenses
    describeEndpointTypesResponse_marker,
    describeEndpointTypesResponse_supportedEndpointTypes,
    describeEndpointTypesResponse_httpStatus,
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
-- /See:/ 'newDescribeEndpointTypes' smart constructor.
data DescribeEndpointTypes = DescribeEndpointTypes'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Filters applied to the endpoint types.
    --
    -- Valid filter names: engine-name | endpoint-type
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeEndpointTypes_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'filters', 'describeEndpointTypes_filters' - Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
--
-- 'maxRecords', 'describeEndpointTypes_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeEndpointTypes ::
  DescribeEndpointTypes
newDescribeEndpointTypes =
  DescribeEndpointTypes'
    { marker = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointTypes_marker :: Lens.Lens' DescribeEndpointTypes (Prelude.Maybe Prelude.Text)
describeEndpointTypes_marker = Lens.lens (\DescribeEndpointTypes' {marker} -> marker) (\s@DescribeEndpointTypes' {} a -> s {marker = a} :: DescribeEndpointTypes)

-- | Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
describeEndpointTypes_filters :: Lens.Lens' DescribeEndpointTypes (Prelude.Maybe [Filter])
describeEndpointTypes_filters = Lens.lens (\DescribeEndpointTypes' {filters} -> filters) (\s@DescribeEndpointTypes' {} a -> s {filters = a} :: DescribeEndpointTypes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEndpointTypes_maxRecords :: Lens.Lens' DescribeEndpointTypes (Prelude.Maybe Prelude.Int)
describeEndpointTypes_maxRecords = Lens.lens (\DescribeEndpointTypes' {maxRecords} -> maxRecords) (\s@DescribeEndpointTypes' {} a -> s {maxRecords = a} :: DescribeEndpointTypes)

instance Core.AWSPager DescribeEndpointTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointTypesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointTypesResponse_supportedEndpointTypes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEndpointTypes_marker
          Lens..~ rs
          Lens.^? describeEndpointTypesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEndpointTypes where
  type
    AWSResponse DescribeEndpointTypes =
      DescribeEndpointTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointTypesResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x Data..?> "SupportedEndpointTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpointTypes where
  hashWithSalt _salt DescribeEndpointTypes' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeEndpointTypes where
  rnf DescribeEndpointTypes' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeEndpointTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeEndpointTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEndpointTypes where
  toJSON DescribeEndpointTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords
          ]
      )

instance Data.ToPath DescribeEndpointTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEndpointTypes where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeEndpointTypesResponse' smart constructor.
data DescribeEndpointTypesResponse = DescribeEndpointTypesResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The types of endpoints that are supported.
    supportedEndpointTypes :: Prelude.Maybe [SupportedEndpointType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeEndpointTypesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'supportedEndpointTypes', 'describeEndpointTypesResponse_supportedEndpointTypes' - The types of endpoints that are supported.
--
-- 'httpStatus', 'describeEndpointTypesResponse_httpStatus' - The response's http status code.
newDescribeEndpointTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointTypesResponse
newDescribeEndpointTypesResponse pHttpStatus_ =
  DescribeEndpointTypesResponse'
    { marker =
        Prelude.Nothing,
      supportedEndpointTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointTypesResponse_marker :: Lens.Lens' DescribeEndpointTypesResponse (Prelude.Maybe Prelude.Text)
describeEndpointTypesResponse_marker = Lens.lens (\DescribeEndpointTypesResponse' {marker} -> marker) (\s@DescribeEndpointTypesResponse' {} a -> s {marker = a} :: DescribeEndpointTypesResponse)

-- | The types of endpoints that are supported.
describeEndpointTypesResponse_supportedEndpointTypes :: Lens.Lens' DescribeEndpointTypesResponse (Prelude.Maybe [SupportedEndpointType])
describeEndpointTypesResponse_supportedEndpointTypes = Lens.lens (\DescribeEndpointTypesResponse' {supportedEndpointTypes} -> supportedEndpointTypes) (\s@DescribeEndpointTypesResponse' {} a -> s {supportedEndpointTypes = a} :: DescribeEndpointTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEndpointTypesResponse_httpStatus :: Lens.Lens' DescribeEndpointTypesResponse Prelude.Int
describeEndpointTypesResponse_httpStatus = Lens.lens (\DescribeEndpointTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointTypesResponse' {} a -> s {httpStatus = a} :: DescribeEndpointTypesResponse)

instance Prelude.NFData DescribeEndpointTypesResponse where
  rnf DescribeEndpointTypesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf supportedEndpointTypes
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular cache parameter
-- group.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeCacheParameters
  ( -- * Creating a Request
    DescribeCacheParameters (..),
    newDescribeCacheParameters,

    -- * Request Lenses
    describeCacheParameters_marker,
    describeCacheParameters_maxRecords,
    describeCacheParameters_source,
    describeCacheParameters_cacheParameterGroupName,

    -- * Destructuring the Response
    DescribeCacheParametersResponse (..),
    newDescribeCacheParametersResponse,

    -- * Response Lenses
    describeCacheParametersResponse_cacheNodeTypeSpecificParameters,
    describeCacheParametersResponse_marker,
    describeCacheParametersResponse_parameters,
    describeCacheParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'newDescribeCacheParameters' smart constructor.
data DescribeCacheParameters = DescribeCacheParameters'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The parameter types to return.
    --
    -- Valid values: @user@ | @system@ | @engine-default@
    source :: Prelude.Maybe Prelude.Text,
    -- | The name of a specific cache parameter group to return details for.
    cacheParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeCacheParameters_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
--
-- 'source', 'describeCacheParameters_source' - The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
--
-- 'cacheParameterGroupName', 'describeCacheParameters_cacheParameterGroupName' - The name of a specific cache parameter group to return details for.
newDescribeCacheParameters ::
  -- | 'cacheParameterGroupName'
  Prelude.Text ->
  DescribeCacheParameters
newDescribeCacheParameters pCacheParameterGroupName_ =
  DescribeCacheParameters'
    { marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      source = Prelude.Nothing,
      cacheParameterGroupName =
        pCacheParameterGroupName_
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheParameters_marker :: Lens.Lens' DescribeCacheParameters (Prelude.Maybe Prelude.Text)
describeCacheParameters_marker = Lens.lens (\DescribeCacheParameters' {marker} -> marker) (\s@DescribeCacheParameters' {} a -> s {marker = a} :: DescribeCacheParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheParameters_maxRecords :: Lens.Lens' DescribeCacheParameters (Prelude.Maybe Prelude.Int)
describeCacheParameters_maxRecords = Lens.lens (\DescribeCacheParameters' {maxRecords} -> maxRecords) (\s@DescribeCacheParameters' {} a -> s {maxRecords = a} :: DescribeCacheParameters)

-- | The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
describeCacheParameters_source :: Lens.Lens' DescribeCacheParameters (Prelude.Maybe Prelude.Text)
describeCacheParameters_source = Lens.lens (\DescribeCacheParameters' {source} -> source) (\s@DescribeCacheParameters' {} a -> s {source = a} :: DescribeCacheParameters)

-- | The name of a specific cache parameter group to return details for.
describeCacheParameters_cacheParameterGroupName :: Lens.Lens' DescribeCacheParameters Prelude.Text
describeCacheParameters_cacheParameterGroupName = Lens.lens (\DescribeCacheParameters' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@DescribeCacheParameters' {} a -> s {cacheParameterGroupName = a} :: DescribeCacheParameters)

instance Core.AWSPager DescribeCacheParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheParametersResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheParametersResponse_parameters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeCacheParameters_marker
          Lens..~ rs
          Lens.^? describeCacheParametersResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeCacheParameters where
  type
    AWSResponse DescribeCacheParameters =
      DescribeCacheParametersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCacheParametersResult"
      ( \s h x ->
          DescribeCacheParametersResponse'
            Prelude.<$> ( x
                            Data..@? "CacheNodeTypeSpecificParameters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Data.parseXMLList "CacheNodeTypeSpecificParameter")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> ( x
                            Data..@? "Parameters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Parameter")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCacheParameters where
  hashWithSalt _salt DescribeCacheParameters' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` cacheParameterGroupName

instance Prelude.NFData DescribeCacheParameters where
  rnf DescribeCacheParameters' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf cacheParameterGroupName

instance Data.ToHeaders DescribeCacheParameters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCacheParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCacheParameters where
  toQuery DescribeCacheParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeCacheParameters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "Source" Data.=: source,
        "CacheParameterGroupName"
          Data.=: cacheParameterGroupName
      ]

-- | Represents the output of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'newDescribeCacheParametersResponse' smart constructor.
data DescribeCacheParametersResponse = DescribeCacheParametersResponse'
  { -- | A list of parameters specific to a particular cache node type. Each
    -- element in the list contains detailed information about one parameter.
    cacheNodeTypeSpecificParameters :: Prelude.Maybe [CacheNodeTypeSpecificParameter],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of Parameter instances.
    parameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheNodeTypeSpecificParameters', 'describeCacheParametersResponse_cacheNodeTypeSpecificParameters' - A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
--
-- 'marker', 'describeCacheParametersResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'parameters', 'describeCacheParametersResponse_parameters' - A list of Parameter instances.
--
-- 'httpStatus', 'describeCacheParametersResponse_httpStatus' - The response's http status code.
newDescribeCacheParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCacheParametersResponse
newDescribeCacheParametersResponse pHttpStatus_ =
  DescribeCacheParametersResponse'
    { cacheNodeTypeSpecificParameters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
describeCacheParametersResponse_cacheNodeTypeSpecificParameters :: Lens.Lens' DescribeCacheParametersResponse (Prelude.Maybe [CacheNodeTypeSpecificParameter])
describeCacheParametersResponse_cacheNodeTypeSpecificParameters = Lens.lens (\DescribeCacheParametersResponse' {cacheNodeTypeSpecificParameters} -> cacheNodeTypeSpecificParameters) (\s@DescribeCacheParametersResponse' {} a -> s {cacheNodeTypeSpecificParameters = a} :: DescribeCacheParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheParametersResponse_marker :: Lens.Lens' DescribeCacheParametersResponse (Prelude.Maybe Prelude.Text)
describeCacheParametersResponse_marker = Lens.lens (\DescribeCacheParametersResponse' {marker} -> marker) (\s@DescribeCacheParametersResponse' {} a -> s {marker = a} :: DescribeCacheParametersResponse)

-- | A list of Parameter instances.
describeCacheParametersResponse_parameters :: Lens.Lens' DescribeCacheParametersResponse (Prelude.Maybe [Parameter])
describeCacheParametersResponse_parameters = Lens.lens (\DescribeCacheParametersResponse' {parameters} -> parameters) (\s@DescribeCacheParametersResponse' {} a -> s {parameters = a} :: DescribeCacheParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCacheParametersResponse_httpStatus :: Lens.Lens' DescribeCacheParametersResponse Prelude.Int
describeCacheParametersResponse_httpStatus = Lens.lens (\DescribeCacheParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheParametersResponse' {} a -> s {httpStatus = a} :: DescribeCacheParametersResponse)

instance
  Prelude.NFData
    DescribeCacheParametersResponse
  where
  rnf DescribeCacheParametersResponse' {..} =
    Prelude.rnf cacheNodeTypeSpecificParameters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus

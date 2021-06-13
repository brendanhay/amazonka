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
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular cache parameter
-- group.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameters
  ( -- * Creating a Request
    DescribeCacheParameters (..),
    newDescribeCacheParameters,

    -- * Request Lenses
    describeCacheParameters_source,
    describeCacheParameters_marker,
    describeCacheParameters_maxRecords,
    describeCacheParameters_cacheParameterGroupName,

    -- * Destructuring the Response
    DescribeCacheParametersResponse (..),
    newDescribeCacheParametersResponse,

    -- * Response Lenses
    describeCacheParametersResponse_cacheNodeTypeSpecificParameters,
    describeCacheParametersResponse_parameters,
    describeCacheParametersResponse_marker,
    describeCacheParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'newDescribeCacheParameters' smart constructor.
data DescribeCacheParameters = DescribeCacheParameters'
  { -- | The parameter types to return.
    --
    -- Valid values: @user@ | @system@ | @engine-default@
    source :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
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
-- 'source', 'describeCacheParameters_source' - The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
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
-- 'cacheParameterGroupName', 'describeCacheParameters_cacheParameterGroupName' - The name of a specific cache parameter group to return details for.
newDescribeCacheParameters ::
  -- | 'cacheParameterGroupName'
  Prelude.Text ->
  DescribeCacheParameters
newDescribeCacheParameters pCacheParameterGroupName_ =
  DescribeCacheParameters'
    { source = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      cacheParameterGroupName =
        pCacheParameterGroupName_
    }

-- | The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
describeCacheParameters_source :: Lens.Lens' DescribeCacheParameters (Prelude.Maybe Prelude.Text)
describeCacheParameters_source = Lens.lens (\DescribeCacheParameters' {source} -> source) (\s@DescribeCacheParameters' {} a -> s {source = a} :: DescribeCacheParameters)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCacheParameters_marker
          Lens..~ rs
          Lens.^? describeCacheParametersResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCacheParameters where
  type
    AWSResponse DescribeCacheParameters =
      DescribeCacheParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeCacheParametersResult"
      ( \s h x ->
          DescribeCacheParametersResponse'
            Prelude.<$> ( x Core..@? "CacheNodeTypeSpecificParameters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Core.parseXMLList "CacheNodeTypeSpecificParameter")
                        )
            Prelude.<*> ( x Core..@? "Parameters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "Parameter")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCacheParameters

instance Prelude.NFData DescribeCacheParameters

instance Core.ToHeaders DescribeCacheParameters where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeCacheParameters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCacheParameters where
  toQuery DescribeCacheParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeCacheParameters" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "Source" Core.=: source,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName
      ]

-- | Represents the output of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'newDescribeCacheParametersResponse' smart constructor.
data DescribeCacheParametersResponse = DescribeCacheParametersResponse'
  { -- | A list of parameters specific to a particular cache node type. Each
    -- element in the list contains detailed information about one parameter.
    cacheNodeTypeSpecificParameters :: Prelude.Maybe [CacheNodeTypeSpecificParameter],
    -- | A list of Parameter instances.
    parameters :: Prelude.Maybe [Parameter],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'parameters', 'describeCacheParametersResponse_parameters' - A list of Parameter instances.
--
-- 'marker', 'describeCacheParametersResponse_marker' - Provides an identifier to allow retrieval of paginated results.
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
      parameters = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
describeCacheParametersResponse_cacheNodeTypeSpecificParameters :: Lens.Lens' DescribeCacheParametersResponse (Prelude.Maybe [CacheNodeTypeSpecificParameter])
describeCacheParametersResponse_cacheNodeTypeSpecificParameters = Lens.lens (\DescribeCacheParametersResponse' {cacheNodeTypeSpecificParameters} -> cacheNodeTypeSpecificParameters) (\s@DescribeCacheParametersResponse' {} a -> s {cacheNodeTypeSpecificParameters = a} :: DescribeCacheParametersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of Parameter instances.
describeCacheParametersResponse_parameters :: Lens.Lens' DescribeCacheParametersResponse (Prelude.Maybe [Parameter])
describeCacheParametersResponse_parameters = Lens.lens (\DescribeCacheParametersResponse' {parameters} -> parameters) (\s@DescribeCacheParametersResponse' {} a -> s {parameters = a} :: DescribeCacheParametersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheParametersResponse_marker :: Lens.Lens' DescribeCacheParametersResponse (Prelude.Maybe Prelude.Text)
describeCacheParametersResponse_marker = Lens.lens (\DescribeCacheParametersResponse' {marker} -> marker) (\s@DescribeCacheParametersResponse' {} a -> s {marker = a} :: DescribeCacheParametersResponse)

-- | The response's http status code.
describeCacheParametersResponse_httpStatus :: Lens.Lens' DescribeCacheParametersResponse Prelude.Int
describeCacheParametersResponse_httpStatus = Lens.lens (\DescribeCacheParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheParametersResponse' {} a -> s {httpStatus = a} :: DescribeCacheParametersResponse)

instance
  Prelude.NFData
    DescribeCacheParametersResponse

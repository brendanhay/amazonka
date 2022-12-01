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
-- Module      : Amazonka.Redshift.DescribeEndpointAuthorization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an endpoint authorization.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeEndpointAuthorization
  ( -- * Creating a Request
    DescribeEndpointAuthorization (..),
    newDescribeEndpointAuthorization,

    -- * Request Lenses
    describeEndpointAuthorization_clusterIdentifier,
    describeEndpointAuthorization_marker,
    describeEndpointAuthorization_account,
    describeEndpointAuthorization_maxRecords,
    describeEndpointAuthorization_grantee,

    -- * Destructuring the Response
    DescribeEndpointAuthorizationResponse (..),
    newDescribeEndpointAuthorizationResponse,

    -- * Response Lenses
    describeEndpointAuthorizationResponse_marker,
    describeEndpointAuthorizationResponse_endpointAuthorizationList,
    describeEndpointAuthorizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEndpointAuthorization' smart constructor.
data DescribeEndpointAuthorization = DescribeEndpointAuthorization'
  { -- | The cluster identifier of the cluster to access.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous
    -- @DescribeEndpointAuthorization@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The AAmazon Web Services account ID of either the cluster owner
    -- (grantor) or grantee. If @Grantee@ parameter is true, then the @Account@
    -- value is of the grantor.
    account :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a @Marker@ is included in the response so that the remaining
    -- results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to check authorization from a grantor or grantee point
    -- of view. If true, Amazon Redshift returns endpoint authorizations that
    -- you\'ve been granted. If false (default), checks authorization from a
    -- grantor point of view.
    grantee :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeEndpointAuthorization_clusterIdentifier' - The cluster identifier of the cluster to access.
--
-- 'marker', 'describeEndpointAuthorization_marker' - An optional pagination token provided by a previous
-- @DescribeEndpointAuthorization@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
--
-- 'account', 'describeEndpointAuthorization_account' - The AAmazon Web Services account ID of either the cluster owner
-- (grantor) or grantee. If @Grantee@ parameter is true, then the @Account@
-- value is of the grantor.
--
-- 'maxRecords', 'describeEndpointAuthorization_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a @Marker@ is included in the response so that the remaining
-- results can be retrieved.
--
-- 'grantee', 'describeEndpointAuthorization_grantee' - Indicates whether to check authorization from a grantor or grantee point
-- of view. If true, Amazon Redshift returns endpoint authorizations that
-- you\'ve been granted. If false (default), checks authorization from a
-- grantor point of view.
newDescribeEndpointAuthorization ::
  DescribeEndpointAuthorization
newDescribeEndpointAuthorization =
  DescribeEndpointAuthorization'
    { clusterIdentifier =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      account = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      grantee = Prelude.Nothing
    }

-- | The cluster identifier of the cluster to access.
describeEndpointAuthorization_clusterIdentifier :: Lens.Lens' DescribeEndpointAuthorization (Prelude.Maybe Prelude.Text)
describeEndpointAuthorization_clusterIdentifier = Lens.lens (\DescribeEndpointAuthorization' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeEndpointAuthorization' {} a -> s {clusterIdentifier = a} :: DescribeEndpointAuthorization)

-- | An optional pagination token provided by a previous
-- @DescribeEndpointAuthorization@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeEndpointAuthorization_marker :: Lens.Lens' DescribeEndpointAuthorization (Prelude.Maybe Prelude.Text)
describeEndpointAuthorization_marker = Lens.lens (\DescribeEndpointAuthorization' {marker} -> marker) (\s@DescribeEndpointAuthorization' {} a -> s {marker = a} :: DescribeEndpointAuthorization)

-- | The AAmazon Web Services account ID of either the cluster owner
-- (grantor) or grantee. If @Grantee@ parameter is true, then the @Account@
-- value is of the grantor.
describeEndpointAuthorization_account :: Lens.Lens' DescribeEndpointAuthorization (Prelude.Maybe Prelude.Text)
describeEndpointAuthorization_account = Lens.lens (\DescribeEndpointAuthorization' {account} -> account) (\s@DescribeEndpointAuthorization' {} a -> s {account = a} :: DescribeEndpointAuthorization)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a @Marker@ is included in the response so that the remaining
-- results can be retrieved.
describeEndpointAuthorization_maxRecords :: Lens.Lens' DescribeEndpointAuthorization (Prelude.Maybe Prelude.Int)
describeEndpointAuthorization_maxRecords = Lens.lens (\DescribeEndpointAuthorization' {maxRecords} -> maxRecords) (\s@DescribeEndpointAuthorization' {} a -> s {maxRecords = a} :: DescribeEndpointAuthorization)

-- | Indicates whether to check authorization from a grantor or grantee point
-- of view. If true, Amazon Redshift returns endpoint authorizations that
-- you\'ve been granted. If false (default), checks authorization from a
-- grantor point of view.
describeEndpointAuthorization_grantee :: Lens.Lens' DescribeEndpointAuthorization (Prelude.Maybe Prelude.Bool)
describeEndpointAuthorization_grantee = Lens.lens (\DescribeEndpointAuthorization' {grantee} -> grantee) (\s@DescribeEndpointAuthorization' {} a -> s {grantee = a} :: DescribeEndpointAuthorization)

instance Core.AWSPager DescribeEndpointAuthorization where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointAuthorizationResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointAuthorizationResponse_endpointAuthorizationList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEndpointAuthorization_marker
          Lens..~ rs
          Lens.^? describeEndpointAuthorizationResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEndpointAuthorization
  where
  type
    AWSResponse DescribeEndpointAuthorization =
      DescribeEndpointAuthorizationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEndpointAuthorizationResult"
      ( \s h x ->
          DescribeEndpointAuthorizationResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "EndpointAuthorizationList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEndpointAuthorization
  where
  hashWithSalt _salt DescribeEndpointAuthorization' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` grantee

instance Prelude.NFData DescribeEndpointAuthorization where
  rnf DescribeEndpointAuthorization' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf account
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf grantee

instance Core.ToHeaders DescribeEndpointAuthorization where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeEndpointAuthorization where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEndpointAuthorization where
  toQuery DescribeEndpointAuthorization' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeEndpointAuthorization" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "Marker" Core.=: marker,
        "Account" Core.=: account,
        "MaxRecords" Core.=: maxRecords,
        "Grantee" Core.=: grantee
      ]

-- | /See:/ 'newDescribeEndpointAuthorizationResponse' smart constructor.
data DescribeEndpointAuthorizationResponse = DescribeEndpointAuthorizationResponse'
  { -- | An optional pagination token provided by a previous
    -- @DescribeEndpointAuthorization@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The authorizations to an endpoint.
    endpointAuthorizationList :: Prelude.Maybe [EndpointAuthorization],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeEndpointAuthorizationResponse_marker' - An optional pagination token provided by a previous
-- @DescribeEndpointAuthorization@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
--
-- 'endpointAuthorizationList', 'describeEndpointAuthorizationResponse_endpointAuthorizationList' - The authorizations to an endpoint.
--
-- 'httpStatus', 'describeEndpointAuthorizationResponse_httpStatus' - The response's http status code.
newDescribeEndpointAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointAuthorizationResponse
newDescribeEndpointAuthorizationResponse pHttpStatus_ =
  DescribeEndpointAuthorizationResponse'
    { marker =
        Prelude.Nothing,
      endpointAuthorizationList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous
-- @DescribeEndpointAuthorization@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeEndpointAuthorizationResponse_marker :: Lens.Lens' DescribeEndpointAuthorizationResponse (Prelude.Maybe Prelude.Text)
describeEndpointAuthorizationResponse_marker = Lens.lens (\DescribeEndpointAuthorizationResponse' {marker} -> marker) (\s@DescribeEndpointAuthorizationResponse' {} a -> s {marker = a} :: DescribeEndpointAuthorizationResponse)

-- | The authorizations to an endpoint.
describeEndpointAuthorizationResponse_endpointAuthorizationList :: Lens.Lens' DescribeEndpointAuthorizationResponse (Prelude.Maybe [EndpointAuthorization])
describeEndpointAuthorizationResponse_endpointAuthorizationList = Lens.lens (\DescribeEndpointAuthorizationResponse' {endpointAuthorizationList} -> endpointAuthorizationList) (\s@DescribeEndpointAuthorizationResponse' {} a -> s {endpointAuthorizationList = a} :: DescribeEndpointAuthorizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEndpointAuthorizationResponse_httpStatus :: Lens.Lens' DescribeEndpointAuthorizationResponse Prelude.Int
describeEndpointAuthorizationResponse_httpStatus = Lens.lens (\DescribeEndpointAuthorizationResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointAuthorizationResponse' {} a -> s {httpStatus = a} :: DescribeEndpointAuthorizationResponse)

instance
  Prelude.NFData
    DescribeEndpointAuthorizationResponse
  where
  rnf DescribeEndpointAuthorizationResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf endpointAuthorizationList
      `Prelude.seq` Prelude.rnf httpStatus

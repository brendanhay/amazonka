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
-- Module      : Network.AWS.Redshift.DescribeEndpointAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a Redshift-managed VPC endpoint.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeEndpointAccess
  ( -- * Creating a Request
    DescribeEndpointAccess (..),
    newDescribeEndpointAccess,

    -- * Request Lenses
    describeEndpointAccess_endpointName,
    describeEndpointAccess_resourceOwner,
    describeEndpointAccess_clusterIdentifier,
    describeEndpointAccess_vpcId,
    describeEndpointAccess_maxRecords,
    describeEndpointAccess_marker,

    -- * Destructuring the Response
    DescribeEndpointAccessResponse (..),
    newDescribeEndpointAccessResponse,

    -- * Response Lenses
    describeEndpointAccessResponse_endpointAccessList,
    describeEndpointAccessResponse_marker,
    describeEndpointAccessResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEndpointAccess' smart constructor.
data DescribeEndpointAccess = DescribeEndpointAccess'
  { -- | The name of the endpoint to be described.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the cluster.
    resourceOwner :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier associated with the described endpoint.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The virtual private cloud (VPC) identifier with access to the cluster.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a @Marker@ is included in the response so that the remaining
    -- results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional pagination token provided by a previous
    -- @DescribeEndpointAccess@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'describeEndpointAccess_endpointName' - The name of the endpoint to be described.
--
-- 'resourceOwner', 'describeEndpointAccess_resourceOwner' - The Amazon Web Services account ID of the owner of the cluster.
--
-- 'clusterIdentifier', 'describeEndpointAccess_clusterIdentifier' - The cluster identifier associated with the described endpoint.
--
-- 'vpcId', 'describeEndpointAccess_vpcId' - The virtual private cloud (VPC) identifier with access to the cluster.
--
-- 'maxRecords', 'describeEndpointAccess_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a @Marker@ is included in the response so that the remaining
-- results can be retrieved.
--
-- 'marker', 'describeEndpointAccess_marker' - An optional pagination token provided by a previous
-- @DescribeEndpointAccess@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
newDescribeEndpointAccess ::
  DescribeEndpointAccess
newDescribeEndpointAccess =
  DescribeEndpointAccess'
    { endpointName =
        Prelude.Nothing,
      resourceOwner = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The name of the endpoint to be described.
describeEndpointAccess_endpointName :: Lens.Lens' DescribeEndpointAccess (Prelude.Maybe Prelude.Text)
describeEndpointAccess_endpointName = Lens.lens (\DescribeEndpointAccess' {endpointName} -> endpointName) (\s@DescribeEndpointAccess' {} a -> s {endpointName = a} :: DescribeEndpointAccess)

-- | The Amazon Web Services account ID of the owner of the cluster.
describeEndpointAccess_resourceOwner :: Lens.Lens' DescribeEndpointAccess (Prelude.Maybe Prelude.Text)
describeEndpointAccess_resourceOwner = Lens.lens (\DescribeEndpointAccess' {resourceOwner} -> resourceOwner) (\s@DescribeEndpointAccess' {} a -> s {resourceOwner = a} :: DescribeEndpointAccess)

-- | The cluster identifier associated with the described endpoint.
describeEndpointAccess_clusterIdentifier :: Lens.Lens' DescribeEndpointAccess (Prelude.Maybe Prelude.Text)
describeEndpointAccess_clusterIdentifier = Lens.lens (\DescribeEndpointAccess' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeEndpointAccess' {} a -> s {clusterIdentifier = a} :: DescribeEndpointAccess)

-- | The virtual private cloud (VPC) identifier with access to the cluster.
describeEndpointAccess_vpcId :: Lens.Lens' DescribeEndpointAccess (Prelude.Maybe Prelude.Text)
describeEndpointAccess_vpcId = Lens.lens (\DescribeEndpointAccess' {vpcId} -> vpcId) (\s@DescribeEndpointAccess' {} a -> s {vpcId = a} :: DescribeEndpointAccess)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a @Marker@ is included in the response so that the remaining
-- results can be retrieved.
describeEndpointAccess_maxRecords :: Lens.Lens' DescribeEndpointAccess (Prelude.Maybe Prelude.Int)
describeEndpointAccess_maxRecords = Lens.lens (\DescribeEndpointAccess' {maxRecords} -> maxRecords) (\s@DescribeEndpointAccess' {} a -> s {maxRecords = a} :: DescribeEndpointAccess)

-- | An optional pagination token provided by a previous
-- @DescribeEndpointAccess@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeEndpointAccess_marker :: Lens.Lens' DescribeEndpointAccess (Prelude.Maybe Prelude.Text)
describeEndpointAccess_marker = Lens.lens (\DescribeEndpointAccess' {marker} -> marker) (\s@DescribeEndpointAccess' {} a -> s {marker = a} :: DescribeEndpointAccess)

instance Core.AWSPager DescribeEndpointAccess where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointAccessResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointAccessResponse_endpointAccessList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEndpointAccess_marker
          Lens..~ rs
          Lens.^? describeEndpointAccessResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEndpointAccess where
  type
    AWSResponse DescribeEndpointAccess =
      DescribeEndpointAccessResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEndpointAccessResult"
      ( \s h x ->
          DescribeEndpointAccessResponse'
            Prelude.<$> ( x Core..@? "EndpointAccessList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpointAccess

instance Prelude.NFData DescribeEndpointAccess

instance Core.ToHeaders DescribeEndpointAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeEndpointAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEndpointAccess where
  toQuery DescribeEndpointAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeEndpointAccess" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "EndpointName" Core.=: endpointName,
        "ResourceOwner" Core.=: resourceOwner,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "VpcId" Core.=: vpcId,
        "MaxRecords" Core.=: maxRecords,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeEndpointAccessResponse' smart constructor.
data DescribeEndpointAccessResponse = DescribeEndpointAccessResponse'
  { -- | The list of endpoints with access to the cluster.
    endpointAccessList :: Prelude.Maybe [EndpointAccess],
    -- | An optional pagination token provided by a previous
    -- @DescribeEndpointAccess@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointAccessList', 'describeEndpointAccessResponse_endpointAccessList' - The list of endpoints with access to the cluster.
--
-- 'marker', 'describeEndpointAccessResponse_marker' - An optional pagination token provided by a previous
-- @DescribeEndpointAccess@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
--
-- 'httpStatus', 'describeEndpointAccessResponse_httpStatus' - The response's http status code.
newDescribeEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointAccessResponse
newDescribeEndpointAccessResponse pHttpStatus_ =
  DescribeEndpointAccessResponse'
    { endpointAccessList =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of endpoints with access to the cluster.
describeEndpointAccessResponse_endpointAccessList :: Lens.Lens' DescribeEndpointAccessResponse (Prelude.Maybe [EndpointAccess])
describeEndpointAccessResponse_endpointAccessList = Lens.lens (\DescribeEndpointAccessResponse' {endpointAccessList} -> endpointAccessList) (\s@DescribeEndpointAccessResponse' {} a -> s {endpointAccessList = a} :: DescribeEndpointAccessResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeEndpointAccess@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeEndpointAccessResponse_marker :: Lens.Lens' DescribeEndpointAccessResponse (Prelude.Maybe Prelude.Text)
describeEndpointAccessResponse_marker = Lens.lens (\DescribeEndpointAccessResponse' {marker} -> marker) (\s@DescribeEndpointAccessResponse' {} a -> s {marker = a} :: DescribeEndpointAccessResponse)

-- | The response's http status code.
describeEndpointAccessResponse_httpStatus :: Lens.Lens' DescribeEndpointAccessResponse Prelude.Int
describeEndpointAccessResponse_httpStatus = Lens.lens (\DescribeEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointAccessResponse' {} a -> s {httpStatus = a} :: DescribeEndpointAccessResponse)

instance
  Prelude.NFData
    DescribeEndpointAccessResponse

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
-- Module      : Amazonka.Redshift.DescribeClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns properties of provisioned clusters including general cluster
-- properties, cluster database properties, maintenance and backup
-- properties, and security and access properties. This operation supports
-- pagination. For more information about managing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all clusters that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- clusters that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, clusters are
-- returned regardless of whether they have tag keys or values associated
-- with them.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeClusters
  ( -- * Creating a Request
    DescribeClusters (..),
    newDescribeClusters,

    -- * Request Lenses
    describeClusters_clusterIdentifier,
    describeClusters_marker,
    describeClusters_maxRecords,
    describeClusters_tagKeys,
    describeClusters_tagValues,

    -- * Destructuring the Response
    DescribeClustersResponse (..),
    newDescribeClustersResponse,

    -- * Response Lenses
    describeClustersResponse_clusters,
    describeClustersResponse_marker,
    describeClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | The unique identifier of a cluster whose properties you are requesting.
    -- This parameter is case sensitive.
    --
    -- The default is that all clusters defined for an account are returned.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeClusters request
    -- exceed the value specified in @MaxRecords@, Amazon Web Services returns
    -- a value in the @Marker@ field of the response. You can retrieve the next
    -- set of response records by providing the returned marker value in the
    -- @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __ClusterIdentifier__ parameter
    -- or the __Marker__ parameter, but not both.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | A tag key or keys for which you want to return all matching clusters
    -- that are associated with the specified key or keys. For example, suppose
    -- that you have clusters that are tagged with keys called @owner@ and
    -- @environment@. If you specify both of these tag keys in the request,
    -- Amazon Redshift returns a response with the clusters that have either or
    -- both of these tag keys associated with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | A tag value or values for which you want to return all matching clusters
    -- that are associated with the specified tag value or values. For example,
    -- suppose that you have clusters that are tagged with values called
    -- @admin@ and @test@. If you specify both of these tag values in the
    -- request, Amazon Redshift returns a response with the clusters that have
    -- either or both of these tag values associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeClusters_clusterIdentifier' - The unique identifier of a cluster whose properties you are requesting.
-- This parameter is case sensitive.
--
-- The default is that all clusters defined for an account are returned.
--
-- 'marker', 'describeClusters_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusters request
-- exceed the value specified in @MaxRecords@, Amazon Web Services returns
-- a value in the @Marker@ field of the response. You can retrieve the next
-- set of response records by providing the returned marker value in the
-- @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterIdentifier__ parameter
-- or the __Marker__ parameter, but not both.
--
-- 'maxRecords', 'describeClusters_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
--
-- 'tagKeys', 'describeClusters_tagKeys' - A tag key or keys for which you want to return all matching clusters
-- that are associated with the specified key or keys. For example, suppose
-- that you have clusters that are tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with the clusters that have either or
-- both of these tag keys associated with them.
--
-- 'tagValues', 'describeClusters_tagValues' - A tag value or values for which you want to return all matching clusters
-- that are associated with the specified tag value or values. For example,
-- suppose that you have clusters that are tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with the clusters that have
-- either or both of these tag values associated with them.
newDescribeClusters ::
  DescribeClusters
newDescribeClusters =
  DescribeClusters'
    { clusterIdentifier =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      tagKeys = Prelude.Nothing,
      tagValues = Prelude.Nothing
    }

-- | The unique identifier of a cluster whose properties you are requesting.
-- This parameter is case sensitive.
--
-- The default is that all clusters defined for an account are returned.
describeClusters_clusterIdentifier :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_clusterIdentifier = Lens.lens (\DescribeClusters' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeClusters' {} a -> s {clusterIdentifier = a} :: DescribeClusters)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusters request
-- exceed the value specified in @MaxRecords@, Amazon Web Services returns
-- a value in the @Marker@ field of the response. You can retrieve the next
-- set of response records by providing the returned marker value in the
-- @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterIdentifier__ parameter
-- or the __Marker__ parameter, but not both.
describeClusters_marker :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_marker = Lens.lens (\DescribeClusters' {marker} -> marker) (\s@DescribeClusters' {} a -> s {marker = a} :: DescribeClusters)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeClusters_maxRecords :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Int)
describeClusters_maxRecords = Lens.lens (\DescribeClusters' {maxRecords} -> maxRecords) (\s@DescribeClusters' {} a -> s {maxRecords = a} :: DescribeClusters)

-- | A tag key or keys for which you want to return all matching clusters
-- that are associated with the specified key or keys. For example, suppose
-- that you have clusters that are tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with the clusters that have either or
-- both of these tag keys associated with them.
describeClusters_tagKeys :: Lens.Lens' DescribeClusters (Prelude.Maybe [Prelude.Text])
describeClusters_tagKeys = Lens.lens (\DescribeClusters' {tagKeys} -> tagKeys) (\s@DescribeClusters' {} a -> s {tagKeys = a} :: DescribeClusters) Prelude.. Lens.mapping Lens.coerced

-- | A tag value or values for which you want to return all matching clusters
-- that are associated with the specified tag value or values. For example,
-- suppose that you have clusters that are tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with the clusters that have
-- either or both of these tag values associated with them.
describeClusters_tagValues :: Lens.Lens' DescribeClusters (Prelude.Maybe [Prelude.Text])
describeClusters_tagValues = Lens.lens (\DescribeClusters' {tagValues} -> tagValues) (\s@DescribeClusters' {} a -> s {tagValues = a} :: DescribeClusters) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClustersResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClustersResponse_clusters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeClusters_marker
          Lens..~ rs
          Lens.^? describeClustersResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeClusters where
  type
    AWSResponse DescribeClusters =
      DescribeClustersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeClustersResult"
      ( \s h x ->
          DescribeClustersResponse'
            Prelude.<$> ( x
                            Data..@? "Clusters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Cluster")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusters where
  hashWithSalt _salt DescribeClusters' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData DescribeClusters where
  rnf DescribeClusters' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToHeaders DescribeClusters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClusters where
  toQuery DescribeClusters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeClusters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "TagKeys"
          Data.=: Data.toQuery
            (Data.toQueryList "TagKey" Prelude.<$> tagKeys),
        "TagValues"
          Data.=: Data.toQuery
            (Data.toQueryList "TagValue" Prelude.<$> tagValues)
      ]

-- | Contains the output from the DescribeClusters action.
--
-- /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | A list of @Cluster@ objects, where each object describes one cluster.
    clusters :: Prelude.Maybe [Cluster],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusters', 'describeClustersResponse_clusters' - A list of @Cluster@ objects, where each object describes one cluster.
--
-- 'marker', 'describeClustersResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeClustersResponse_httpStatus' - The response's http status code.
newDescribeClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClustersResponse
newDescribeClustersResponse pHttpStatus_ =
  DescribeClustersResponse'
    { clusters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @Cluster@ objects, where each object describes one cluster.
describeClustersResponse_clusters :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Cluster])
describeClustersResponse_clusters = Lens.lens (\DescribeClustersResponse' {clusters} -> clusters) (\s@DescribeClustersResponse' {} a -> s {clusters = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClustersResponse_marker :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe Prelude.Text)
describeClustersResponse_marker = Lens.lens (\DescribeClustersResponse' {marker} -> marker) (\s@DescribeClustersResponse' {} a -> s {marker = a} :: DescribeClustersResponse)

-- | The response's http status code.
describeClustersResponse_httpStatus :: Lens.Lens' DescribeClustersResponse Prelude.Int
describeClustersResponse_httpStatus = Lens.lens (\DescribeClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeClustersResponse' {} a -> s {httpStatus = a} :: DescribeClustersResponse)

instance Prelude.NFData DescribeClustersResponse where
  rnf DescribeClustersResponse' {..} =
    Prelude.rnf clusters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

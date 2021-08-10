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
-- Module      : Network.AWS.Redshift.DescribeUsageLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shows usage limits on a cluster. Results are filtered based on the
-- combination of input usage limit identifier, cluster identifier, and
-- feature type parameters:
--
-- -   If usage limit identifier, cluster identifier, and feature type are
--     not provided, then all usage limit objects for the current account
--     in the current region are returned.
--
-- -   If usage limit identifier is provided, then the corresponding usage
--     limit object is returned.
--
-- -   If cluster identifier is provided, then all usage limit objects for
--     the specified cluster are returned.
--
-- -   If cluster identifier and feature type are provided, then all usage
--     limit objects for the combination of cluster and feature are
--     returned.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeUsageLimits
  ( -- * Creating a Request
    DescribeUsageLimits (..),
    newDescribeUsageLimits,

    -- * Request Lenses
    describeUsageLimits_featureType,
    describeUsageLimits_tagKeys,
    describeUsageLimits_clusterIdentifier,
    describeUsageLimits_usageLimitId,
    describeUsageLimits_tagValues,
    describeUsageLimits_marker,
    describeUsageLimits_maxRecords,

    -- * Destructuring the Response
    DescribeUsageLimitsResponse (..),
    newDescribeUsageLimitsResponse,

    -- * Response Lenses
    describeUsageLimitsResponse_usageLimits,
    describeUsageLimitsResponse_marker,
    describeUsageLimitsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUsageLimits' smart constructor.
data DescribeUsageLimits = DescribeUsageLimits'
  { -- | The feature type for which you want to describe usage limits.
    featureType :: Prelude.Maybe UsageLimitFeatureType,
    -- | A tag key or keys for which you want to return all matching usage limit
    -- objects that are associated with the specified key or keys. For example,
    -- suppose that you have parameter groups that are tagged with keys called
    -- @owner@ and @environment@. If you specify both of these tag keys in the
    -- request, Amazon Redshift returns a response with the usage limit objects
    -- have either or both of these tag keys associated with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the cluster for which you want to describe usage
    -- limits.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the usage limit to describe.
    usageLimitId :: Prelude.Maybe Prelude.Text,
    -- | A tag value or values for which you want to return all matching usage
    -- limit objects that are associated with the specified tag value or
    -- values. For example, suppose that you have parameter groups that are
    -- tagged with values called @admin@ and @test@. If you specify both of
    -- these tag values in the request, Amazon Redshift returns a response with
    -- the usage limit objects that have either or both of these tag values
    -- associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeUsageLimits request
    -- exceed the value specified in @MaxRecords@, AWS returns a value in the
    -- @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsageLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureType', 'describeUsageLimits_featureType' - The feature type for which you want to describe usage limits.
--
-- 'tagKeys', 'describeUsageLimits_tagKeys' - A tag key or keys for which you want to return all matching usage limit
-- objects that are associated with the specified key or keys. For example,
-- suppose that you have parameter groups that are tagged with keys called
-- @owner@ and @environment@. If you specify both of these tag keys in the
-- request, Amazon Redshift returns a response with the usage limit objects
-- have either or both of these tag keys associated with them.
--
-- 'clusterIdentifier', 'describeUsageLimits_clusterIdentifier' - The identifier of the cluster for which you want to describe usage
-- limits.
--
-- 'usageLimitId', 'describeUsageLimits_usageLimitId' - The identifier of the usage limit to describe.
--
-- 'tagValues', 'describeUsageLimits_tagValues' - A tag value or values for which you want to return all matching usage
-- limit objects that are associated with the specified tag value or
-- values. For example, suppose that you have parameter groups that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the usage limit objects that have either or both of these tag values
-- associated with them.
--
-- 'marker', 'describeUsageLimits_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeUsageLimits request
-- exceed the value specified in @MaxRecords@, AWS returns a value in the
-- @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'maxRecords', 'describeUsageLimits_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeUsageLimits ::
  DescribeUsageLimits
newDescribeUsageLimits =
  DescribeUsageLimits'
    { featureType = Prelude.Nothing,
      tagKeys = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      usageLimitId = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The feature type for which you want to describe usage limits.
describeUsageLimits_featureType :: Lens.Lens' DescribeUsageLimits (Prelude.Maybe UsageLimitFeatureType)
describeUsageLimits_featureType = Lens.lens (\DescribeUsageLimits' {featureType} -> featureType) (\s@DescribeUsageLimits' {} a -> s {featureType = a} :: DescribeUsageLimits)

-- | A tag key or keys for which you want to return all matching usage limit
-- objects that are associated with the specified key or keys. For example,
-- suppose that you have parameter groups that are tagged with keys called
-- @owner@ and @environment@. If you specify both of these tag keys in the
-- request, Amazon Redshift returns a response with the usage limit objects
-- have either or both of these tag keys associated with them.
describeUsageLimits_tagKeys :: Lens.Lens' DescribeUsageLimits (Prelude.Maybe [Prelude.Text])
describeUsageLimits_tagKeys = Lens.lens (\DescribeUsageLimits' {tagKeys} -> tagKeys) (\s@DescribeUsageLimits' {} a -> s {tagKeys = a} :: DescribeUsageLimits) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier of the cluster for which you want to describe usage
-- limits.
describeUsageLimits_clusterIdentifier :: Lens.Lens' DescribeUsageLimits (Prelude.Maybe Prelude.Text)
describeUsageLimits_clusterIdentifier = Lens.lens (\DescribeUsageLimits' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeUsageLimits' {} a -> s {clusterIdentifier = a} :: DescribeUsageLimits)

-- | The identifier of the usage limit to describe.
describeUsageLimits_usageLimitId :: Lens.Lens' DescribeUsageLimits (Prelude.Maybe Prelude.Text)
describeUsageLimits_usageLimitId = Lens.lens (\DescribeUsageLimits' {usageLimitId} -> usageLimitId) (\s@DescribeUsageLimits' {} a -> s {usageLimitId = a} :: DescribeUsageLimits)

-- | A tag value or values for which you want to return all matching usage
-- limit objects that are associated with the specified tag value or
-- values. For example, suppose that you have parameter groups that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the usage limit objects that have either or both of these tag values
-- associated with them.
describeUsageLimits_tagValues :: Lens.Lens' DescribeUsageLimits (Prelude.Maybe [Prelude.Text])
describeUsageLimits_tagValues = Lens.lens (\DescribeUsageLimits' {tagValues} -> tagValues) (\s@DescribeUsageLimits' {} a -> s {tagValues = a} :: DescribeUsageLimits) Prelude.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeUsageLimits request
-- exceed the value specified in @MaxRecords@, AWS returns a value in the
-- @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeUsageLimits_marker :: Lens.Lens' DescribeUsageLimits (Prelude.Maybe Prelude.Text)
describeUsageLimits_marker = Lens.lens (\DescribeUsageLimits' {marker} -> marker) (\s@DescribeUsageLimits' {} a -> s {marker = a} :: DescribeUsageLimits)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeUsageLimits_maxRecords :: Lens.Lens' DescribeUsageLimits (Prelude.Maybe Prelude.Int)
describeUsageLimits_maxRecords = Lens.lens (\DescribeUsageLimits' {maxRecords} -> maxRecords) (\s@DescribeUsageLimits' {} a -> s {maxRecords = a} :: DescribeUsageLimits)

instance Core.AWSPager DescribeUsageLimits where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUsageLimitsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUsageLimitsResponse_usageLimits
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeUsageLimits_marker
          Lens..~ rs
          Lens.^? describeUsageLimitsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeUsageLimits where
  type
    AWSResponse DescribeUsageLimits =
      DescribeUsageLimitsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeUsageLimitsResult"
      ( \s h x ->
          DescribeUsageLimitsResponse'
            Prelude.<$> ( x Core..@? "UsageLimits" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUsageLimits

instance Prelude.NFData DescribeUsageLimits

instance Core.ToHeaders DescribeUsageLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeUsageLimits where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUsageLimits where
  toQuery DescribeUsageLimits' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeUsageLimits" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "FeatureType" Core.=: featureType,
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Prelude.<$> tagKeys),
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "UsageLimitId" Core.=: usageLimitId,
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Prelude.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeUsageLimitsResponse' smart constructor.
data DescribeUsageLimitsResponse = DescribeUsageLimitsResponse'
  { -- | Contains the output from the DescribeUsageLimits action.
    usageLimits :: Prelude.Maybe [UsageLimit],
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
-- Create a value of 'DescribeUsageLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimits', 'describeUsageLimitsResponse_usageLimits' - Contains the output from the DescribeUsageLimits action.
--
-- 'marker', 'describeUsageLimitsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeUsageLimitsResponse_httpStatus' - The response's http status code.
newDescribeUsageLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUsageLimitsResponse
newDescribeUsageLimitsResponse pHttpStatus_ =
  DescribeUsageLimitsResponse'
    { usageLimits =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the output from the DescribeUsageLimits action.
describeUsageLimitsResponse_usageLimits :: Lens.Lens' DescribeUsageLimitsResponse (Prelude.Maybe [UsageLimit])
describeUsageLimitsResponse_usageLimits = Lens.lens (\DescribeUsageLimitsResponse' {usageLimits} -> usageLimits) (\s@DescribeUsageLimitsResponse' {} a -> s {usageLimits = a} :: DescribeUsageLimitsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeUsageLimitsResponse_marker :: Lens.Lens' DescribeUsageLimitsResponse (Prelude.Maybe Prelude.Text)
describeUsageLimitsResponse_marker = Lens.lens (\DescribeUsageLimitsResponse' {marker} -> marker) (\s@DescribeUsageLimitsResponse' {} a -> s {marker = a} :: DescribeUsageLimitsResponse)

-- | The response's http status code.
describeUsageLimitsResponse_httpStatus :: Lens.Lens' DescribeUsageLimitsResponse Prelude.Int
describeUsageLimitsResponse_httpStatus = Lens.lens (\DescribeUsageLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeUsageLimitsResponse' {} a -> s {httpStatus = a} :: DescribeUsageLimitsResponse)

instance Prelude.NFData DescribeUsageLimitsResponse

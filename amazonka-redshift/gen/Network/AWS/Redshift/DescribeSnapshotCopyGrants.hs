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
-- Module      : Network.AWS.Redshift.DescribeSnapshotCopyGrants
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot copy grants owned by the AWS account in the
-- destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeSnapshotCopyGrants
  ( -- * Creating a Request
    DescribeSnapshotCopyGrants (..),
    newDescribeSnapshotCopyGrants,

    -- * Request Lenses
    describeSnapshotCopyGrants_tagKeys,
    describeSnapshotCopyGrants_snapshotCopyGrantName,
    describeSnapshotCopyGrants_tagValues,
    describeSnapshotCopyGrants_marker,
    describeSnapshotCopyGrants_maxRecords,

    -- * Destructuring the Response
    DescribeSnapshotCopyGrantsResponse (..),
    newDescribeSnapshotCopyGrantsResponse,

    -- * Response Lenses
    describeSnapshotCopyGrantsResponse_snapshotCopyGrants,
    describeSnapshotCopyGrantsResponse_marker,
    describeSnapshotCopyGrantsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result of the @DescribeSnapshotCopyGrants@ action.
--
-- /See:/ 'newDescribeSnapshotCopyGrants' smart constructor.
data DescribeSnapshotCopyGrants = DescribeSnapshotCopyGrants'
  { -- | A tag key or keys for which you want to return all matching resources
    -- that are associated with the specified key or keys. For example, suppose
    -- that you have resources tagged with keys called @owner@ and
    -- @environment@. If you specify both of these tag keys in the request,
    -- Amazon Redshift returns a response with all resources that have either
    -- or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Core.Text],
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Core.Maybe Core.Text,
    -- | A tag value or values for which you want to return all matching
    -- resources that are associated with the specified value or values. For
    -- example, suppose that you have resources tagged with values called
    -- @admin@ and @test@. If you specify both of these tag values in the
    -- request, Amazon Redshift returns a response with all resources that have
    -- either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Core.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a @DescribeSnapshotCopyGrant@
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__
    -- parameter or the __Marker__ parameter, but not both.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSnapshotCopyGrants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'describeSnapshotCopyGrants_tagKeys' - A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
--
-- 'snapshotCopyGrantName', 'describeSnapshotCopyGrants_snapshotCopyGrantName' - The name of the snapshot copy grant.
--
-- 'tagValues', 'describeSnapshotCopyGrants_tagValues' - A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
--
-- 'marker', 'describeSnapshotCopyGrants_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
--
-- 'maxRecords', 'describeSnapshotCopyGrants_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeSnapshotCopyGrants ::
  DescribeSnapshotCopyGrants
newDescribeSnapshotCopyGrants =
  DescribeSnapshotCopyGrants'
    { tagKeys = Core.Nothing,
      snapshotCopyGrantName = Core.Nothing,
      tagValues = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
describeSnapshotCopyGrants_tagKeys :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe [Core.Text])
describeSnapshotCopyGrants_tagKeys = Lens.lens (\DescribeSnapshotCopyGrants' {tagKeys} -> tagKeys) (\s@DescribeSnapshotCopyGrants' {} a -> s {tagKeys = a} :: DescribeSnapshotCopyGrants) Core.. Lens.mapping Lens._Coerce

-- | The name of the snapshot copy grant.
describeSnapshotCopyGrants_snapshotCopyGrantName :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Core.Text)
describeSnapshotCopyGrants_snapshotCopyGrantName = Lens.lens (\DescribeSnapshotCopyGrants' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@DescribeSnapshotCopyGrants' {} a -> s {snapshotCopyGrantName = a} :: DescribeSnapshotCopyGrants)

-- | A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
describeSnapshotCopyGrants_tagValues :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe [Core.Text])
describeSnapshotCopyGrants_tagValues = Lens.lens (\DescribeSnapshotCopyGrants' {tagValues} -> tagValues) (\s@DescribeSnapshotCopyGrants' {} a -> s {tagValues = a} :: DescribeSnapshotCopyGrants) Core.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
describeSnapshotCopyGrants_marker :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Core.Text)
describeSnapshotCopyGrants_marker = Lens.lens (\DescribeSnapshotCopyGrants' {marker} -> marker) (\s@DescribeSnapshotCopyGrants' {} a -> s {marker = a} :: DescribeSnapshotCopyGrants)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeSnapshotCopyGrants_maxRecords :: Lens.Lens' DescribeSnapshotCopyGrants (Core.Maybe Core.Int)
describeSnapshotCopyGrants_maxRecords = Lens.lens (\DescribeSnapshotCopyGrants' {maxRecords} -> maxRecords) (\s@DescribeSnapshotCopyGrants' {} a -> s {maxRecords = a} :: DescribeSnapshotCopyGrants)

instance Core.AWSPager DescribeSnapshotCopyGrants where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotCopyGrantsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotCopyGrantsResponse_snapshotCopyGrants
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSnapshotCopyGrants_marker
          Lens..~ rs
          Lens.^? describeSnapshotCopyGrantsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeSnapshotCopyGrants where
  type
    AWSResponse DescribeSnapshotCopyGrants =
      DescribeSnapshotCopyGrantsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeSnapshotCopyGrantsResult"
      ( \s h x ->
          DescribeSnapshotCopyGrantsResponse'
            Core.<$> ( x Core..@? "SnapshotCopyGrants" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "SnapshotCopyGrant")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSnapshotCopyGrants

instance Core.NFData DescribeSnapshotCopyGrants

instance Core.ToHeaders DescribeSnapshotCopyGrants where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSnapshotCopyGrants where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSnapshotCopyGrants where
  toQuery DescribeSnapshotCopyGrants' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSnapshotCopyGrants" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Core.<$> tagKeys),
        "SnapshotCopyGrantName"
          Core.=: snapshotCopyGrantName,
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Core.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeSnapshotCopyGrantsResponse' smart constructor.
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
  { -- | The list of @SnapshotCopyGrant@ objects.
    snapshotCopyGrants :: Core.Maybe [SnapshotCopyGrant],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a @DescribeSnapshotCopyGrant@
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__
    -- parameter or the __Marker__ parameter, but not both.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSnapshotCopyGrantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotCopyGrants', 'describeSnapshotCopyGrantsResponse_snapshotCopyGrants' - The list of @SnapshotCopyGrant@ objects.
--
-- 'marker', 'describeSnapshotCopyGrantsResponse_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
--
-- 'httpStatus', 'describeSnapshotCopyGrantsResponse_httpStatus' - The response's http status code.
newDescribeSnapshotCopyGrantsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSnapshotCopyGrantsResponse
newDescribeSnapshotCopyGrantsResponse pHttpStatus_ =
  DescribeSnapshotCopyGrantsResponse'
    { snapshotCopyGrants =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of @SnapshotCopyGrant@ objects.
describeSnapshotCopyGrantsResponse_snapshotCopyGrants :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Core.Maybe [SnapshotCopyGrant])
describeSnapshotCopyGrantsResponse_snapshotCopyGrants = Lens.lens (\DescribeSnapshotCopyGrantsResponse' {snapshotCopyGrants} -> snapshotCopyGrants) (\s@DescribeSnapshotCopyGrantsResponse' {} a -> s {snapshotCopyGrants = a} :: DescribeSnapshotCopyGrantsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
describeSnapshotCopyGrantsResponse_marker :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Core.Maybe Core.Text)
describeSnapshotCopyGrantsResponse_marker = Lens.lens (\DescribeSnapshotCopyGrantsResponse' {marker} -> marker) (\s@DescribeSnapshotCopyGrantsResponse' {} a -> s {marker = a} :: DescribeSnapshotCopyGrantsResponse)

-- | The response's http status code.
describeSnapshotCopyGrantsResponse_httpStatus :: Lens.Lens' DescribeSnapshotCopyGrantsResponse Core.Int
describeSnapshotCopyGrantsResponse_httpStatus = Lens.lens (\DescribeSnapshotCopyGrantsResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotCopyGrantsResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotCopyGrantsResponse)

instance
  Core.NFData
    DescribeSnapshotCopyGrantsResponse

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
-- Module      : Network.AWS.Redshift.DescribeClusterDbRevisions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterDbRevision@ objects.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterDbRevisions
  ( -- * Creating a Request
    DescribeClusterDbRevisions (..),
    newDescribeClusterDbRevisions,

    -- * Request Lenses
    describeClusterDbRevisions_clusterIdentifier,
    describeClusterDbRevisions_marker,
    describeClusterDbRevisions_maxRecords,

    -- * Destructuring the Response
    DescribeClusterDbRevisionsResponse (..),
    newDescribeClusterDbRevisionsResponse,

    -- * Response Lenses
    describeClusterDbRevisionsResponse_clusterDbRevisions,
    describeClusterDbRevisionsResponse_marker,
    describeClusterDbRevisionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClusterDbRevisions' smart constructor.
data DescribeClusterDbRevisions = DescribeClusterDbRevisions'
  { -- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are
    -- requesting. This parameter is case sensitive. All clusters defined for
    -- an account are returned by default.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point for returning a
    -- set of response records. When the results of a
    -- @DescribeClusterDbRevisions@ request exceed the value specified in
    -- @MaxRecords@, Amazon Redshift returns a value in the @marker@ field of
    -- the response. You can retrieve the next set of response records by
    -- providing the returned @marker@ value in the @marker@ parameter and
    -- retrying the request.
    --
    -- Constraints: You can specify either the @ClusterIdentifier@ parameter,
    -- or the @marker@ parameter, but not both.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified MaxRecords
    -- value, a value is returned in the @marker@ field of the response. You
    -- can retrieve the next set of response records by providing the returned
    -- @marker@ value in the @marker@ parameter and retrying the request.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClusterDbRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeClusterDbRevisions_clusterIdentifier' - A unique identifier for a cluster whose @ClusterDbRevisions@ you are
-- requesting. This parameter is case sensitive. All clusters defined for
-- an account are returned by default.
--
-- 'marker', 'describeClusterDbRevisions_marker' - An optional parameter that specifies the starting point for returning a
-- set of response records. When the results of a
-- @DescribeClusterDbRevisions@ request exceed the value specified in
-- @MaxRecords@, Amazon Redshift returns a value in the @marker@ field of
-- the response. You can retrieve the next set of response records by
-- providing the returned @marker@ value in the @marker@ parameter and
-- retrying the request.
--
-- Constraints: You can specify either the @ClusterIdentifier@ parameter,
-- or the @marker@ parameter, but not both.
--
-- 'maxRecords', 'describeClusterDbRevisions_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in the @marker@ field of the response. You
-- can retrieve the next set of response records by providing the returned
-- @marker@ value in the @marker@ parameter and retrying the request.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100.
newDescribeClusterDbRevisions ::
  DescribeClusterDbRevisions
newDescribeClusterDbRevisions =
  DescribeClusterDbRevisions'
    { clusterIdentifier =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are
-- requesting. This parameter is case sensitive. All clusters defined for
-- an account are returned by default.
describeClusterDbRevisions_clusterIdentifier :: Lens.Lens' DescribeClusterDbRevisions (Core.Maybe Core.Text)
describeClusterDbRevisions_clusterIdentifier = Lens.lens (\DescribeClusterDbRevisions' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeClusterDbRevisions' {} a -> s {clusterIdentifier = a} :: DescribeClusterDbRevisions)

-- | An optional parameter that specifies the starting point for returning a
-- set of response records. When the results of a
-- @DescribeClusterDbRevisions@ request exceed the value specified in
-- @MaxRecords@, Amazon Redshift returns a value in the @marker@ field of
-- the response. You can retrieve the next set of response records by
-- providing the returned @marker@ value in the @marker@ parameter and
-- retrying the request.
--
-- Constraints: You can specify either the @ClusterIdentifier@ parameter,
-- or the @marker@ parameter, but not both.
describeClusterDbRevisions_marker :: Lens.Lens' DescribeClusterDbRevisions (Core.Maybe Core.Text)
describeClusterDbRevisions_marker = Lens.lens (\DescribeClusterDbRevisions' {marker} -> marker) (\s@DescribeClusterDbRevisions' {} a -> s {marker = a} :: DescribeClusterDbRevisions)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in the @marker@ field of the response. You
-- can retrieve the next set of response records by providing the returned
-- @marker@ value in the @marker@ parameter and retrying the request.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100.
describeClusterDbRevisions_maxRecords :: Lens.Lens' DescribeClusterDbRevisions (Core.Maybe Core.Int)
describeClusterDbRevisions_maxRecords = Lens.lens (\DescribeClusterDbRevisions' {maxRecords} -> maxRecords) (\s@DescribeClusterDbRevisions' {} a -> s {maxRecords = a} :: DescribeClusterDbRevisions)

instance Core.AWSPager DescribeClusterDbRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterDbRevisionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterDbRevisionsResponse_clusterDbRevisions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClusterDbRevisions_marker
          Lens..~ rs
          Lens.^? describeClusterDbRevisionsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeClusterDbRevisions where
  type
    AWSResponse DescribeClusterDbRevisions =
      DescribeClusterDbRevisionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeClusterDbRevisionsResult"
      ( \s h x ->
          DescribeClusterDbRevisionsResponse'
            Core.<$> ( x Core..@? "ClusterDbRevisions" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ClusterDbRevision")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClusterDbRevisions

instance Core.NFData DescribeClusterDbRevisions

instance Core.ToHeaders DescribeClusterDbRevisions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClusterDbRevisions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClusterDbRevisions where
  toQuery DescribeClusterDbRevisions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClusterDbRevisions" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeClusterDbRevisionsResponse' smart constructor.
data DescribeClusterDbRevisionsResponse = DescribeClusterDbRevisionsResponse'
  { -- | A list of revisions.
    clusterDbRevisions :: Core.Maybe [ClusterDbRevision],
    -- | A string representing the starting point for the next set of revisions.
    -- If a value is returned in a response, you can retrieve the next set of
    -- revisions by providing the value in the @marker@ parameter and retrying
    -- the command. If the @marker@ field is empty, all revisions have already
    -- been returned.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClusterDbRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterDbRevisions', 'describeClusterDbRevisionsResponse_clusterDbRevisions' - A list of revisions.
--
-- 'marker', 'describeClusterDbRevisionsResponse_marker' - A string representing the starting point for the next set of revisions.
-- If a value is returned in a response, you can retrieve the next set of
-- revisions by providing the value in the @marker@ parameter and retrying
-- the command. If the @marker@ field is empty, all revisions have already
-- been returned.
--
-- 'httpStatus', 'describeClusterDbRevisionsResponse_httpStatus' - The response's http status code.
newDescribeClusterDbRevisionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClusterDbRevisionsResponse
newDescribeClusterDbRevisionsResponse pHttpStatus_ =
  DescribeClusterDbRevisionsResponse'
    { clusterDbRevisions =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of revisions.
describeClusterDbRevisionsResponse_clusterDbRevisions :: Lens.Lens' DescribeClusterDbRevisionsResponse (Core.Maybe [ClusterDbRevision])
describeClusterDbRevisionsResponse_clusterDbRevisions = Lens.lens (\DescribeClusterDbRevisionsResponse' {clusterDbRevisions} -> clusterDbRevisions) (\s@DescribeClusterDbRevisionsResponse' {} a -> s {clusterDbRevisions = a} :: DescribeClusterDbRevisionsResponse) Core.. Lens.mapping Lens._Coerce

-- | A string representing the starting point for the next set of revisions.
-- If a value is returned in a response, you can retrieve the next set of
-- revisions by providing the value in the @marker@ parameter and retrying
-- the command. If the @marker@ field is empty, all revisions have already
-- been returned.
describeClusterDbRevisionsResponse_marker :: Lens.Lens' DescribeClusterDbRevisionsResponse (Core.Maybe Core.Text)
describeClusterDbRevisionsResponse_marker = Lens.lens (\DescribeClusterDbRevisionsResponse' {marker} -> marker) (\s@DescribeClusterDbRevisionsResponse' {} a -> s {marker = a} :: DescribeClusterDbRevisionsResponse)

-- | The response's http status code.
describeClusterDbRevisionsResponse_httpStatus :: Lens.Lens' DescribeClusterDbRevisionsResponse Core.Int
describeClusterDbRevisionsResponse_httpStatus = Lens.lens (\DescribeClusterDbRevisionsResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterDbRevisionsResponse' {} a -> s {httpStatus = a} :: DescribeClusterDbRevisionsResponse)

instance
  Core.NFData
    DescribeClusterDbRevisionsResponse

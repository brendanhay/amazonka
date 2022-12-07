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
-- Module      : Amazonka.Redshift.DescribeClusterDbRevisions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterDbRevision@ objects.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeClusterDbRevisions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusterDbRevisions' smart constructor.
data DescribeClusterDbRevisions = DescribeClusterDbRevisions'
  { -- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are
    -- requesting. This parameter is case sensitive. All clusters defined for
    -- an account are returned by default.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
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
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified MaxRecords
    -- value, a value is returned in the @marker@ field of the response. You
    -- can retrieve the next set of response records by providing the returned
    -- @marker@ value in the @marker@ parameter and retrying the request.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are
-- requesting. This parameter is case sensitive. All clusters defined for
-- an account are returned by default.
describeClusterDbRevisions_clusterIdentifier :: Lens.Lens' DescribeClusterDbRevisions (Prelude.Maybe Prelude.Text)
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
describeClusterDbRevisions_marker :: Lens.Lens' DescribeClusterDbRevisions (Prelude.Maybe Prelude.Text)
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
describeClusterDbRevisions_maxRecords :: Lens.Lens' DescribeClusterDbRevisions (Prelude.Maybe Prelude.Int)
describeClusterDbRevisions_maxRecords = Lens.lens (\DescribeClusterDbRevisions' {maxRecords} -> maxRecords) (\s@DescribeClusterDbRevisions' {} a -> s {maxRecords = a} :: DescribeClusterDbRevisions)

instance Core.AWSPager DescribeClusterDbRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterDbRevisionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterDbRevisionsResponse_clusterDbRevisions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeClusterDbRevisions_marker
          Lens..~ rs
          Lens.^? describeClusterDbRevisionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeClusterDbRevisions where
  type
    AWSResponse DescribeClusterDbRevisions =
      DescribeClusterDbRevisionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeClusterDbRevisionsResult"
      ( \s h x ->
          DescribeClusterDbRevisionsResponse'
            Prelude.<$> ( x Data..@? "ClusterDbRevisions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "ClusterDbRevision")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusterDbRevisions where
  hashWithSalt _salt DescribeClusterDbRevisions' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeClusterDbRevisions where
  rnf DescribeClusterDbRevisions' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeClusterDbRevisions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeClusterDbRevisions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClusterDbRevisions where
  toQuery DescribeClusterDbRevisions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeClusterDbRevisions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | /See:/ 'newDescribeClusterDbRevisionsResponse' smart constructor.
data DescribeClusterDbRevisionsResponse = DescribeClusterDbRevisionsResponse'
  { -- | A list of revisions.
    clusterDbRevisions :: Prelude.Maybe [ClusterDbRevision],
    -- | A string representing the starting point for the next set of revisions.
    -- If a value is returned in a response, you can retrieve the next set of
    -- revisions by providing the value in the @marker@ parameter and retrying
    -- the command. If the @marker@ field is empty, all revisions have already
    -- been returned.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeClusterDbRevisionsResponse
newDescribeClusterDbRevisionsResponse pHttpStatus_ =
  DescribeClusterDbRevisionsResponse'
    { clusterDbRevisions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of revisions.
describeClusterDbRevisionsResponse_clusterDbRevisions :: Lens.Lens' DescribeClusterDbRevisionsResponse (Prelude.Maybe [ClusterDbRevision])
describeClusterDbRevisionsResponse_clusterDbRevisions = Lens.lens (\DescribeClusterDbRevisionsResponse' {clusterDbRevisions} -> clusterDbRevisions) (\s@DescribeClusterDbRevisionsResponse' {} a -> s {clusterDbRevisions = a} :: DescribeClusterDbRevisionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A string representing the starting point for the next set of revisions.
-- If a value is returned in a response, you can retrieve the next set of
-- revisions by providing the value in the @marker@ parameter and retrying
-- the command. If the @marker@ field is empty, all revisions have already
-- been returned.
describeClusterDbRevisionsResponse_marker :: Lens.Lens' DescribeClusterDbRevisionsResponse (Prelude.Maybe Prelude.Text)
describeClusterDbRevisionsResponse_marker = Lens.lens (\DescribeClusterDbRevisionsResponse' {marker} -> marker) (\s@DescribeClusterDbRevisionsResponse' {} a -> s {marker = a} :: DescribeClusterDbRevisionsResponse)

-- | The response's http status code.
describeClusterDbRevisionsResponse_httpStatus :: Lens.Lens' DescribeClusterDbRevisionsResponse Prelude.Int
describeClusterDbRevisionsResponse_httpStatus = Lens.lens (\DescribeClusterDbRevisionsResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterDbRevisionsResponse' {} a -> s {httpStatus = a} :: DescribeClusterDbRevisionsResponse)

instance
  Prelude.NFData
    DescribeClusterDbRevisionsResponse
  where
  rnf DescribeClusterDbRevisionsResponse' {..} =
    Prelude.rnf clusterDbRevisions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

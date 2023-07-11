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
-- Module      : Amazonka.Redshift.DescribeClusterTracks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the available maintenance tracks.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeClusterTracks
  ( -- * Creating a Request
    DescribeClusterTracks (..),
    newDescribeClusterTracks,

    -- * Request Lenses
    describeClusterTracks_maintenanceTrackName,
    describeClusterTracks_marker,
    describeClusterTracks_maxRecords,

    -- * Destructuring the Response
    DescribeClusterTracksResponse (..),
    newDescribeClusterTracksResponse,

    -- * Response Lenses
    describeClusterTracksResponse_maintenanceTracks,
    describeClusterTracksResponse_marker,
    describeClusterTracksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusterTracks' smart constructor.
data DescribeClusterTracks = DescribeClusterTracks'
  { -- | The name of the maintenance track.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a @DescribeClusterTracks@
    -- request exceed the value specified in @MaxRecords@, Amazon Redshift
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An integer value for the maximum number of maintenance tracks to return.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterTracks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceTrackName', 'describeClusterTracks_maintenanceTrackName' - The name of the maintenance track.
--
-- 'marker', 'describeClusterTracks_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeClusterTracks@
-- request exceed the value specified in @MaxRecords@, Amazon Redshift
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'maxRecords', 'describeClusterTracks_maxRecords' - An integer value for the maximum number of maintenance tracks to return.
newDescribeClusterTracks ::
  DescribeClusterTracks
newDescribeClusterTracks =
  DescribeClusterTracks'
    { maintenanceTrackName =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of the maintenance track.
describeClusterTracks_maintenanceTrackName :: Lens.Lens' DescribeClusterTracks (Prelude.Maybe Prelude.Text)
describeClusterTracks_maintenanceTrackName = Lens.lens (\DescribeClusterTracks' {maintenanceTrackName} -> maintenanceTrackName) (\s@DescribeClusterTracks' {} a -> s {maintenanceTrackName = a} :: DescribeClusterTracks)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeClusterTracks@
-- request exceed the value specified in @MaxRecords@, Amazon Redshift
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeClusterTracks_marker :: Lens.Lens' DescribeClusterTracks (Prelude.Maybe Prelude.Text)
describeClusterTracks_marker = Lens.lens (\DescribeClusterTracks' {marker} -> marker) (\s@DescribeClusterTracks' {} a -> s {marker = a} :: DescribeClusterTracks)

-- | An integer value for the maximum number of maintenance tracks to return.
describeClusterTracks_maxRecords :: Lens.Lens' DescribeClusterTracks (Prelude.Maybe Prelude.Int)
describeClusterTracks_maxRecords = Lens.lens (\DescribeClusterTracks' {maxRecords} -> maxRecords) (\s@DescribeClusterTracks' {} a -> s {maxRecords = a} :: DescribeClusterTracks)

instance Core.AWSPager DescribeClusterTracks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterTracksResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterTracksResponse_maintenanceTracks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeClusterTracks_marker
          Lens..~ rs
          Lens.^? describeClusterTracksResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeClusterTracks where
  type
    AWSResponse DescribeClusterTracks =
      DescribeClusterTracksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeClusterTracksResult"
      ( \s h x ->
          DescribeClusterTracksResponse'
            Prelude.<$> ( x
                            Data..@? "MaintenanceTracks"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "MaintenanceTrack")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusterTracks where
  hashWithSalt _salt DescribeClusterTracks' {..} =
    _salt
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeClusterTracks where
  rnf DescribeClusterTracks' {..} =
    Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeClusterTracks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeClusterTracks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClusterTracks where
  toQuery DescribeClusterTracks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeClusterTracks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "MaintenanceTrackName" Data.=: maintenanceTrackName,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | /See:/ 'newDescribeClusterTracksResponse' smart constructor.
data DescribeClusterTracksResponse = DescribeClusterTracksResponse'
  { -- | A list of maintenance tracks output by the @DescribeClusterTracks@
    -- operation.
    maintenanceTracks :: Prelude.Maybe [MaintenanceTrack],
    -- | The starting point to return a set of response tracklist records. You
    -- can retrieve the next set of response records by providing the returned
    -- marker value in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterTracksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceTracks', 'describeClusterTracksResponse_maintenanceTracks' - A list of maintenance tracks output by the @DescribeClusterTracks@
-- operation.
--
-- 'marker', 'describeClusterTracksResponse_marker' - The starting point to return a set of response tracklist records. You
-- can retrieve the next set of response records by providing the returned
-- marker value in the @Marker@ parameter and retrying the request.
--
-- 'httpStatus', 'describeClusterTracksResponse_httpStatus' - The response's http status code.
newDescribeClusterTracksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClusterTracksResponse
newDescribeClusterTracksResponse pHttpStatus_ =
  DescribeClusterTracksResponse'
    { maintenanceTracks =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of maintenance tracks output by the @DescribeClusterTracks@
-- operation.
describeClusterTracksResponse_maintenanceTracks :: Lens.Lens' DescribeClusterTracksResponse (Prelude.Maybe [MaintenanceTrack])
describeClusterTracksResponse_maintenanceTracks = Lens.lens (\DescribeClusterTracksResponse' {maintenanceTracks} -> maintenanceTracks) (\s@DescribeClusterTracksResponse' {} a -> s {maintenanceTracks = a} :: DescribeClusterTracksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The starting point to return a set of response tracklist records. You
-- can retrieve the next set of response records by providing the returned
-- marker value in the @Marker@ parameter and retrying the request.
describeClusterTracksResponse_marker :: Lens.Lens' DescribeClusterTracksResponse (Prelude.Maybe Prelude.Text)
describeClusterTracksResponse_marker = Lens.lens (\DescribeClusterTracksResponse' {marker} -> marker) (\s@DescribeClusterTracksResponse' {} a -> s {marker = a} :: DescribeClusterTracksResponse)

-- | The response's http status code.
describeClusterTracksResponse_httpStatus :: Lens.Lens' DescribeClusterTracksResponse Prelude.Int
describeClusterTracksResponse_httpStatus = Lens.lens (\DescribeClusterTracksResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterTracksResponse' {} a -> s {httpStatus = a} :: DescribeClusterTracksResponse)

instance Prelude.NFData DescribeClusterTracksResponse where
  rnf DescribeClusterTracksResponse' {..} =
    Prelude.rnf maintenanceTracks
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

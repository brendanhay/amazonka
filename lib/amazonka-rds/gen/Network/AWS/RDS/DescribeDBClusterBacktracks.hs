{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterBacktracks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about backtracks for a DB cluster.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterBacktracks
  ( -- * Creating a request
    DescribeDBClusterBacktracks (..),
    mkDescribeDBClusterBacktracks,

    -- ** Request lenses
    ddcbBacktrackIdentifier,
    ddcbFilters,
    ddcbMarker,
    ddcbMaxRecords,
    ddcbDBClusterIdentifier,

    -- * Destructuring the response
    DescribeDBClusterBacktracksResponse (..),
    mkDescribeDBClusterBacktracksResponse,

    -- ** Response lenses
    ddcbrsMarker,
    ddcbrsDBClusterBacktracks,
    ddcbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeDBClusterBacktracks' smart constructor.
data DescribeDBClusterBacktracks = DescribeDBClusterBacktracks'
  { backtrackIdentifier ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    dbClusterIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusterBacktracks' with the minimum fields required to make a request.
--
-- * 'backtrackIdentifier' - If specified, this value is the backtrack identifier of the backtrack to be described.
--
-- Constraints:
--
--     * Must contain a valid universally unique identifier (UUID). For more information about UUIDs, see <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace> .
--
--
-- Example: @123e4567-e89b-12d3-a456-426655440000@
-- * 'dbClusterIdentifier' - The DB cluster identifier of the DB cluster to be described. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1@
-- * 'filters' - A filter that specifies one or more DB clusters to describe. Supported filters include the following:
--
--
--     * @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The results list includes information about only the backtracks identified by these identifiers.
--
--
--     * @db-cluster-backtrack-status@ - Accepts any of the following backtrack status values:
--
--     * @applying@
--
--
--     * @completed@
--
--
--     * @failed@
--
--
--     * @pending@
--
--
-- The results list includes information about only the backtracks identified by these values.
--
--
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterBacktracks@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBClusterBacktracks ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  DescribeDBClusterBacktracks
mkDescribeDBClusterBacktracks pDBClusterIdentifier_ =
  DescribeDBClusterBacktracks'
    { backtrackIdentifier = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | If specified, this value is the backtrack identifier of the backtrack to be described.
--
-- Constraints:
--
--     * Must contain a valid universally unique identifier (UUID). For more information about UUIDs, see <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace> .
--
--
-- Example: @123e4567-e89b-12d3-a456-426655440000@
--
-- /Note:/ Consider using 'backtrackIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbBacktrackIdentifier :: Lens.Lens' DescribeDBClusterBacktracks (Lude.Maybe Lude.Text)
ddcbBacktrackIdentifier = Lens.lens (backtrackIdentifier :: DescribeDBClusterBacktracks -> Lude.Maybe Lude.Text) (\s a -> s {backtrackIdentifier = a} :: DescribeDBClusterBacktracks)
{-# DEPRECATED ddcbBacktrackIdentifier "Use generic-lens or generic-optics with 'backtrackIdentifier' instead." #-}

-- | A filter that specifies one or more DB clusters to describe. Supported filters include the following:
--
--
--     * @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The results list includes information about only the backtracks identified by these identifiers.
--
--
--     * @db-cluster-backtrack-status@ - Accepts any of the following backtrack status values:
--
--     * @applying@
--
--
--     * @completed@
--
--
--     * @failed@
--
--
--     * @pending@
--
--
-- The results list includes information about only the backtracks identified by these values.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbFilters :: Lens.Lens' DescribeDBClusterBacktracks (Lude.Maybe [Filter])
ddcbFilters = Lens.lens (filters :: DescribeDBClusterBacktracks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBClusterBacktracks)
{-# DEPRECATED ddcbFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterBacktracks@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbMarker :: Lens.Lens' DescribeDBClusterBacktracks (Lude.Maybe Lude.Text)
ddcbMarker = Lens.lens (marker :: DescribeDBClusterBacktracks -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterBacktracks)
{-# DEPRECATED ddcbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbMaxRecords :: Lens.Lens' DescribeDBClusterBacktracks (Lude.Maybe Lude.Int)
ddcbMaxRecords = Lens.lens (maxRecords :: DescribeDBClusterBacktracks -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBClusterBacktracks)
{-# DEPRECATED ddcbMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The DB cluster identifier of the DB cluster to be described. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1@
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbDBClusterIdentifier :: Lens.Lens' DescribeDBClusterBacktracks Lude.Text
ddcbDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DescribeDBClusterBacktracks -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DescribeDBClusterBacktracks)
{-# DEPRECATED ddcbDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

instance Page.AWSPager DescribeDBClusterBacktracks where
  page rq rs
    | Page.stop (rs Lens.^. ddcbrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcbrsDBClusterBacktracks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcbMarker Lens..~ rs Lens.^. ddcbrsMarker

instance Lude.AWSRequest DescribeDBClusterBacktracks where
  type
    Rs DescribeDBClusterBacktracks =
      DescribeDBClusterBacktracksResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBClusterBacktracksResult"
      ( \s h x ->
          DescribeDBClusterBacktracksResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "DBClusterBacktracks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBClusterBacktrack")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBClusterBacktracks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBClusterBacktracks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBClusterBacktracks where
  toQuery DescribeDBClusterBacktracks' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBClusterBacktracks" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "BacktrackIdentifier" Lude.=: backtrackIdentifier,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier
      ]

-- | Contains the result of a successful invocation of the @DescribeDBClusterBacktracks@ action.
--
-- /See:/ 'mkDescribeDBClusterBacktracksResponse' smart constructor.
data DescribeDBClusterBacktracksResponse = DescribeDBClusterBacktracksResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    dbClusterBacktracks ::
      Lude.Maybe
        [DBClusterBacktrack],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusterBacktracksResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterBacktracks' - Contains a list of backtracks for the user.
-- * 'marker' - A pagination token that can be used in a later @DescribeDBClusterBacktracks@ request.
-- * 'responseStatus' - The response status code.
mkDescribeDBClusterBacktracksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBClusterBacktracksResponse
mkDescribeDBClusterBacktracksResponse pResponseStatus_ =
  DescribeDBClusterBacktracksResponse'
    { marker = Lude.Nothing,
      dbClusterBacktracks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token that can be used in a later @DescribeDBClusterBacktracks@ request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbrsMarker :: Lens.Lens' DescribeDBClusterBacktracksResponse (Lude.Maybe Lude.Text)
ddcbrsMarker = Lens.lens (marker :: DescribeDBClusterBacktracksResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterBacktracksResponse)
{-# DEPRECATED ddcbrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Contains a list of backtracks for the user.
--
-- /Note:/ Consider using 'dbClusterBacktracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbrsDBClusterBacktracks :: Lens.Lens' DescribeDBClusterBacktracksResponse (Lude.Maybe [DBClusterBacktrack])
ddcbrsDBClusterBacktracks = Lens.lens (dbClusterBacktracks :: DescribeDBClusterBacktracksResponse -> Lude.Maybe [DBClusterBacktrack]) (\s a -> s {dbClusterBacktracks = a} :: DescribeDBClusterBacktracksResponse)
{-# DEPRECATED ddcbrsDBClusterBacktracks "Use generic-lens or generic-optics with 'dbClusterBacktracks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcbrsResponseStatus :: Lens.Lens' DescribeDBClusterBacktracksResponse Lude.Int
ddcbrsResponseStatus = Lens.lens (responseStatus :: DescribeDBClusterBacktracksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBClusterBacktracksResponse)
{-# DEPRECATED ddcbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays backups for both current and deleted instances. For example, use this operation to find details about automated backups for previously deleted instances. Current instances with retention periods greater than zero (0) are returned for both the @DescribeDBInstanceAutomatedBackups@ and @DescribeDBInstances@ operations.
--
-- All parameters are optional.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
  ( -- * Creating a request
    DescribeDBInstanceAutomatedBackups (..),
    mkDescribeDBInstanceAutomatedBackups,

    -- ** Request lenses
    ddiabFilters,
    ddiabDBInstanceIdentifier,
    ddiabMarker,
    ddiabMaxRecords,
    ddiabDBiResourceId,

    -- * Destructuring the response
    DescribeDBInstanceAutomatedBackupsResponse (..),
    mkDescribeDBInstanceAutomatedBackupsResponse,

    -- ** Response lenses
    ddiabrsDBInstanceAutomatedBackups,
    ddiabrsMarker,
    ddiabrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Parameter input for DescribeDBInstanceAutomatedBackups.
--
-- /See:/ 'mkDescribeDBInstanceAutomatedBackups' smart constructor.
data DescribeDBInstanceAutomatedBackups = DescribeDBInstanceAutomatedBackups'
  { -- | A filter that specifies which resources to return based on status.
    --
    -- Supported filters are the following:
    --
    --     * @status@
    --
    --     * @active@ - automated backups for current instances
    --
    --
    --     * @retained@ - automated backups for deleted instances
    --
    --
    --     * @creating@ - automated backups that are waiting for the first automated snapshot to be available
    --
    --
    --
    --
    --     * @db-instance-id@ - Accepts DB instance identifiers and Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance automated backupss identified by these ARNs.
    --
    --
    --     * @dbi-resource-id@ - Accepts DB instance resource identifiers and DB Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance resources identified by these ARNs.
    --
    --
    -- Returns all resources by default. The status for each resource is specified in the response.
    filters :: Lude.Maybe [Filter],
    -- | (Optional) The user-supplied instance identifier. If this parameter is specified, it must match the identifier of an existing DB instance. It returns information from the specific DB instance' automated backup. This parameter isn't case-sensitive.
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    -- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The resource ID of the DB instance that is the source of the automated backup. This parameter isn't case-sensitive.
    dbiResourceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBInstanceAutomatedBackups' with the minimum fields required to make a request.
--
-- * 'filters' - A filter that specifies which resources to return based on status.
--
-- Supported filters are the following:
--
--     * @status@
--
--     * @active@ - automated backups for current instances
--
--
--     * @retained@ - automated backups for deleted instances
--
--
--     * @creating@ - automated backups that are waiting for the first automated snapshot to be available
--
--
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance automated backupss identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers and DB Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance resources identified by these ARNs.
--
--
-- Returns all resources by default. The status for each resource is specified in the response.
-- * 'dbInstanceIdentifier' - (Optional) The user-supplied instance identifier. If this parameter is specified, it must match the identifier of an existing DB instance. It returns information from the specific DB instance' automated backup. This parameter isn't case-sensitive.
-- * 'marker' - The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
-- * 'dbiResourceId' - The resource ID of the DB instance that is the source of the automated backup. This parameter isn't case-sensitive.
mkDescribeDBInstanceAutomatedBackups ::
  DescribeDBInstanceAutomatedBackups
mkDescribeDBInstanceAutomatedBackups =
  DescribeDBInstanceAutomatedBackups'
    { filters = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbiResourceId = Lude.Nothing
    }

-- | A filter that specifies which resources to return based on status.
--
-- Supported filters are the following:
--
--     * @status@
--
--     * @active@ - automated backups for current instances
--
--
--     * @retained@ - automated backups for deleted instances
--
--
--     * @creating@ - automated backups that are waiting for the first automated snapshot to be available
--
--
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance automated backupss identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers and DB Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance resources identified by these ARNs.
--
--
-- Returns all resources by default. The status for each resource is specified in the response.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabFilters :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Lude.Maybe [Filter])
ddiabFilters = Lens.lens (filters :: DescribeDBInstanceAutomatedBackups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBInstanceAutomatedBackups)
{-# DEPRECATED ddiabFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | (Optional) The user-supplied instance identifier. If this parameter is specified, it must match the identifier of an existing DB instance. It returns information from the specific DB instance' automated backup. This parameter isn't case-sensitive.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabDBInstanceIdentifier :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Lude.Maybe Lude.Text)
ddiabDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DescribeDBInstanceAutomatedBackups -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DescribeDBInstanceAutomatedBackups)
{-# DEPRECATED ddiabDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabMarker :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Lude.Maybe Lude.Text)
ddiabMarker = Lens.lens (marker :: DescribeDBInstanceAutomatedBackups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBInstanceAutomatedBackups)
{-# DEPRECATED ddiabMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabMaxRecords :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Lude.Maybe Lude.Int)
ddiabMaxRecords = Lens.lens (maxRecords :: DescribeDBInstanceAutomatedBackups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBInstanceAutomatedBackups)
{-# DEPRECATED ddiabMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The resource ID of the DB instance that is the source of the automated backup. This parameter isn't case-sensitive.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabDBiResourceId :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Lude.Maybe Lude.Text)
ddiabDBiResourceId = Lens.lens (dbiResourceId :: DescribeDBInstanceAutomatedBackups -> Lude.Maybe Lude.Text) (\s a -> s {dbiResourceId = a} :: DescribeDBInstanceAutomatedBackups)
{-# DEPRECATED ddiabDBiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

instance Page.AWSPager DescribeDBInstanceAutomatedBackups where
  page rq rs
    | Page.stop (rs Lens.^. ddiabrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddiabrsDBInstanceAutomatedBackups) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddiabMarker Lens..~ rs Lens.^. ddiabrsMarker

instance Lude.AWSRequest DescribeDBInstanceAutomatedBackups where
  type
    Rs DescribeDBInstanceAutomatedBackups =
      DescribeDBInstanceAutomatedBackupsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBInstanceAutomatedBackupsResult"
      ( \s h x ->
          DescribeDBInstanceAutomatedBackupsResponse'
            Lude.<$> ( x Lude..@? "DBInstanceAutomatedBackups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBInstanceAutomatedBackup")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBInstanceAutomatedBackups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBInstanceAutomatedBackups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBInstanceAutomatedBackups where
  toQuery DescribeDBInstanceAutomatedBackups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBInstanceAutomatedBackups" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DbiResourceId" Lude.=: dbiResourceId
      ]

-- | Contains the result of a successful invocation of the @DescribeDBInstanceAutomatedBackups@ action.
--
-- /See:/ 'mkDescribeDBInstanceAutomatedBackupsResponse' smart constructor.
data DescribeDBInstanceAutomatedBackupsResponse = DescribeDBInstanceAutomatedBackupsResponse'
  { -- | A list of @DBInstanceAutomatedBackup@ instances.
    dbInstanceAutomatedBackups :: Lude.Maybe [DBInstanceAutomatedBackup],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBInstanceAutomatedBackupsResponse' with the minimum fields required to make a request.
--
-- * 'dbInstanceAutomatedBackups' - A list of @DBInstanceAutomatedBackup@ instances.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBInstanceAutomatedBackupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBInstanceAutomatedBackupsResponse
mkDescribeDBInstanceAutomatedBackupsResponse pResponseStatus_ =
  DescribeDBInstanceAutomatedBackupsResponse'
    { dbInstanceAutomatedBackups =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @DBInstanceAutomatedBackup@ instances.
--
-- /Note:/ Consider using 'dbInstanceAutomatedBackups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabrsDBInstanceAutomatedBackups :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Lude.Maybe [DBInstanceAutomatedBackup])
ddiabrsDBInstanceAutomatedBackups = Lens.lens (dbInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackupsResponse -> Lude.Maybe [DBInstanceAutomatedBackup]) (\s a -> s {dbInstanceAutomatedBackups = a} :: DescribeDBInstanceAutomatedBackupsResponse)
{-# DEPRECATED ddiabrsDBInstanceAutomatedBackups "Use generic-lens or generic-optics with 'dbInstanceAutomatedBackups' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabrsMarker :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Lude.Maybe Lude.Text)
ddiabrsMarker = Lens.lens (marker :: DescribeDBInstanceAutomatedBackupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBInstanceAutomatedBackupsResponse)
{-# DEPRECATED ddiabrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddiabrsResponseStatus :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse Lude.Int
ddiabrsResponseStatus = Lens.lens (responseStatus :: DescribeDBInstanceAutomatedBackupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBInstanceAutomatedBackupsResponse)
{-# DEPRECATED ddiabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

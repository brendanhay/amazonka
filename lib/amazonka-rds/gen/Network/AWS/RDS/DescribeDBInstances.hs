{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned RDS instances. This API supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstances
  ( -- * Creating a request
    DescribeDBInstances (..),
    mkDescribeDBInstances,

    -- ** Request lenses
    ddbiFilters,
    ddbiDBInstanceIdentifier,
    ddbiMarker,
    ddbiMaxRecords,

    -- * Destructuring the response
    DescribeDBInstancesResponse (..),
    mkDescribeDBInstancesResponse,

    -- ** Response lenses
    ddbirsDBInstances,
    ddbirsMarker,
    ddbirsResponseStatus,
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
-- /See:/ 'mkDescribeDBInstances' smart constructor.
data DescribeDBInstances = DescribeDBInstances'
  { filters ::
      Lude.Maybe [Filter],
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBInstances' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
-- * 'filters' - A filter that specifies one or more DB instances to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB instances associated with the DB clusters identified by these ARNs.
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs). The results list will only include information about the DB instances identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers. The results list will only include information about the DB instances identified by these DB instance resource identifiers.
--
--
--     * @domain@ - Accepts Active Directory directory IDs. The results list will only include information about the DB instances associated with these domains.
--
--
--     * @engine@ - Accepts engine names. The results list will only include information about the DB instances for these engines.
--
--
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBInstances ::
  DescribeDBInstances
mkDescribeDBInstances =
  DescribeDBInstances'
    { filters = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A filter that specifies one or more DB instances to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB instances associated with the DB clusters identified by these ARNs.
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs). The results list will only include information about the DB instances identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers. The results list will only include information about the DB instances identified by these DB instance resource identifiers.
--
--
--     * @domain@ - Accepts Active Directory directory IDs. The results list will only include information about the DB instances associated with these domains.
--
--
--     * @engine@ - Accepts engine names. The results list will only include information about the DB instances for these engines.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiFilters :: Lens.Lens' DescribeDBInstances (Lude.Maybe [Filter])
ddbiFilters = Lens.lens (filters :: DescribeDBInstances -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBInstances)
{-# DEPRECATED ddbiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiDBInstanceIdentifier :: Lens.Lens' DescribeDBInstances (Lude.Maybe Lude.Text)
ddbiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DescribeDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DescribeDBInstances)
{-# DEPRECATED ddbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiMarker :: Lens.Lens' DescribeDBInstances (Lude.Maybe Lude.Text)
ddbiMarker = Lens.lens (marker :: DescribeDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBInstances)
{-# DEPRECATED ddbiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiMaxRecords :: Lens.Lens' DescribeDBInstances (Lude.Maybe Lude.Int)
ddbiMaxRecords = Lens.lens (maxRecords :: DescribeDBInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBInstances)
{-# DEPRECATED ddbiMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeDBInstances where
  page rq rs
    | Page.stop (rs Lens.^. ddbirsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddbirsDBInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddbiMarker Lens..~ rs Lens.^. ddbirsMarker

instance Lude.AWSRequest DescribeDBInstances where
  type Rs DescribeDBInstances = DescribeDBInstancesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBInstancesResult"
      ( \s h x ->
          DescribeDBInstancesResponse'
            Lude.<$> ( x Lude..@? "DBInstances" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBInstance")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBInstances where
  toQuery DescribeDBInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the @DescribeDBInstances@ action.
--
-- /See:/ 'mkDescribeDBInstancesResponse' smart constructor.
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
  { dbInstances ::
      Lude.Maybe [DBInstance],
    marker :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBInstancesResponse' with the minimum fields required to make a request.
--
-- * 'dbInstances' - A list of @DBInstance@ instances.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBInstancesResponse
mkDescribeDBInstancesResponse pResponseStatus_ =
  DescribeDBInstancesResponse'
    { dbInstances = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @DBInstance@ instances.
--
-- /Note:/ Consider using 'dbInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirsDBInstances :: Lens.Lens' DescribeDBInstancesResponse (Lude.Maybe [DBInstance])
ddbirsDBInstances = Lens.lens (dbInstances :: DescribeDBInstancesResponse -> Lude.Maybe [DBInstance]) (\s a -> s {dbInstances = a} :: DescribeDBInstancesResponse)
{-# DEPRECATED ddbirsDBInstances "Use generic-lens or generic-optics with 'dbInstances' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirsMarker :: Lens.Lens' DescribeDBInstancesResponse (Lude.Maybe Lude.Text)
ddbirsMarker = Lens.lens (marker :: DescribeDBInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBInstancesResponse)
{-# DEPRECATED ddbirsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirsResponseStatus :: Lens.Lens' DescribeDBInstancesResponse Lude.Int
ddbirsResponseStatus = Lens.lens (responseStatus :: DescribeDBInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBInstancesResponse)
{-# DEPRECATED ddbirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

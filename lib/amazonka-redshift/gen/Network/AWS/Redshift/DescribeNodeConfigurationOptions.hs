{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeNodeConfigurationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns properties of possible node configurations such as node type, number of nodes, and disk usage for the specified action type.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeNodeConfigurationOptions
  ( -- * Creating a request
    DescribeNodeConfigurationOptions (..),
    mkDescribeNodeConfigurationOptions,

    -- ** Request lenses
    dncoActionType,
    dncoClusterIdentifier,
    dncoFilters,
    dncoMarker,
    dncoMaxRecords,
    dncoOwnerAccount,
    dncoSnapshotIdentifier,

    -- * Destructuring the response
    DescribeNodeConfigurationOptionsResponse (..),
    mkDescribeNodeConfigurationOptionsResponse,

    -- ** Response lenses
    dncorrsMarker,
    dncorrsNodeConfigurationOptionList,
    dncorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeNodeConfigurationOptions' smart constructor.
data DescribeNodeConfigurationOptions = DescribeNodeConfigurationOptions'
  { -- | The action type to evaluate for possible node configurations. Specify "restore-cluster" to get configuration combinations based on an existing snapshot. Specify "recommend-node-config" to get configuration recommendations based on an existing cluster or snapshot. Specify "resize-cluster" to get configuration combinations for elastic resize based on an existing cluster.
    actionType :: Types.ActionType,
    -- | The identifier of the cluster to evaluate for possible node configurations.
    clusterIdentifier :: Core.Maybe Types.String,
    -- | A set of name, operator, and value items to filter the results.
    filters :: Core.Maybe [Types.NodeConfigurationOptionsFilter],
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeNodeConfigurationOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @500@
    -- Constraints: minimum 100, maximum 500.
    maxRecords :: Core.Maybe Core.Int,
    -- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
    ownerAccount :: Core.Maybe Types.String,
    -- | The identifier of the snapshot to evaluate for possible node configurations.
    snapshotIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNodeConfigurationOptions' value with any optional fields omitted.
mkDescribeNodeConfigurationOptions ::
  -- | 'actionType'
  Types.ActionType ->
  DescribeNodeConfigurationOptions
mkDescribeNodeConfigurationOptions actionType =
  DescribeNodeConfigurationOptions'
    { actionType,
      clusterIdentifier = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      ownerAccount = Core.Nothing,
      snapshotIdentifier = Core.Nothing
    }

-- | The action type to evaluate for possible node configurations. Specify "restore-cluster" to get configuration combinations based on an existing snapshot. Specify "recommend-node-config" to get configuration recommendations based on an existing cluster or snapshot. Specify "resize-cluster" to get configuration combinations for elastic resize based on an existing cluster.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoActionType :: Lens.Lens' DescribeNodeConfigurationOptions Types.ActionType
dncoActionType = Lens.field @"actionType"
{-# DEPRECATED dncoActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The identifier of the cluster to evaluate for possible node configurations.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoClusterIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Types.String)
dncoClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED dncoClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A set of name, operator, and value items to filter the results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoFilters :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe [Types.NodeConfigurationOptionsFilter])
dncoFilters = Lens.field @"filters"
{-# DEPRECATED dncoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeNodeConfigurationOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoMarker :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Types.String)
dncoMarker = Lens.field @"marker"
{-# DEPRECATED dncoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @500@
-- Constraints: minimum 100, maximum 500.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoMaxRecords :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Int)
dncoMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dncoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoOwnerAccount :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Types.String)
dncoOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED dncoOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The identifier of the snapshot to evaluate for possible node configurations.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoSnapshotIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Types.String)
dncoSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED dncoSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

instance Core.AWSRequest DescribeNodeConfigurationOptions where
  type
    Rs DescribeNodeConfigurationOptions =
      DescribeNodeConfigurationOptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeNodeConfigurationOptions")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ActionType" actionType)
                Core.<> (Core.toQueryValue "ClusterIdentifier" Core.<$> clusterIdentifier)
                Core.<> ( Core.toQueryValue
                            "Filter"
                            ( Core.toQueryList "NodeConfigurationOptionsFilter"
                                Core.<$> filters
                            )
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "OwnerAccount" Core.<$> ownerAccount)
                Core.<> ( Core.toQueryValue "SnapshotIdentifier"
                            Core.<$> snapshotIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeNodeConfigurationOptionsResult"
      ( \s h x ->
          DescribeNodeConfigurationOptionsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "NodeConfigurationOptionList"
                         Core..<@> Core.parseXMLList "NodeConfigurationOption"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeNodeConfigurationOptions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"nodeConfigurationOptionList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeNodeConfigurationOptionsResponse' smart constructor.
data DescribeNodeConfigurationOptionsResponse = DescribeNodeConfigurationOptionsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | A list of valid node configurations.
    nodeConfigurationOptionList :: Core.Maybe [Types.NodeConfigurationOption],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNodeConfigurationOptionsResponse' value with any optional fields omitted.
mkDescribeNodeConfigurationOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeNodeConfigurationOptionsResponse
mkDescribeNodeConfigurationOptionsResponse responseStatus =
  DescribeNodeConfigurationOptionsResponse'
    { marker = Core.Nothing,
      nodeConfigurationOptionList = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorrsMarker :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Core.Maybe Types.String)
dncorrsMarker = Lens.field @"marker"
{-# DEPRECATED dncorrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of valid node configurations.
--
-- /Note:/ Consider using 'nodeConfigurationOptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorrsNodeConfigurationOptionList :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Core.Maybe [Types.NodeConfigurationOption])
dncorrsNodeConfigurationOptionList = Lens.field @"nodeConfigurationOptionList"
{-# DEPRECATED dncorrsNodeConfigurationOptionList "Use generic-lens or generic-optics with 'nodeConfigurationOptionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorrsResponseStatus :: Lens.Lens' DescribeNodeConfigurationOptionsResponse Core.Int
dncorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dncorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

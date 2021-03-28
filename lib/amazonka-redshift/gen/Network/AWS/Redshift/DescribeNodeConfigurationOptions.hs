{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeNodeConfigurationOptions (..)
    , mkDescribeNodeConfigurationOptions
    -- ** Request lenses
    , dncoActionType
    , dncoClusterIdentifier
    , dncoFilters
    , dncoMarker
    , dncoMaxRecords
    , dncoOwnerAccount
    , dncoSnapshotIdentifier

    -- * Destructuring the response
    , DescribeNodeConfigurationOptionsResponse (..)
    , mkDescribeNodeConfigurationOptionsResponse
    -- ** Response lenses
    , dncorrsMarker
    , dncorrsNodeConfigurationOptionList
    , dncorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeNodeConfigurationOptions' smart constructor.
data DescribeNodeConfigurationOptions = DescribeNodeConfigurationOptions'
  { actionType :: Types.ActionType
    -- ^ The action type to evaluate for possible node configurations. Specify "restore-cluster" to get configuration combinations based on an existing snapshot. Specify "recommend-node-config" to get configuration recommendations based on an existing cluster or snapshot. Specify "resize-cluster" to get configuration combinations for elastic resize based on an existing cluster. 
  , clusterIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the cluster to evaluate for possible node configurations.
  , filters :: Core.Maybe [Types.NodeConfigurationOptionsFilter]
    -- ^ A set of name, operator, and value items to filter the results.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeNodeConfigurationOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @500@ 
-- Constraints: minimum 100, maximum 500.
  , ownerAccount :: Core.Maybe Core.Text
    -- ^ The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
  , snapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the snapshot to evaluate for possible node configurations.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNodeConfigurationOptions' value with any optional fields omitted.
mkDescribeNodeConfigurationOptions
    :: Types.ActionType -- ^ 'actionType'
    -> DescribeNodeConfigurationOptions
mkDescribeNodeConfigurationOptions actionType
  = DescribeNodeConfigurationOptions'{actionType,
                                      clusterIdentifier = Core.Nothing, filters = Core.Nothing,
                                      marker = Core.Nothing, maxRecords = Core.Nothing,
                                      ownerAccount = Core.Nothing,
                                      snapshotIdentifier = Core.Nothing}

-- | The action type to evaluate for possible node configurations. Specify "restore-cluster" to get configuration combinations based on an existing snapshot. Specify "recommend-node-config" to get configuration recommendations based on an existing cluster or snapshot. Specify "resize-cluster" to get configuration combinations for elastic resize based on an existing cluster. 
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoActionType :: Lens.Lens' DescribeNodeConfigurationOptions Types.ActionType
dncoActionType = Lens.field @"actionType"
{-# INLINEABLE dncoActionType #-}
{-# DEPRECATED actionType "Use generic-lens or generic-optics with 'actionType' instead"  #-}

-- | The identifier of the cluster to evaluate for possible node configurations.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoClusterIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
dncoClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE dncoClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | A set of name, operator, and value items to filter the results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoFilters :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe [Types.NodeConfigurationOptionsFilter])
dncoFilters = Lens.field @"filters"
{-# INLINEABLE dncoFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeNodeConfigurationOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoMarker :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
dncoMarker = Lens.field @"marker"
{-# INLINEABLE dncoMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @500@ 
-- Constraints: minimum 100, maximum 500.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoMaxRecords :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Int)
dncoMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dncoMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoOwnerAccount :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
dncoOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE dncoOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | The identifier of the snapshot to evaluate for possible node configurations.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoSnapshotIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
dncoSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE dncoSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

instance Core.ToQuery DescribeNodeConfigurationOptions where
        toQuery DescribeNodeConfigurationOptions{..}
          = Core.toQueryPair "Action"
              ("DescribeNodeConfigurationOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ActionType" actionType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterIdentifier")
                clusterIdentifier
              Core.<>
              Core.toQueryPair "Filter"
                (Core.maybe Core.mempty
                   (Core.toQueryList "NodeConfigurationOptionsFilter")
                   filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OwnerAccount")
                ownerAccount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotIdentifier")
                snapshotIdentifier

instance Core.ToHeaders DescribeNodeConfigurationOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeNodeConfigurationOptions where
        type Rs DescribeNodeConfigurationOptions =
             DescribeNodeConfigurationOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "DescribeNodeConfigurationOptionsResult"
              (\ s h x ->
                 DescribeNodeConfigurationOptionsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "NodeConfigurationOptionList" Core..<@>
                       Core.parseXMLList "NodeConfigurationOption"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeNodeConfigurationOptions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"nodeConfigurationOptionList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeNodeConfigurationOptionsResponse' smart constructor.
data DescribeNodeConfigurationOptionsResponse = DescribeNodeConfigurationOptionsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
  , nodeConfigurationOptionList :: Core.Maybe [Types.NodeConfigurationOption]
    -- ^ A list of valid node configurations.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNodeConfigurationOptionsResponse' value with any optional fields omitted.
mkDescribeNodeConfigurationOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNodeConfigurationOptionsResponse
mkDescribeNodeConfigurationOptionsResponse responseStatus
  = DescribeNodeConfigurationOptionsResponse'{marker = Core.Nothing,
                                              nodeConfigurationOptionList = Core.Nothing,
                                              responseStatus}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorrsMarker :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Core.Maybe Core.Text)
dncorrsMarker = Lens.field @"marker"
{-# INLINEABLE dncorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of valid node configurations.
--
-- /Note:/ Consider using 'nodeConfigurationOptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorrsNodeConfigurationOptionList :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Core.Maybe [Types.NodeConfigurationOption])
dncorrsNodeConfigurationOptionList = Lens.field @"nodeConfigurationOptionList"
{-# INLINEABLE dncorrsNodeConfigurationOptionList #-}
{-# DEPRECATED nodeConfigurationOptionList "Use generic-lens or generic-optics with 'nodeConfigurationOptionList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorrsResponseStatus :: Lens.Lens' DescribeNodeConfigurationOptionsResponse Core.Int
dncorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dncorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

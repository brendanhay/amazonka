{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeResize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the last resize operation for the specified cluster. If no resize operation has ever been initiated for the specified cluster, a @HTTP 404@ error is returned. If a resize operation was initiated and completed, the status of the resize remains as @SUCCEEDED@ until the next resize. 
--
-- A resize operation can be requested using 'ModifyCluster' and specifying a different number or type of nodes for the cluster. 
module Network.AWS.Redshift.DescribeResize
    (
    -- * Creating a request
      DescribeResize (..)
    , mkDescribeResize
    -- ** Request lenses
    , drClusterIdentifier

     -- * Destructuring the response
    , Types.ResizeProgressMessage (..)
    , Types.mkResizeProgressMessage
    -- ** Response lenses
    , Types.rpmAvgResizeRateInMegaBytesPerSecond
    , Types.rpmDataTransferProgressPercent
    , Types.rpmElapsedTimeInSeconds
    , Types.rpmEstimatedTimeToCompletionInSeconds
    , Types.rpmImportTablesCompleted
    , Types.rpmImportTablesInProgress
    , Types.rpmImportTablesNotStarted
    , Types.rpmMessage
    , Types.rpmProgressInMegaBytes
    , Types.rpmResizeType
    , Types.rpmStatus
    , Types.rpmTargetClusterType
    , Types.rpmTargetEncryptionType
    , Types.rpmTargetNodeType
    , Types.rpmTargetNumberOfNodes
    , Types.rpmTotalResizeDataInMegaBytes
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeResize' smart constructor.
newtype DescribeResize = DescribeResize'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive.
--
-- By default, resize operations for all clusters defined for an AWS account are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResize' value with any optional fields omitted.
mkDescribeResize
    :: Core.Text -- ^ 'clusterIdentifier'
    -> DescribeResize
mkDescribeResize clusterIdentifier
  = DescribeResize'{clusterIdentifier}

-- | The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive.
--
-- By default, resize operations for all clusters defined for an AWS account are returned.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drClusterIdentifier :: Lens.Lens' DescribeResize Core.Text
drClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE drClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.ToQuery DescribeResize where
        toQuery DescribeResize{..}
          = Core.toQueryPair "Action" ("DescribeResize" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier

instance Core.ToHeaders DescribeResize where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeResize where
        type Rs DescribeResize = Types.ResizeProgressMessage
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
          = Response.receiveXMLWrapper "DescribeResizeResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

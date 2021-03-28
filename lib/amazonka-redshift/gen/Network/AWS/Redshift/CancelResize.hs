{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CancelResize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a resize operation for a cluster.
module Network.AWS.Redshift.CancelResize
    (
    -- * Creating a request
      CancelResize (..)
    , mkCancelResize
    -- ** Request lenses
    , crClusterIdentifier

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

-- | /See:/ 'mkCancelResize' smart constructor.
newtype CancelResize = CancelResize'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier for the cluster that you want to cancel a resize operation for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelResize' value with any optional fields omitted.
mkCancelResize
    :: Core.Text -- ^ 'clusterIdentifier'
    -> CancelResize
mkCancelResize clusterIdentifier = CancelResize'{clusterIdentifier}

-- | The unique identifier for the cluster that you want to cancel a resize operation for.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crClusterIdentifier :: Lens.Lens' CancelResize Core.Text
crClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE crClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.ToQuery CancelResize where
        toQuery CancelResize{..}
          = Core.toQueryPair "Action" ("CancelResize" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier

instance Core.ToHeaders CancelResize where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelResize where
        type Rs CancelResize = Types.ResizeProgressMessage
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
          = Response.receiveXMLWrapper "CancelResizeResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

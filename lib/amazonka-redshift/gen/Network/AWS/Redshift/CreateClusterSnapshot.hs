{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a manual snapshot of the specified cluster. The cluster must be in the @available@ state. 
--
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterSnapshot
    (
    -- * Creating a request
      CreateClusterSnapshot (..)
    , mkCreateClusterSnapshot
    -- ** Request lenses
    , ccsSnapshotIdentifier
    , ccsClusterIdentifier
    , ccsManualSnapshotRetentionPeriod
    , ccsTags

    -- * Destructuring the response
    , CreateClusterSnapshotResponse (..)
    , mkCreateClusterSnapshotResponse
    -- ** Response lenses
    , ccsrrsSnapshot
    , ccsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateClusterSnapshot' smart constructor.
data CreateClusterSnapshot = CreateClusterSnapshot'
  { snapshotIdentifier :: Core.Text
    -- ^ A unique identifier for the snapshot that you are requesting. This identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@ 
  , clusterIdentifier :: Core.Text
    -- ^ The cluster identifier for which you want a snapshot.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSnapshot' value with any optional fields omitted.
mkCreateClusterSnapshot
    :: Core.Text -- ^ 'snapshotIdentifier'
    -> Core.Text -- ^ 'clusterIdentifier'
    -> CreateClusterSnapshot
mkCreateClusterSnapshot snapshotIdentifier clusterIdentifier
  = CreateClusterSnapshot'{snapshotIdentifier, clusterIdentifier,
                           manualSnapshotRetentionPeriod = Core.Nothing, tags = Core.Nothing}

-- | A unique identifier for the snapshot that you are requesting. This identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@ 
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsSnapshotIdentifier :: Lens.Lens' CreateClusterSnapshot Core.Text
ccsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE ccsSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | The cluster identifier for which you want a snapshot.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsClusterIdentifier :: Lens.Lens' CreateClusterSnapshot Core.Text
ccsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE ccsClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsManualSnapshotRetentionPeriod :: Lens.Lens' CreateClusterSnapshot (Core.Maybe Core.Int)
ccsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE ccsManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTags :: Lens.Lens' CreateClusterSnapshot (Core.Maybe [Types.Tag])
ccsTags = Lens.field @"tags"
{-# INLINEABLE ccsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateClusterSnapshot where
        toQuery CreateClusterSnapshot{..}
          = Core.toQueryPair "Action" ("CreateClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "SnapshotIdentifier" snapshotIdentifier
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ManualSnapshotRetentionPeriod")
                manualSnapshotRetentionPeriod
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateClusterSnapshot where
        type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse
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
          = Response.receiveXMLWrapper "CreateClusterSnapshotResult"
              (\ s h x ->
                 CreateClusterSnapshotResponse' Core.<$>
                   (x Core..@? "Snapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClusterSnapshotResponse' smart constructor.
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateClusterSnapshotResponse' value with any optional fields omitted.
mkCreateClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClusterSnapshotResponse
mkCreateClusterSnapshotResponse responseStatus
  = CreateClusterSnapshotResponse'{snapshot = Core.Nothing,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsSnapshot :: Lens.Lens' CreateClusterSnapshotResponse (Core.Maybe Types.Snapshot)
ccsrrsSnapshot = Lens.field @"snapshot"
{-# INLINEABLE ccsrrsSnapshot #-}
{-# DEPRECATED snapshot "Use generic-lens or generic-optics with 'snapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsResponseStatus :: Lens.Lens' CreateClusterSnapshotResponse Core.Int
ccsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

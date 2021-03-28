{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB cluster. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.CreateDBClusterSnapshot
    (
    -- * Creating a request
      CreateDBClusterSnapshot (..)
    , mkCreateDBClusterSnapshot
    -- ** Request lenses
    , cdbcsDBClusterSnapshotIdentifier
    , cdbcsDBClusterIdentifier
    , cdbcsTags

    -- * Destructuring the response
    , CreateDBClusterSnapshotResponse (..)
    , mkCreateDBClusterSnapshotResponse
    -- ** Response lenses
    , cdbcsrrsDBClusterSnapshot
    , cdbcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateDBClusterSnapshot' smart constructor.
data CreateDBClusterSnapshot = CreateDBClusterSnapshot'
  { dBClusterSnapshotIdentifier :: Core.Text
    -- ^ The identifier of the DB cluster snapshot. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1-snapshot1@ 
  , dBClusterIdentifier :: Core.Text
    -- ^ The identifier of the DB cluster to create a snapshot for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
-- Example: @my-cluster1@ 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to be assigned to the DB cluster snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterSnapshot' value with any optional fields omitted.
mkCreateDBClusterSnapshot
    :: Core.Text -- ^ 'dBClusterSnapshotIdentifier'
    -> Core.Text -- ^ 'dBClusterIdentifier'
    -> CreateDBClusterSnapshot
mkCreateDBClusterSnapshot dBClusterSnapshotIdentifier
  dBClusterIdentifier
  = CreateDBClusterSnapshot'{dBClusterSnapshotIdentifier,
                             dBClusterIdentifier, tags = Core.Nothing}

-- | The identifier of the DB cluster snapshot. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1-snapshot1@ 
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsDBClusterSnapshotIdentifier :: Lens.Lens' CreateDBClusterSnapshot Core.Text
cdbcsDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# INLINEABLE cdbcsDBClusterSnapshotIdentifier #-}
{-# DEPRECATED dBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead"  #-}

-- | The identifier of the DB cluster to create a snapshot for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
-- Example: @my-cluster1@ 
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsDBClusterIdentifier :: Lens.Lens' CreateDBClusterSnapshot Core.Text
cdbcsDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE cdbcsDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The tags to be assigned to the DB cluster snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsTags :: Lens.Lens' CreateDBClusterSnapshot (Core.Maybe [Types.Tag])
cdbcsTags = Lens.field @"tags"
{-# INLINEABLE cdbcsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDBClusterSnapshot where
        toQuery CreateDBClusterSnapshot{..}
          = Core.toQueryPair "Action"
              ("CreateDBClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBClusterSnapshotIdentifier"
                dBClusterSnapshotIdentifier
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateDBClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBClusterSnapshot where
        type Rs CreateDBClusterSnapshot = CreateDBClusterSnapshotResponse
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
          = Response.receiveXMLWrapper "CreateDBClusterSnapshotResult"
              (\ s h x ->
                 CreateDBClusterSnapshotResponse' Core.<$>
                   (x Core..@? "DBClusterSnapshot") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDBClusterSnapshotResponse' smart constructor.
data CreateDBClusterSnapshotResponse = CreateDBClusterSnapshotResponse'
  { dBClusterSnapshot :: Core.Maybe Types.DBClusterSnapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDBClusterSnapshotResponse' value with any optional fields omitted.
mkCreateDBClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDBClusterSnapshotResponse
mkCreateDBClusterSnapshotResponse responseStatus
  = CreateDBClusterSnapshotResponse'{dBClusterSnapshot =
                                       Core.Nothing,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrrsDBClusterSnapshot :: Lens.Lens' CreateDBClusterSnapshotResponse (Core.Maybe Types.DBClusterSnapshot)
cdbcsrrsDBClusterSnapshot = Lens.field @"dBClusterSnapshot"
{-# INLINEABLE cdbcsrrsDBClusterSnapshot #-}
{-# DEPRECATED dBClusterSnapshot "Use generic-lens or generic-optics with 'dBClusterSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrrsResponseStatus :: Lens.Lens' CreateDBClusterSnapshotResponse Core.Int
cdbcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

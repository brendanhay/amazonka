{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB instance. The source DB instance must be in the @available@ or @storage-optimization@ state.
module Network.AWS.RDS.CreateDBSnapshot
    (
    -- * Creating a request
      CreateDBSnapshot (..)
    , mkCreateDBSnapshot
    -- ** Request lenses
    , cDBSnapshotIdentifier
    , cDBInstanceIdentifier
    , cTags

    -- * Destructuring the response
    , CreateDBSnapshotResponse (..)
    , mkCreateDBSnapshotResponse
    -- ** Response lenses
    , crsDBSnapshot
    , crsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateDBSnapshot' smart constructor.
data CreateDBSnapshot = CreateDBSnapshot'
  { dBSnapshotIdentifier :: Core.Text
    -- ^ The identifier for the DB snapshot.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@ 
  , dBInstanceIdentifier :: Core.Text
    -- ^ The identifier of the DB instance that you want to create the snapshot of.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
  , tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBSnapshot' value with any optional fields omitted.
mkCreateDBSnapshot
    :: Core.Text -- ^ 'dBSnapshotIdentifier'
    -> Core.Text -- ^ 'dBInstanceIdentifier'
    -> CreateDBSnapshot
mkCreateDBSnapshot dBSnapshotIdentifier dBInstanceIdentifier
  = CreateDBSnapshot'{dBSnapshotIdentifier, dBInstanceIdentifier,
                      tags = Core.Nothing}

-- | The identifier for the DB snapshot.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@ 
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDBSnapshotIdentifier :: Lens.Lens' CreateDBSnapshot Core.Text
cDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE cDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

-- | The identifier of the DB instance that you want to create the snapshot of.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDBInstanceIdentifier :: Lens.Lens' CreateDBSnapshot Core.Text
cDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE cDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateDBSnapshot (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDBSnapshot where
        toQuery CreateDBSnapshot{..}
          = Core.toQueryPair "Action" ("CreateDBSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBSnapshotIdentifier" dBSnapshotIdentifier
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateDBSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBSnapshot where
        type Rs CreateDBSnapshot = CreateDBSnapshotResponse
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
          = Response.receiveXMLWrapper "CreateDBSnapshotResult"
              (\ s h x ->
                 CreateDBSnapshotResponse' Core.<$>
                   (x Core..@? "DBSnapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDBSnapshotResponse' smart constructor.
data CreateDBSnapshotResponse = CreateDBSnapshotResponse'
  { dBSnapshot :: Core.Maybe Types.DBSnapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDBSnapshotResponse' value with any optional fields omitted.
mkCreateDBSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDBSnapshotResponse
mkCreateDBSnapshotResponse responseStatus
  = CreateDBSnapshotResponse'{dBSnapshot = Core.Nothing,
                              responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDBSnapshot :: Lens.Lens' CreateDBSnapshotResponse (Core.Maybe Types.DBSnapshot)
crsDBSnapshot = Lens.field @"dBSnapshot"
{-# INLINEABLE crsDBSnapshot #-}
{-# DEPRECATED dBSnapshot "Use generic-lens or generic-optics with 'dBSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateDBSnapshotResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

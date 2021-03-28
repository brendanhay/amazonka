{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a manual DB snapshot with a new engine version. The snapshot can be encrypted or unencrypted, but not shared or public. 
--
-- Amazon RDS supports upgrading DB snapshots for MySQL, Oracle, and PostgreSQL. 
module Network.AWS.RDS.ModifyDBSnapshot
    (
    -- * Creating a request
      ModifyDBSnapshot (..)
    , mkModifyDBSnapshot
    -- ** Request lenses
    , mdbsDBSnapshotIdentifier
    , mdbsEngineVersion
    , mdbsOptionGroupName

    -- * Destructuring the response
    , ModifyDBSnapshotResponse (..)
    , mkModifyDBSnapshotResponse
    -- ** Response lenses
    , mdbsrrsDBSnapshot
    , mdbsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyDBSnapshot' smart constructor.
data ModifyDBSnapshot = ModifyDBSnapshot'
  { dBSnapshotIdentifier :: Core.Text
    -- ^ The identifier of the DB snapshot to modify.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The engine version to upgrade the DB snapshot to. 
--
-- The following are the database engines and engine versions that are available when you upgrade a DB snapshot. 
-- __MySQL__ 
--
--     * @5.5.46@ (supported for 5.1 DB snapshots)
--
--
-- __Oracle__ 
--
--     * @12.1.0.2.v8@ (supported for 12.1.0.1 DB snapshots)
--
--
--     * @11.2.0.4.v12@ (supported for 11.2.0.2 DB snapshots)
--
--
--     * @11.2.0.4.v11@ (supported for 11.2.0.3 DB snapshots)
--
--
-- __PostgreSQL__ 
-- For the list of engine versions that are available for upgrading a DB snapshot, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html#USER_UpgradeDBInstance.PostgreSQL.MajorVersion Upgrading the PostgreSQL DB Engine for Amazon RDS> . 
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ The option group to identify with the upgraded DB snapshot. 
--
-- You can specify this parameter when you upgrade an Oracle DB snapshot. The same option group considerations apply when upgrading a DB snapshot as when upgrading a DB instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option Group Considerations> in the /Amazon RDS User Guide./ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSnapshot' value with any optional fields omitted.
mkModifyDBSnapshot
    :: Core.Text -- ^ 'dBSnapshotIdentifier'
    -> ModifyDBSnapshot
mkModifyDBSnapshot dBSnapshotIdentifier
  = ModifyDBSnapshot'{dBSnapshotIdentifier,
                      engineVersion = Core.Nothing, optionGroupName = Core.Nothing}

-- | The identifier of the DB snapshot to modify.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsDBSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshot Core.Text
mdbsDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE mdbsDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

-- | The engine version to upgrade the DB snapshot to. 
--
-- The following are the database engines and engine versions that are available when you upgrade a DB snapshot. 
-- __MySQL__ 
--
--     * @5.5.46@ (supported for 5.1 DB snapshots)
--
--
-- __Oracle__ 
--
--     * @12.1.0.2.v8@ (supported for 12.1.0.1 DB snapshots)
--
--
--     * @11.2.0.4.v12@ (supported for 11.2.0.2 DB snapshots)
--
--
--     * @11.2.0.4.v11@ (supported for 11.2.0.3 DB snapshots)
--
--
-- __PostgreSQL__ 
-- For the list of engine versions that are available for upgrading a DB snapshot, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html#USER_UpgradeDBInstance.PostgreSQL.MajorVersion Upgrading the PostgreSQL DB Engine for Amazon RDS> . 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsEngineVersion :: Lens.Lens' ModifyDBSnapshot (Core.Maybe Core.Text)
mdbsEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE mdbsEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The option group to identify with the upgraded DB snapshot. 
--
-- You can specify this parameter when you upgrade an Oracle DB snapshot. The same option group considerations apply when upgrading a DB snapshot as when upgrading a DB instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option Group Considerations> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsOptionGroupName :: Lens.Lens' ModifyDBSnapshot (Core.Maybe Core.Text)
mdbsOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE mdbsOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

instance Core.ToQuery ModifyDBSnapshot where
        toQuery ModifyDBSnapshot{..}
          = Core.toQueryPair "Action" ("ModifyDBSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBSnapshotIdentifier" dBSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionGroupName")
                optionGroupName

instance Core.ToHeaders ModifyDBSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyDBSnapshot where
        type Rs ModifyDBSnapshot = ModifyDBSnapshotResponse
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
          = Response.receiveXMLWrapper "ModifyDBSnapshotResult"
              (\ s h x ->
                 ModifyDBSnapshotResponse' Core.<$>
                   (x Core..@? "DBSnapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyDBSnapshotResponse' smart constructor.
data ModifyDBSnapshotResponse = ModifyDBSnapshotResponse'
  { dBSnapshot :: Core.Maybe Types.DBSnapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyDBSnapshotResponse' value with any optional fields omitted.
mkModifyDBSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyDBSnapshotResponse
mkModifyDBSnapshotResponse responseStatus
  = ModifyDBSnapshotResponse'{dBSnapshot = Core.Nothing,
                              responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsrrsDBSnapshot :: Lens.Lens' ModifyDBSnapshotResponse (Core.Maybe Types.DBSnapshot)
mdbsrrsDBSnapshot = Lens.field @"dBSnapshot"
{-# INLINEABLE mdbsrrsDBSnapshot #-}
{-# DEPRECATED dBSnapshot "Use generic-lens or generic-optics with 'dBSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsrrsResponseStatus :: Lens.Lens' ModifyDBSnapshotResponse Core.Int
mdbsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mdbsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

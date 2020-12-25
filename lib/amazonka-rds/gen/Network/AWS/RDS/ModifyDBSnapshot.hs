{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyDBSnapshot (..),
    mkModifyDBSnapshot,

    -- ** Request lenses
    mdbsDBSnapshotIdentifier,
    mdbsEngineVersion,
    mdbsOptionGroupName,

    -- * Destructuring the response
    ModifyDBSnapshotResponse (..),
    mkModifyDBSnapshotResponse,

    -- ** Response lenses
    mdbsrrsDBSnapshot,
    mdbsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyDBSnapshot' smart constructor.
data ModifyDBSnapshot = ModifyDBSnapshot'
  { -- | The identifier of the DB snapshot to modify.
    dBSnapshotIdentifier :: Types.String,
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
    engineVersion :: Core.Maybe Types.String,
    -- | The option group to identify with the upgraded DB snapshot.
    --
    -- You can specify this parameter when you upgrade an Oracle DB snapshot. The same option group considerations apply when upgrading a DB snapshot as when upgrading a DB instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option Group Considerations> in the /Amazon RDS User Guide./
    optionGroupName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSnapshot' value with any optional fields omitted.
mkModifyDBSnapshot ::
  -- | 'dBSnapshotIdentifier'
  Types.String ->
  ModifyDBSnapshot
mkModifyDBSnapshot dBSnapshotIdentifier =
  ModifyDBSnapshot'
    { dBSnapshotIdentifier,
      engineVersion = Core.Nothing,
      optionGroupName = Core.Nothing
    }

-- | The identifier of the DB snapshot to modify.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsDBSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshot Types.String
mdbsDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# DEPRECATED mdbsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead." #-}

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
mdbsEngineVersion :: Lens.Lens' ModifyDBSnapshot (Core.Maybe Types.String)
mdbsEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED mdbsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The option group to identify with the upgraded DB snapshot.
--
-- You can specify this parameter when you upgrade an Oracle DB snapshot. The same option group considerations apply when upgrading a DB snapshot as when upgrading a DB instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option Group Considerations> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsOptionGroupName :: Lens.Lens' ModifyDBSnapshot (Core.Maybe Types.String)
mdbsOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED mdbsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

instance Core.AWSRequest ModifyDBSnapshot where
  type Rs ModifyDBSnapshot = ModifyDBSnapshotResponse
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
            ( Core.pure ("Action", "ModifyDBSnapshot")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSnapshotIdentifier" dBSnapshotIdentifier)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBSnapshotResult"
      ( \s h x ->
          ModifyDBSnapshotResponse'
            Core.<$> (x Core..@? "DBSnapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDBSnapshotResponse' smart constructor.
data ModifyDBSnapshotResponse = ModifyDBSnapshotResponse'
  { dBSnapshot :: Core.Maybe Types.DBSnapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyDBSnapshotResponse' value with any optional fields omitted.
mkModifyDBSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDBSnapshotResponse
mkModifyDBSnapshotResponse responseStatus =
  ModifyDBSnapshotResponse'
    { dBSnapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsrrsDBSnapshot :: Lens.Lens' ModifyDBSnapshotResponse (Core.Maybe Types.DBSnapshot)
mdbsrrsDBSnapshot = Lens.field @"dBSnapshot"
{-# DEPRECATED mdbsrrsDBSnapshot "Use generic-lens or generic-optics with 'dBSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsrrsResponseStatus :: Lens.Lens' ModifyDBSnapshotResponse Core.Int
mdbsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdbsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

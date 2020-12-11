{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    mdsEngineVersion,
    mdsOptionGroupName,
    mdsDBSnapshotIdentifier,

    -- * Destructuring the response
    ModifyDBSnapshotResponse (..),
    mkModifyDBSnapshotResponse,

    -- ** Response lenses
    mdsrsDBSnapshot,
    mdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyDBSnapshot' smart constructor.
data ModifyDBSnapshot = ModifyDBSnapshot'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    optionGroupName :: Lude.Maybe Lude.Text,
    dbSnapshotIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBSnapshot' with the minimum fields required to make a request.
--
-- * 'dbSnapshotIdentifier' - The identifier of the DB snapshot to modify.
-- * 'engineVersion' - The engine version to upgrade the DB snapshot to.
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
-- * 'optionGroupName' - The option group to identify with the upgraded DB snapshot.
--
-- You can specify this parameter when you upgrade an Oracle DB snapshot. The same option group considerations apply when upgrading a DB snapshot as when upgrading a DB instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option Group Considerations> in the /Amazon RDS User Guide./
mkModifyDBSnapshot ::
  -- | 'dbSnapshotIdentifier'
  Lude.Text ->
  ModifyDBSnapshot
mkModifyDBSnapshot pDBSnapshotIdentifier_ =
  ModifyDBSnapshot'
    { engineVersion = Lude.Nothing,
      optionGroupName = Lude.Nothing,
      dbSnapshotIdentifier = pDBSnapshotIdentifier_
    }

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
mdsEngineVersion :: Lens.Lens' ModifyDBSnapshot (Lude.Maybe Lude.Text)
mdsEngineVersion = Lens.lens (engineVersion :: ModifyDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ModifyDBSnapshot)
{-# DEPRECATED mdsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The option group to identify with the upgraded DB snapshot.
--
-- You can specify this parameter when you upgrade an Oracle DB snapshot. The same option group considerations apply when upgrading a DB snapshot as when upgrading a DB instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option Group Considerations> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsOptionGroupName :: Lens.Lens' ModifyDBSnapshot (Lude.Maybe Lude.Text)
mdsOptionGroupName = Lens.lens (optionGroupName :: ModifyDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: ModifyDBSnapshot)
{-# DEPRECATED mdsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The identifier of the DB snapshot to modify.
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsDBSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshot Lude.Text
mdsDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: ModifyDBSnapshot -> Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: ModifyDBSnapshot)
{-# DEPRECATED mdsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest ModifyDBSnapshot where
  type Rs ModifyDBSnapshot = ModifyDBSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBSnapshotResult"
      ( \s h x ->
          ModifyDBSnapshotResponse'
            Lude.<$> (x Lude..@? "DBSnapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBSnapshot where
  toQuery ModifyDBSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "OptionGroupName" Lude.=: optionGroupName,
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'mkModifyDBSnapshotResponse' smart constructor.
data ModifyDBSnapshotResponse = ModifyDBSnapshotResponse'
  { dbSnapshot ::
      Lude.Maybe DBSnapshot,
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

-- | Creates a value of 'ModifyDBSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbSnapshot' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyDBSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBSnapshotResponse
mkModifyDBSnapshotResponse pResponseStatus_ =
  ModifyDBSnapshotResponse'
    { dbSnapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsrsDBSnapshot :: Lens.Lens' ModifyDBSnapshotResponse (Lude.Maybe DBSnapshot)
mdsrsDBSnapshot = Lens.lens (dbSnapshot :: ModifyDBSnapshotResponse -> Lude.Maybe DBSnapshot) (\s a -> s {dbSnapshot = a} :: ModifyDBSnapshotResponse)
{-# DEPRECATED mdsrsDBSnapshot "Use generic-lens or generic-optics with 'dbSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsrsResponseStatus :: Lens.Lens' ModifyDBSnapshotResponse Lude.Int
mdsrsResponseStatus = Lens.lens (responseStatus :: ModifyDBSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBSnapshotResponse)
{-# DEPRECATED mdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

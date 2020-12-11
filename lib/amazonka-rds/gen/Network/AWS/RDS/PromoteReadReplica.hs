{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PromoteReadReplica
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes a read replica DB instance to a standalone DB instance.
module Network.AWS.RDS.PromoteReadReplica
  ( -- * Creating a request
    PromoteReadReplica (..),
    mkPromoteReadReplica,

    -- ** Request lenses
    prrPreferredBackupWindow,
    prrBackupRetentionPeriod,
    prrDBInstanceIdentifier,

    -- * Destructuring the response
    PromoteReadReplicaResponse (..),
    mkPromoteReadReplicaResponse,

    -- ** Response lenses
    prrrsDBInstance,
    prrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkPromoteReadReplica' smart constructor.
data PromoteReadReplica = PromoteReadReplica'
  { preferredBackupWindow ::
      Lude.Maybe Lude.Text,
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    dbInstanceIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PromoteReadReplica' with the minimum fields required to make a request.
--
-- * 'backupRetentionPeriod' - The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 0 to 35.
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas.
--
--
-- * 'dbInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing read replica DB instance.
--
--
-- Example: @mydbinstance@
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./
-- Constraints:
--
--     * Must be in the format @hh24:mi-hh24:mi@ .
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
mkPromoteReadReplica ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  PromoteReadReplica
mkPromoteReadReplica pDBInstanceIdentifier_ =
  PromoteReadReplica'
    { preferredBackupWindow = Lude.Nothing,
      backupRetentionPeriod = Lude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./
-- Constraints:
--
--     * Must be in the format @hh24:mi-hh24:mi@ .
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
--
--
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrPreferredBackupWindow :: Lens.Lens' PromoteReadReplica (Lude.Maybe Lude.Text)
prrPreferredBackupWindow = Lens.lens (preferredBackupWindow :: PromoteReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: PromoteReadReplica)
{-# DEPRECATED prrPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 0 to 35.
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas.
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrBackupRetentionPeriod :: Lens.Lens' PromoteReadReplica (Lude.Maybe Lude.Int)
prrBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: PromoteReadReplica -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: PromoteReadReplica)
{-# DEPRECATED prrBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing read replica DB instance.
--
--
-- Example: @mydbinstance@
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrDBInstanceIdentifier :: Lens.Lens' PromoteReadReplica Lude.Text
prrDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: PromoteReadReplica -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: PromoteReadReplica)
{-# DEPRECATED prrDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

instance Lude.AWSRequest PromoteReadReplica where
  type Rs PromoteReadReplica = PromoteReadReplicaResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "PromoteReadReplicaResult"
      ( \s h x ->
          PromoteReadReplicaResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PromoteReadReplica where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PromoteReadReplica where
  toPath = Lude.const "/"

instance Lude.ToQuery PromoteReadReplica where
  toQuery PromoteReadReplica' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PromoteReadReplica" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "PreferredBackupWindow" Lude.=: preferredBackupWindow,
        "BackupRetentionPeriod" Lude.=: backupRetentionPeriod,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier
      ]

-- | /See:/ 'mkPromoteReadReplicaResponse' smart constructor.
data PromoteReadReplicaResponse = PromoteReadReplicaResponse'
  { dbInstance ::
      Lude.Maybe DBInstance,
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

-- | Creates a value of 'PromoteReadReplicaResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPromoteReadReplicaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PromoteReadReplicaResponse
mkPromoteReadReplicaResponse pResponseStatus_ =
  PromoteReadReplicaResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsDBInstance :: Lens.Lens' PromoteReadReplicaResponse (Lude.Maybe DBInstance)
prrrsDBInstance = Lens.lens (dbInstance :: PromoteReadReplicaResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: PromoteReadReplicaResponse)
{-# DEPRECATED prrrsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsResponseStatus :: Lens.Lens' PromoteReadReplicaResponse Lude.Int
prrrsResponseStatus = Lens.lens (responseStatus :: PromoteReadReplicaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PromoteReadReplicaResponse)
{-# DEPRECATED prrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

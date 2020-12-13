{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBInstance action deletes a previously provisioned DB instance. When you delete a DB instance, all automated backups for that instance are deleted and can't be recovered. Manual DB snapshots of the DB instance to be deleted by @DeleteDBInstance@ are not deleted.
--
-- If you request a final DB snapshot the status of the Amazon RDS DB instance is @deleting@ until the DB snapshot is created. The API action @DescribeDBInstance@ is used to monitor the status of this operation. The action can't be canceled or reverted once submitted.
-- When a DB instance is in a failure state and has a status of @failed@ , @incompatible-restore@ , or @incompatible-network@ , you can only delete it when you skip creation of the final snapshot with the @SkipFinalSnapshot@ parameter.
-- If the specified DB instance is part of an Amazon Aurora DB cluster, you can't delete the DB instance if both of the following conditions are true:
--
--     * The DB cluster is a read replica of another Amazon Aurora DB cluster.
--
--
--     * The DB instance is the only instance in the DB cluster.
--
--
-- To delete a DB instance in this case, first call the @PromoteReadReplicaDBCluster@ API action to promote the DB cluster so it's no longer a read replica. After the promotion completes, then call the @DeleteDBInstance@ API action to delete the final instance in the DB cluster.
module Network.AWS.RDS.DeleteDBInstance
  ( -- * Creating a request
    DeleteDBInstance (..),
    mkDeleteDBInstance,

    -- ** Request lenses
    ddbiFinalDBSnapshotIdentifier,
    ddbiDBInstanceIdentifier,
    ddbiDeleteAutomatedBackups,
    ddbiSkipFinalSnapshot,

    -- * Destructuring the response
    DeleteDBInstanceResponse (..),
    mkDeleteDBInstanceResponse,

    -- ** Response lenses
    ddirsDBInstance,
    ddirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBInstance' smart constructor.
data DeleteDBInstance = DeleteDBInstance'
  { -- | The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the @SkipFinalSnapshot@ parameter is disabled.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    --
    --
    --     * Can't be specified when deleting a read replica.
    finalDBSnapshotIdentifier :: Lude.Maybe Lude.Text,
    -- | The DB instance identifier for the DB instance to be deleted. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the name of an existing DB instance.
    dbInstanceIdentifier :: Lude.Text,
    -- | A value that indicates whether to remove automated backups immediately after the DB instance is deleted. This parameter isn't case-sensitive. The default is to remove automated backups immediately after the DB instance is deleted.
    deleteAutomatedBackups :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether to skip the creation of a final DB snapshot before the DB instance is deleted. If skip is specified, no DB snapshot is created. If skip isn't specified, a DB snapshot is created before the DB instance is deleted. By default, skip isn't specified, and the DB snapshot is created.
    --
    -- When a DB instance is in a failure state and has a status of 'failed', 'incompatible-restore', or 'incompatible-network', it can only be deleted when skip is specified.
    -- Specify skip when deleting a read replica.
    skipFinalSnapshot :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBInstance' with the minimum fields required to make a request.
--
-- * 'finalDBSnapshotIdentifier' - The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the @SkipFinalSnapshot@ parameter is disabled.
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
--     * Can't be specified when deleting a read replica.
--
--
-- * 'dbInstanceIdentifier' - The DB instance identifier for the DB instance to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the name of an existing DB instance.
--
--
-- * 'deleteAutomatedBackups' - A value that indicates whether to remove automated backups immediately after the DB instance is deleted. This parameter isn't case-sensitive. The default is to remove automated backups immediately after the DB instance is deleted.
-- * 'skipFinalSnapshot' - A value that indicates whether to skip the creation of a final DB snapshot before the DB instance is deleted. If skip is specified, no DB snapshot is created. If skip isn't specified, a DB snapshot is created before the DB instance is deleted. By default, skip isn't specified, and the DB snapshot is created.
--
-- When a DB instance is in a failure state and has a status of 'failed', 'incompatible-restore', or 'incompatible-network', it can only be deleted when skip is specified.
-- Specify skip when deleting a read replica.
mkDeleteDBInstance ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  DeleteDBInstance
mkDeleteDBInstance pDBInstanceIdentifier_ =
  DeleteDBInstance'
    { finalDBSnapshotIdentifier = Lude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_,
      deleteAutomatedBackups = Lude.Nothing,
      skipFinalSnapshot = Lude.Nothing
    }

-- | The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the @SkipFinalSnapshot@ parameter is disabled.
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
--     * Can't be specified when deleting a read replica.
--
--
--
-- /Note:/ Consider using 'finalDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiFinalDBSnapshotIdentifier :: Lens.Lens' DeleteDBInstance (Lude.Maybe Lude.Text)
ddbiFinalDBSnapshotIdentifier = Lens.lens (finalDBSnapshotIdentifier :: DeleteDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {finalDBSnapshotIdentifier = a} :: DeleteDBInstance)
{-# DEPRECATED ddbiFinalDBSnapshotIdentifier "Use generic-lens or generic-optics with 'finalDBSnapshotIdentifier' instead." #-}

-- | The DB instance identifier for the DB instance to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the name of an existing DB instance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiDBInstanceIdentifier :: Lens.Lens' DeleteDBInstance Lude.Text
ddbiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DeleteDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DeleteDBInstance)
{-# DEPRECATED ddbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | A value that indicates whether to remove automated backups immediately after the DB instance is deleted. This parameter isn't case-sensitive. The default is to remove automated backups immediately after the DB instance is deleted.
--
-- /Note:/ Consider using 'deleteAutomatedBackups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiDeleteAutomatedBackups :: Lens.Lens' DeleteDBInstance (Lude.Maybe Lude.Bool)
ddbiDeleteAutomatedBackups = Lens.lens (deleteAutomatedBackups :: DeleteDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {deleteAutomatedBackups = a} :: DeleteDBInstance)
{-# DEPRECATED ddbiDeleteAutomatedBackups "Use generic-lens or generic-optics with 'deleteAutomatedBackups' instead." #-}

-- | A value that indicates whether to skip the creation of a final DB snapshot before the DB instance is deleted. If skip is specified, no DB snapshot is created. If skip isn't specified, a DB snapshot is created before the DB instance is deleted. By default, skip isn't specified, and the DB snapshot is created.
--
-- When a DB instance is in a failure state and has a status of 'failed', 'incompatible-restore', or 'incompatible-network', it can only be deleted when skip is specified.
-- Specify skip when deleting a read replica.
--
-- /Note:/ Consider using 'skipFinalSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiSkipFinalSnapshot :: Lens.Lens' DeleteDBInstance (Lude.Maybe Lude.Bool)
ddbiSkipFinalSnapshot = Lens.lens (skipFinalSnapshot :: DeleteDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {skipFinalSnapshot = a} :: DeleteDBInstance)
{-# DEPRECATED ddbiSkipFinalSnapshot "Use generic-lens or generic-optics with 'skipFinalSnapshot' instead." #-}

instance Lude.AWSRequest DeleteDBInstance where
  type Rs DeleteDBInstance = DeleteDBInstanceResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteDBInstanceResult"
      ( \s h x ->
          DeleteDBInstanceResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBInstance where
  toQuery DeleteDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "FinalDBSnapshotIdentifier" Lude.=: finalDBSnapshotIdentifier,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "DeleteAutomatedBackups" Lude.=: deleteAutomatedBackups,
        "SkipFinalSnapshot" Lude.=: skipFinalSnapshot
      ]

-- | /See:/ 'mkDeleteDBInstanceResponse' smart constructor.
data DeleteDBInstanceResponse = DeleteDBInstanceResponse'
  { dbInstance :: Lude.Maybe DBInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBInstanceResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' -
-- * 'responseStatus' - The response status code.
mkDeleteDBInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDBInstanceResponse
mkDeleteDBInstanceResponse pResponseStatus_ =
  DeleteDBInstanceResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddirsDBInstance :: Lens.Lens' DeleteDBInstanceResponse (Lude.Maybe DBInstance)
ddirsDBInstance = Lens.lens (dbInstance :: DeleteDBInstanceResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: DeleteDBInstanceResponse)
{-# DEPRECATED ddirsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddirsResponseStatus :: Lens.Lens' DeleteDBInstanceResponse Lude.Int
ddirsResponseStatus = Lens.lens (responseStatus :: DeleteDBInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDBInstanceResponse)
{-# DEPRECATED ddirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

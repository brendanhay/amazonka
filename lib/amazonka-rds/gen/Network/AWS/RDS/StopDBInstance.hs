{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon RDS DB instance. When you stop a DB instance, Amazon RDS retains the DB instance's metadata, including its endpoint, DB parameter group, and option group membership. Amazon RDS also retains the transaction logs so you can do a point-in-time restore if necessary.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_StopInstance.html Stopping an Amazon RDS DB Instance Temporarily> in the /Amazon RDS User Guide./
module Network.AWS.RDS.StopDBInstance
  ( -- * Creating a request
    StopDBInstance (..),
    mkStopDBInstance,

    -- ** Request lenses
    sdiDBSnapshotIdentifier,
    sdiDBInstanceIdentifier,

    -- * Destructuring the response
    StopDBInstanceResponse (..),
    mkStopDBInstanceResponse,

    -- ** Response lenses
    sdirsDBInstance,
    sdirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopDBInstance' smart constructor.
data StopDBInstance = StopDBInstance'
  { dbSnapshotIdentifier ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StopDBInstance' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The user-supplied instance identifier.
-- * 'dbSnapshotIdentifier' - The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped.
mkStopDBInstance ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  StopDBInstance
mkStopDBInstance pDBInstanceIdentifier_ =
  StopDBInstance'
    { dbSnapshotIdentifier = Lude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped.
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiDBSnapshotIdentifier :: Lens.Lens' StopDBInstance (Lude.Maybe Lude.Text)
sdiDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: StopDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: StopDBInstance)
{-# DEPRECATED sdiDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

-- | The user-supplied instance identifier.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiDBInstanceIdentifier :: Lens.Lens' StopDBInstance Lude.Text
sdiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: StopDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: StopDBInstance)
{-# DEPRECATED sdiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

instance Lude.AWSRequest StopDBInstance where
  type Rs StopDBInstance = StopDBInstanceResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "StopDBInstanceResult"
      ( \s h x ->
          StopDBInstanceResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StopDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StopDBInstance where
  toQuery StopDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StopDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier
      ]

-- | /See:/ 'mkStopDBInstanceResponse' smart constructor.
data StopDBInstanceResponse = StopDBInstanceResponse'
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

-- | Creates a value of 'StopDBInstanceResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkStopDBInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopDBInstanceResponse
mkStopDBInstanceResponse pResponseStatus_ =
  StopDBInstanceResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdirsDBInstance :: Lens.Lens' StopDBInstanceResponse (Lude.Maybe DBInstance)
sdirsDBInstance = Lens.lens (dbInstance :: StopDBInstanceResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: StopDBInstanceResponse)
{-# DEPRECATED sdirsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdirsResponseStatus :: Lens.Lens' StopDBInstanceResponse Lude.Int
sdirsResponseStatus = Lens.lens (responseStatus :: StopDBInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopDBInstanceResponse)
{-# DEPRECATED sdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

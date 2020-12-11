{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RebootDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You might need to reboot your DB instance, usually for maintenance reasons. For example, if you make certain modifications, or if you change the DB parameter group associated with the DB instance, you must reboot the instance for the changes to take effect.
--
-- Rebooting a DB instance restarts the database engine service. Rebooting a DB instance results in a momentary outage, during which the DB instance status is set to rebooting.
-- For more information about rebooting, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_RebootInstance.html Rebooting a DB Instance> in the /Amazon RDS User Guide./
module Network.AWS.RDS.RebootDBInstance
  ( -- * Creating a request
    RebootDBInstance (..),
    mkRebootDBInstance,

    -- ** Request lenses
    rdiForceFailover,
    rdiDBInstanceIdentifier,

    -- * Destructuring the response
    RebootDBInstanceResponse (..),
    mkRebootDBInstanceResponse,

    -- ** Response lenses
    rdirsDBInstance,
    rdirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRebootDBInstance' smart constructor.
data RebootDBInstance = RebootDBInstance'
  { forceFailover ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'RebootDBInstance' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
-- * 'forceFailover' - A value that indicates whether the reboot is conducted through a Multi-AZ failover.
--
-- Constraint: You can't enable force failover if the instance isn't configured for Multi-AZ.
mkRebootDBInstance ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  RebootDBInstance
mkRebootDBInstance pDBInstanceIdentifier_ =
  RebootDBInstance'
    { forceFailover = Lude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | A value that indicates whether the reboot is conducted through a Multi-AZ failover.
--
-- Constraint: You can't enable force failover if the instance isn't configured for Multi-AZ.
--
-- /Note:/ Consider using 'forceFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiForceFailover :: Lens.Lens' RebootDBInstance (Lude.Maybe Lude.Bool)
rdiForceFailover = Lens.lens (forceFailover :: RebootDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {forceFailover = a} :: RebootDBInstance)
{-# DEPRECATED rdiForceFailover "Use generic-lens or generic-optics with 'forceFailover' instead." #-}

-- | The DB instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDBInstanceIdentifier :: Lens.Lens' RebootDBInstance Lude.Text
rdiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: RebootDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: RebootDBInstance)
{-# DEPRECATED rdiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

instance Lude.AWSRequest RebootDBInstance where
  type Rs RebootDBInstance = RebootDBInstanceResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RebootDBInstanceResult"
      ( \s h x ->
          RebootDBInstanceResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RebootDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootDBInstance where
  toQuery RebootDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RebootDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ForceFailover" Lude.=: forceFailover,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier
      ]

-- | /See:/ 'mkRebootDBInstanceResponse' smart constructor.
data RebootDBInstanceResponse = RebootDBInstanceResponse'
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

-- | Creates a value of 'RebootDBInstanceResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRebootDBInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootDBInstanceResponse
mkRebootDBInstanceResponse pResponseStatus_ =
  RebootDBInstanceResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdirsDBInstance :: Lens.Lens' RebootDBInstanceResponse (Lude.Maybe DBInstance)
rdirsDBInstance = Lens.lens (dbInstance :: RebootDBInstanceResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: RebootDBInstanceResponse)
{-# DEPRECATED rdirsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdirsResponseStatus :: Lens.Lens' RebootDBInstanceResponse Lude.Int
rdirsResponseStatus = Lens.lens (responseStatus :: RebootDBInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootDBInstanceResponse)
{-# DEPRECATED rdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

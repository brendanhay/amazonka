{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon RDS DB instance that was stopped using the AWS console, the stop-db-instance AWS CLI command, or the StopDBInstance action.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_StartInstance.html Starting an Amazon RDS DB instance That Was Previously Stopped> in the /Amazon RDS User Guide./
module Network.AWS.RDS.StartDBInstance
  ( -- * Creating a request
    StartDBInstance (..),
    mkStartDBInstance,

    -- ** Request lenses
    sdbiDBInstanceIdentifier,

    -- * Destructuring the response
    StartDBInstanceResponse (..),
    mkStartDBInstanceResponse,

    -- ** Response lenses
    sdbirsDBInstance,
    sdbirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartDBInstance' smart constructor.
newtype StartDBInstance = StartDBInstance'
  { dbInstanceIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDBInstance' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The user-supplied instance identifier.
mkStartDBInstance ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  StartDBInstance
mkStartDBInstance pDBInstanceIdentifier_ =
  StartDBInstance' {dbInstanceIdentifier = pDBInstanceIdentifier_}

-- | The user-supplied instance identifier.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbiDBInstanceIdentifier :: Lens.Lens' StartDBInstance Lude.Text
sdbiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: StartDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: StartDBInstance)
{-# DEPRECATED sdbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

instance Lude.AWSRequest StartDBInstance where
  type Rs StartDBInstance = StartDBInstanceResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "StartDBInstanceResult"
      ( \s h x ->
          StartDBInstanceResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StartDBInstance where
  toQuery StartDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StartDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier
      ]

-- | /See:/ 'mkStartDBInstanceResponse' smart constructor.
data StartDBInstanceResponse = StartDBInstanceResponse'
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

-- | Creates a value of 'StartDBInstanceResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkStartDBInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartDBInstanceResponse
mkStartDBInstanceResponse pResponseStatus_ =
  StartDBInstanceResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirsDBInstance :: Lens.Lens' StartDBInstanceResponse (Lude.Maybe DBInstance)
sdbirsDBInstance = Lens.lens (dbInstance :: StartDBInstanceResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: StartDBInstanceResponse)
{-# DEPRECATED sdbirsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirsResponseStatus :: Lens.Lens' StartDBInstanceResponse Lude.Int
sdbirsResponseStatus = Lens.lens (responseStatus :: StartDBInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartDBInstanceResponse)
{-# DEPRECATED sdbirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

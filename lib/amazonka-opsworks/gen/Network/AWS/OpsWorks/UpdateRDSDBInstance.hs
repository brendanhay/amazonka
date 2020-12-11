{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateRDSDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon RDS instance.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateRDSDBInstance
  ( -- * Creating a request
    UpdateRDSDBInstance (..),
    mkUpdateRDSDBInstance,

    -- ** Request lenses
    urdiDBUser,
    urdiDBPassword,
    urdiRDSDBInstanceARN,

    -- * Destructuring the response
    UpdateRDSDBInstanceResponse (..),
    mkUpdateRDSDBInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRDSDBInstance' smart constructor.
data UpdateRDSDBInstance = UpdateRDSDBInstance'
  { dbUser ::
      Lude.Maybe Lude.Text,
    dbPassword :: Lude.Maybe Lude.Text,
    rdsDBInstanceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRDSDBInstance' with the minimum fields required to make a request.
--
-- * 'dbPassword' - The database password.
-- * 'dbUser' - The master user name.
-- * 'rdsDBInstanceARN' - The Amazon RDS instance's ARN.
mkUpdateRDSDBInstance ::
  -- | 'rdsDBInstanceARN'
  Lude.Text ->
  UpdateRDSDBInstance
mkUpdateRDSDBInstance pRDSDBInstanceARN_ =
  UpdateRDSDBInstance'
    { dbUser = Lude.Nothing,
      dbPassword = Lude.Nothing,
      rdsDBInstanceARN = pRDSDBInstanceARN_
    }

-- | The master user name.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdiDBUser :: Lens.Lens' UpdateRDSDBInstance (Lude.Maybe Lude.Text)
urdiDBUser = Lens.lens (dbUser :: UpdateRDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbUser = a} :: UpdateRDSDBInstance)
{-# DEPRECATED urdiDBUser "Use generic-lens or generic-optics with 'dbUser' instead." #-}

-- | The database password.
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdiDBPassword :: Lens.Lens' UpdateRDSDBInstance (Lude.Maybe Lude.Text)
urdiDBPassword = Lens.lens (dbPassword :: UpdateRDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbPassword = a} :: UpdateRDSDBInstance)
{-# DEPRECATED urdiDBPassword "Use generic-lens or generic-optics with 'dbPassword' instead." #-}

-- | The Amazon RDS instance's ARN.
--
-- /Note:/ Consider using 'rdsDBInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdiRDSDBInstanceARN :: Lens.Lens' UpdateRDSDBInstance Lude.Text
urdiRDSDBInstanceARN = Lens.lens (rdsDBInstanceARN :: UpdateRDSDBInstance -> Lude.Text) (\s a -> s {rdsDBInstanceARN = a} :: UpdateRDSDBInstance)
{-# DEPRECATED urdiRDSDBInstanceARN "Use generic-lens or generic-optics with 'rdsDBInstanceARN' instead." #-}

instance Lude.AWSRequest UpdateRDSDBInstance where
  type Rs UpdateRDSDBInstance = UpdateRDSDBInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateRDSDBInstanceResponse'

instance Lude.ToHeaders UpdateRDSDBInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateRdsDbInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRDSDBInstance where
  toJSON UpdateRDSDBInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DbUser" Lude..=) Lude.<$> dbUser,
            ("DbPassword" Lude..=) Lude.<$> dbPassword,
            Lude.Just ("RdsDbInstanceArn" Lude..= rdsDBInstanceARN)
          ]
      )

instance Lude.ToPath UpdateRDSDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRDSDBInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRDSDBInstanceResponse' smart constructor.
data UpdateRDSDBInstanceResponse = UpdateRDSDBInstanceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRDSDBInstanceResponse' with the minimum fields required to make a request.
mkUpdateRDSDBInstanceResponse ::
  UpdateRDSDBInstanceResponse
mkUpdateRDSDBInstanceResponse = UpdateRDSDBInstanceResponse'

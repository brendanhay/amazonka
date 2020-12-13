{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterRDSDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon RDS instance with a stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterRDSDBInstance
  ( -- * Creating a request
    RegisterRDSDBInstance (..),
    mkRegisterRDSDBInstance,

    -- ** Request lenses
    rrdiRDSDBInstanceARN,
    rrdiDBUser,
    rrdiStackId,
    rrdiDBPassword,

    -- * Destructuring the response
    RegisterRDSDBInstanceResponse (..),
    mkRegisterRDSDBInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterRDSDBInstance' smart constructor.
data RegisterRDSDBInstance = RegisterRDSDBInstance'
  { -- | The Amazon RDS instance's ARN.
    rdsDBInstanceARN :: Lude.Text,
    -- | The database's master user name.
    dbUser :: Lude.Text,
    -- | The stack ID.
    stackId :: Lude.Text,
    -- | The database password.
    dbPassword :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterRDSDBInstance' with the minimum fields required to make a request.
--
-- * 'rdsDBInstanceARN' - The Amazon RDS instance's ARN.
-- * 'dbUser' - The database's master user name.
-- * 'stackId' - The stack ID.
-- * 'dbPassword' - The database password.
mkRegisterRDSDBInstance ::
  -- | 'rdsDBInstanceARN'
  Lude.Text ->
  -- | 'dbUser'
  Lude.Text ->
  -- | 'stackId'
  Lude.Text ->
  -- | 'dbPassword'
  Lude.Text ->
  RegisterRDSDBInstance
mkRegisterRDSDBInstance
  pRDSDBInstanceARN_
  pDBUser_
  pStackId_
  pDBPassword_ =
    RegisterRDSDBInstance'
      { rdsDBInstanceARN = pRDSDBInstanceARN_,
        dbUser = pDBUser_,
        stackId = pStackId_,
        dbPassword = pDBPassword_
      }

-- | The Amazon RDS instance's ARN.
--
-- /Note:/ Consider using 'rdsDBInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiRDSDBInstanceARN :: Lens.Lens' RegisterRDSDBInstance Lude.Text
rrdiRDSDBInstanceARN = Lens.lens (rdsDBInstanceARN :: RegisterRDSDBInstance -> Lude.Text) (\s a -> s {rdsDBInstanceARN = a} :: RegisterRDSDBInstance)
{-# DEPRECATED rrdiRDSDBInstanceARN "Use generic-lens or generic-optics with 'rdsDBInstanceARN' instead." #-}

-- | The database's master user name.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiDBUser :: Lens.Lens' RegisterRDSDBInstance Lude.Text
rrdiDBUser = Lens.lens (dbUser :: RegisterRDSDBInstance -> Lude.Text) (\s a -> s {dbUser = a} :: RegisterRDSDBInstance)
{-# DEPRECATED rrdiDBUser "Use generic-lens or generic-optics with 'dbUser' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiStackId :: Lens.Lens' RegisterRDSDBInstance Lude.Text
rrdiStackId = Lens.lens (stackId :: RegisterRDSDBInstance -> Lude.Text) (\s a -> s {stackId = a} :: RegisterRDSDBInstance)
{-# DEPRECATED rrdiStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The database password.
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiDBPassword :: Lens.Lens' RegisterRDSDBInstance Lude.Text
rrdiDBPassword = Lens.lens (dbPassword :: RegisterRDSDBInstance -> Lude.Text) (\s a -> s {dbPassword = a} :: RegisterRDSDBInstance)
{-# DEPRECATED rrdiDBPassword "Use generic-lens or generic-optics with 'dbPassword' instead." #-}

instance Lude.AWSRequest RegisterRDSDBInstance where
  type Rs RegisterRDSDBInstance = RegisterRDSDBInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull RegisterRDSDBInstanceResponse'

instance Lude.ToHeaders RegisterRDSDBInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.RegisterRdsDbInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterRDSDBInstance where
  toJSON RegisterRDSDBInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RdsDbInstanceArn" Lude..= rdsDBInstanceARN),
            Lude.Just ("DbUser" Lude..= dbUser),
            Lude.Just ("StackId" Lude..= stackId),
            Lude.Just ("DbPassword" Lude..= dbPassword)
          ]
      )

instance Lude.ToPath RegisterRDSDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterRDSDBInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterRDSDBInstanceResponse' smart constructor.
data RegisterRDSDBInstanceResponse = RegisterRDSDBInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterRDSDBInstanceResponse' with the minimum fields required to make a request.
mkRegisterRDSDBInstanceResponse ::
  RegisterRDSDBInstanceResponse
mkRegisterRDSDBInstanceResponse = RegisterRDSDBInstanceResponse'

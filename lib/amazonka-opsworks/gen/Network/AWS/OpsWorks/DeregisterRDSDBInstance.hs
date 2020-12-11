{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterRDSDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon RDS instance.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeregisterRDSDBInstance
  ( -- * Creating a request
    DeregisterRDSDBInstance (..),
    mkDeregisterRDSDBInstance,

    -- ** Request lenses
    drdiRDSDBInstanceARN,

    -- * Destructuring the response
    DeregisterRDSDBInstanceResponse (..),
    mkDeregisterRDSDBInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterRDSDBInstance' smart constructor.
newtype DeregisterRDSDBInstance = DeregisterRDSDBInstance'
  { rdsDBInstanceARN ::
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

-- | Creates a value of 'DeregisterRDSDBInstance' with the minimum fields required to make a request.
--
-- * 'rdsDBInstanceARN' - The Amazon RDS instance's ARN.
mkDeregisterRDSDBInstance ::
  -- | 'rdsDBInstanceARN'
  Lude.Text ->
  DeregisterRDSDBInstance
mkDeregisterRDSDBInstance pRDSDBInstanceARN_ =
  DeregisterRDSDBInstance' {rdsDBInstanceARN = pRDSDBInstanceARN_}

-- | The Amazon RDS instance's ARN.
--
-- /Note:/ Consider using 'rdsDBInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiRDSDBInstanceARN :: Lens.Lens' DeregisterRDSDBInstance Lude.Text
drdiRDSDBInstanceARN = Lens.lens (rdsDBInstanceARN :: DeregisterRDSDBInstance -> Lude.Text) (\s a -> s {rdsDBInstanceARN = a} :: DeregisterRDSDBInstance)
{-# DEPRECATED drdiRDSDBInstanceARN "Use generic-lens or generic-optics with 'rdsDBInstanceARN' instead." #-}

instance Lude.AWSRequest DeregisterRDSDBInstance where
  type Rs DeregisterRDSDBInstance = DeregisterRDSDBInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeregisterRDSDBInstanceResponse'

instance Lude.ToHeaders DeregisterRDSDBInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeregisterRdsDbInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterRDSDBInstance where
  toJSON DeregisterRDSDBInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RdsDbInstanceArn" Lude..= rdsDBInstanceARN)]
      )

instance Lude.ToPath DeregisterRDSDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterRDSDBInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterRDSDBInstanceResponse' smart constructor.
data DeregisterRDSDBInstanceResponse = DeregisterRDSDBInstanceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterRDSDBInstanceResponse' with the minimum fields required to make a request.
mkDeregisterRDSDBInstanceResponse ::
  DeregisterRDSDBInstanceResponse
mkDeregisterRDSDBInstanceResponse =
  DeregisterRDSDBInstanceResponse'

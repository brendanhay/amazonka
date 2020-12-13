{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregister a registered Amazon EC2 or on-premises instance. This action removes the instance from the stack and returns it to your control. This action cannot be used with instances that were created with AWS OpsWorks Stacks.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeregisterInstance
  ( -- * Creating a request
    DeregisterInstance (..),
    mkDeregisterInstance,

    -- ** Request lenses
    diInstanceId,

    -- * Destructuring the response
    DeregisterInstanceResponse (..),
    mkDeregisterInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterInstance' smart constructor.
newtype DeregisterInstance = DeregisterInstance'
  { -- | The instance ID.
    instanceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
mkDeregisterInstance ::
  -- | 'instanceId'
  Lude.Text ->
  DeregisterInstance
mkDeregisterInstance pInstanceId_ =
  DeregisterInstance' {instanceId = pInstanceId_}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DeregisterInstance Lude.Text
diInstanceId = Lens.lens (instanceId :: DeregisterInstance -> Lude.Text) (\s a -> s {instanceId = a} :: DeregisterInstance)
{-# DEPRECATED diInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DeregisterInstance where
  type Rs DeregisterInstance = DeregisterInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeregisterInstanceResponse'

instance Lude.ToHeaders DeregisterInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeregisterInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterInstance where
  toJSON DeregisterInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("InstanceId" Lude..= instanceId)])

instance Lude.ToPath DeregisterInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstanceResponse' with the minimum fields required to make a request.
mkDeregisterInstanceResponse ::
  DeregisterInstanceResponse
mkDeregisterInstanceResponse = DeregisterInstanceResponse'

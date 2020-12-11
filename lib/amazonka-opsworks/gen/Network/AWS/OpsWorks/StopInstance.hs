{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StopInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified instance. When you stop a standard instance, the data disappears and must be reinstalled when you restart the instance. You can stop an Amazon EBS-backed instance without losing data. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StopInstance
  ( -- * Creating a request
    StopInstance (..),
    mkStopInstance,

    -- ** Request lenses
    siForce,
    siInstanceId,

    -- * Destructuring the response
    StopInstanceResponse (..),
    mkStopInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopInstance' smart constructor.
data StopInstance = StopInstance'
  { force :: Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopInstance' with the minimum fields required to make a request.
--
-- * 'force' - Specifies whether to force an instance to stop. If the instance's root device type is @ebs@ , or EBS-backed, adding the @Force@ parameter to the @StopInstances@ API call disassociates the AWS OpsWorks Stacks instance from EC2, and forces deletion of /only/ the OpsWorks Stacks instance. You must also delete the formerly-associated instance in EC2 after troubleshooting and replacing the AWS OpsWorks Stacks instance with a new one.
-- * 'instanceId' - The instance ID.
mkStopInstance ::
  -- | 'instanceId'
  Lude.Text ->
  StopInstance
mkStopInstance pInstanceId_ =
  StopInstance' {force = Lude.Nothing, instanceId = pInstanceId_}

-- | Specifies whether to force an instance to stop. If the instance's root device type is @ebs@ , or EBS-backed, adding the @Force@ parameter to the @StopInstances@ API call disassociates the AWS OpsWorks Stacks instance from EC2, and forces deletion of /only/ the OpsWorks Stacks instance. You must also delete the formerly-associated instance in EC2 after troubleshooting and replacing the AWS OpsWorks Stacks instance with a new one.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siForce :: Lens.Lens' StopInstance (Lude.Maybe Lude.Bool)
siForce = Lens.lens (force :: StopInstance -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: StopInstance)
{-# DEPRECATED siForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceId :: Lens.Lens' StopInstance Lude.Text
siInstanceId = Lens.lens (instanceId :: StopInstance -> Lude.Text) (\s a -> s {instanceId = a} :: StopInstance)
{-# DEPRECATED siInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest StopInstance where
  type Rs StopInstance = StopInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull StopInstanceResponse'

instance Lude.ToHeaders StopInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.StopInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopInstance where
  toJSON StopInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Force" Lude..=) Lude.<$> force,
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath StopInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StopInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopInstanceResponse' with the minimum fields required to make a request.
mkStopInstanceResponse ::
  StopInstanceResponse
mkStopInstanceResponse = StopInstanceResponse'

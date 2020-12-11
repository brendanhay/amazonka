{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specified instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StartInstance
  ( -- * Creating a request
    StartInstance (..),
    mkStartInstance,

    -- ** Request lenses
    sInstanceId,

    -- * Destructuring the response
    StartInstanceResponse (..),
    mkStartInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartInstance' smart constructor.
newtype StartInstance = StartInstance' {instanceId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
mkStartInstance ::
  -- | 'instanceId'
  Lude.Text ->
  StartInstance
mkStartInstance pInstanceId_ =
  StartInstance' {instanceId = pInstanceId_}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceId :: Lens.Lens' StartInstance Lude.Text
sInstanceId = Lens.lens (instanceId :: StartInstance -> Lude.Text) (\s a -> s {instanceId = a} :: StartInstance)
{-# DEPRECATED sInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest StartInstance where
  type Rs StartInstance = StartInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull StartInstanceResponse'

instance Lude.ToHeaders StartInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.StartInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartInstance where
  toJSON StartInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("InstanceId" Lude..= instanceId)])

instance Lude.ToPath StartInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StartInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstanceResponse' with the minimum fields required to make a request.
mkStartInstanceResponse ::
  StartInstanceResponse
mkStartInstanceResponse = StartInstanceResponse'

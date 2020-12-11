{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RebootInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a specified instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RebootInstance
  ( -- * Creating a request
    RebootInstance (..),
    mkRebootInstance,

    -- ** Request lenses
    riInstanceId,

    -- * Destructuring the response
    RebootInstanceResponse (..),
    mkRebootInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebootInstance' smart constructor.
newtype RebootInstance = RebootInstance' {instanceId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
mkRebootInstance ::
  -- | 'instanceId'
  Lude.Text ->
  RebootInstance
mkRebootInstance pInstanceId_ =
  RebootInstance' {instanceId = pInstanceId_}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceId :: Lens.Lens' RebootInstance Lude.Text
riInstanceId = Lens.lens (instanceId :: RebootInstance -> Lude.Text) (\s a -> s {instanceId = a} :: RebootInstance)
{-# DEPRECATED riInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest RebootInstance where
  type Rs RebootInstance = RebootInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull RebootInstanceResponse'

instance Lude.ToHeaders RebootInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.RebootInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebootInstance where
  toJSON RebootInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("InstanceId" Lude..= instanceId)])

instance Lude.ToPath RebootInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootInstanceResponse' with the minimum fields required to make a request.
mkRebootInstanceResponse ::
  RebootInstanceResponse
mkRebootInstanceResponse = RebootInstanceResponse'

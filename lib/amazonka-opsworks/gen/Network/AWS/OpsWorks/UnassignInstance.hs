{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UnassignInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns a registered instance from all layers that are using the instance. The instance remains in the stack as an unassigned instance, and can be assigned to another layer as needed. You cannot use this action with instances that were created with AWS OpsWorks Stacks.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UnassignInstance
  ( -- * Creating a request
    UnassignInstance (..),
    mkUnassignInstance,

    -- ** Request lenses
    uiInstanceId,

    -- * Destructuring the response
    UnassignInstanceResponse (..),
    mkUnassignInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnassignInstance' smart constructor.
newtype UnassignInstance = UnassignInstance'
  { -- | The instance ID.
    instanceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
mkUnassignInstance ::
  -- | 'instanceId'
  Lude.Text ->
  UnassignInstance
mkUnassignInstance pInstanceId_ =
  UnassignInstance' {instanceId = pInstanceId_}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInstanceId :: Lens.Lens' UnassignInstance Lude.Text
uiInstanceId = Lens.lens (instanceId :: UnassignInstance -> Lude.Text) (\s a -> s {instanceId = a} :: UnassignInstance)
{-# DEPRECATED uiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest UnassignInstance where
  type Rs UnassignInstance = UnassignInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UnassignInstanceResponse'

instance Lude.ToHeaders UnassignInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UnassignInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnassignInstance where
  toJSON UnassignInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("InstanceId" Lude..= instanceId)])

instance Lude.ToPath UnassignInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery UnassignInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnassignInstanceResponse' smart constructor.
data UnassignInstanceResponse = UnassignInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignInstanceResponse' with the minimum fields required to make a request.
mkUnassignInstanceResponse ::
  UnassignInstanceResponse
mkUnassignInstanceResponse = UnassignInstanceResponse'

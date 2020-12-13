{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssignInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assign a registered instance to a layer.
--
--
--     * You can assign registered on-premises instances to any layer type.
--
--
--     * You can assign registered Amazon EC2 instances only to custom layers.
--
--
--     * You cannot use this action with instances that were created with AWS OpsWorks Stacks.
--
--
-- __Required Permissions__ : To use this action, an AWS Identity and Access Management (IAM) user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AssignInstance
  ( -- * Creating a request
    AssignInstance (..),
    mkAssignInstance,

    -- ** Request lenses
    aiInstanceId,
    aiLayerIds,

    -- * Destructuring the response
    AssignInstanceResponse (..),
    mkAssignInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssignInstance' smart constructor.
data AssignInstance = AssignInstance'
  { -- | The instance ID.
    instanceId :: Lude.Text,
    -- | The layer ID, which must correspond to a custom layer. You cannot assign a registered instance to a built-in layer.
    layerIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
-- * 'layerIds' - The layer ID, which must correspond to a custom layer. You cannot assign a registered instance to a built-in layer.
mkAssignInstance ::
  -- | 'instanceId'
  Lude.Text ->
  AssignInstance
mkAssignInstance pInstanceId_ =
  AssignInstance'
    { instanceId = pInstanceId_,
      layerIds = Lude.mempty
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceId :: Lens.Lens' AssignInstance Lude.Text
aiInstanceId = Lens.lens (instanceId :: AssignInstance -> Lude.Text) (\s a -> s {instanceId = a} :: AssignInstance)
{-# DEPRECATED aiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The layer ID, which must correspond to a custom layer. You cannot assign a registered instance to a built-in layer.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiLayerIds :: Lens.Lens' AssignInstance [Lude.Text]
aiLayerIds = Lens.lens (layerIds :: AssignInstance -> [Lude.Text]) (\s a -> s {layerIds = a} :: AssignInstance)
{-# DEPRECATED aiLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

instance Lude.AWSRequest AssignInstance where
  type Rs AssignInstance = AssignInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull AssignInstanceResponse'

instance Lude.ToHeaders AssignInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.AssignInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssignInstance where
  toJSON AssignInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("LayerIds" Lude..= layerIds)
          ]
      )

instance Lude.ToPath AssignInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery AssignInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssignInstanceResponse' smart constructor.
data AssignInstanceResponse = AssignInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignInstanceResponse' with the minimum fields required to make a request.
mkAssignInstanceResponse ::
  AssignInstanceResponse
mkAssignInstanceResponse = AssignInstanceResponse'

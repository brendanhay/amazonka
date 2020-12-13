{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet scaling policy. Once deleted, the policy is no longer in force and GameLift removes all record of it. To delete a scaling policy, specify both the scaling policy name and the fleet ID it is associated with.
--
-- To temporarily suspend scaling policies, call 'StopFleetActions' . This operation suspends all policies for the fleet.
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--
--     * 'StopFleetActions'
module Network.AWS.GameLift.DeleteScalingPolicy
  ( -- * Creating a request
    DeleteScalingPolicy (..),
    mkDeleteScalingPolicy,

    -- ** Request lenses
    dName,
    dFleetId,

    -- * Destructuring the response
    DeleteScalingPolicyResponse (..),
    mkDeleteScalingPolicyResponse,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteScalingPolicy' smart constructor.
data DeleteScalingPolicy = DeleteScalingPolicy'
  { -- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
    name :: Lude.Text,
    -- | A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScalingPolicy' with the minimum fields required to make a request.
--
-- * 'name' - A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
-- * 'fleetId' - A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
mkDeleteScalingPolicy ::
  -- | 'name'
  Lude.Text ->
  -- | 'fleetId'
  Lude.Text ->
  DeleteScalingPolicy
mkDeleteScalingPolicy pName_ pFleetId_ =
  DeleteScalingPolicy' {name = pName_, fleetId = pFleetId_}

-- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeleteScalingPolicy Lude.Text
dName = Lens.lens (name :: DeleteScalingPolicy -> Lude.Text) (\s a -> s {name = a} :: DeleteScalingPolicy)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetId :: Lens.Lens' DeleteScalingPolicy Lude.Text
dFleetId = Lens.lens (fleetId :: DeleteScalingPolicy -> Lude.Text) (\s a -> s {fleetId = a} :: DeleteScalingPolicy)
{-# DEPRECATED dFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest DeleteScalingPolicy where
  type Rs DeleteScalingPolicy = DeleteScalingPolicyResponse
  request = Req.postJSON gameLiftService
  response = Res.receiveNull DeleteScalingPolicyResponse'

instance Lude.ToHeaders DeleteScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteScalingPolicy where
  toJSON DeleteScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath DeleteScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteScalingPolicyResponse' smart constructor.
data DeleteScalingPolicyResponse = DeleteScalingPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScalingPolicyResponse' with the minimum fields required to make a request.
mkDeleteScalingPolicyResponse ::
  DeleteScalingPolicyResponse
mkDeleteScalingPolicyResponse = DeleteScalingPolicyResponse'

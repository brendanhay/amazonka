{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet scaling policy. Once deleted, the policy is no longer in
-- force and GameLift removes all record of it. To delete a scaling policy,
-- specify both the scaling policy name and the fleet ID it is associated
-- with.
--
-- To temporarily suspend scaling policies, call StopFleetActions. This
-- operation suspends all policies for the fleet.
--
-- -   DescribeFleetCapacity
--
-- -   UpdateFleetCapacity
--
-- -   DescribeEC2InstanceLimits
--
-- -   Manage scaling policies:
--
--     -   PutScalingPolicy (auto-scaling)
--
--     -   DescribeScalingPolicies (auto-scaling)
--
--     -   DeleteScalingPolicy (auto-scaling)
--
-- -   Manage fleet actions:
--
--     -   StartFleetActions
--
--     -   StopFleetActions
module Network.AWS.GameLift.DeleteScalingPolicy
  ( -- * Creating a Request
    DeleteScalingPolicy (..),
    newDeleteScalingPolicy,

    -- * Request Lenses
    deleteScalingPolicy_name,
    deleteScalingPolicy_fleetId,

    -- * Destructuring the Response
    DeleteScalingPolicyResponse (..),
    newDeleteScalingPolicyResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteScalingPolicy' smart constructor.
data DeleteScalingPolicy = DeleteScalingPolicy'
  { -- | A descriptive label that is associated with a scaling policy. Policy
    -- names do not need to be unique.
    name :: Core.Text,
    -- | A unique identifier for a fleet to be deleted. You can use either the
    -- fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteScalingPolicy_name' - A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique.
--
-- 'fleetId', 'deleteScalingPolicy_fleetId' - A unique identifier for a fleet to be deleted. You can use either the
-- fleet ID or ARN value.
newDeleteScalingPolicy ::
  -- | 'name'
  Core.Text ->
  -- | 'fleetId'
  Core.Text ->
  DeleteScalingPolicy
newDeleteScalingPolicy pName_ pFleetId_ =
  DeleteScalingPolicy'
    { name = pName_,
      fleetId = pFleetId_
    }

-- | A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique.
deleteScalingPolicy_name :: Lens.Lens' DeleteScalingPolicy Core.Text
deleteScalingPolicy_name = Lens.lens (\DeleteScalingPolicy' {name} -> name) (\s@DeleteScalingPolicy' {} a -> s {name = a} :: DeleteScalingPolicy)

-- | A unique identifier for a fleet to be deleted. You can use either the
-- fleet ID or ARN value.
deleteScalingPolicy_fleetId :: Lens.Lens' DeleteScalingPolicy Core.Text
deleteScalingPolicy_fleetId = Lens.lens (\DeleteScalingPolicy' {fleetId} -> fleetId) (\s@DeleteScalingPolicy' {} a -> s {fleetId = a} :: DeleteScalingPolicy)

instance Core.AWSRequest DeleteScalingPolicy where
  type
    AWSResponse DeleteScalingPolicy =
      DeleteScalingPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteScalingPolicyResponse'

instance Core.Hashable DeleteScalingPolicy

instance Core.NFData DeleteScalingPolicy

instance Core.ToHeaders DeleteScalingPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DeleteScalingPolicy" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteScalingPolicy where
  toJSON DeleteScalingPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath DeleteScalingPolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeleteScalingPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteScalingPolicyResponse' smart constructor.
data DeleteScalingPolicyResponse = DeleteScalingPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScalingPolicyResponse ::
  DeleteScalingPolicyResponse
newDeleteScalingPolicyResponse =
  DeleteScalingPolicyResponse'

instance Core.NFData DeleteScalingPolicyResponse

{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteScalingPolicy' smart constructor.
data DeleteScalingPolicy = DeleteScalingPolicy'
  { -- | A descriptive label that is associated with a scaling policy. Policy
    -- names do not need to be unique.
    name :: Prelude.Text,
    -- | A unique identifier for a fleet to be deleted. You can use either the
    -- fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'fleetId'
  Prelude.Text ->
  DeleteScalingPolicy
newDeleteScalingPolicy pName_ pFleetId_ =
  DeleteScalingPolicy'
    { name = pName_,
      fleetId = pFleetId_
    }

-- | A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique.
deleteScalingPolicy_name :: Lens.Lens' DeleteScalingPolicy Prelude.Text
deleteScalingPolicy_name = Lens.lens (\DeleteScalingPolicy' {name} -> name) (\s@DeleteScalingPolicy' {} a -> s {name = a} :: DeleteScalingPolicy)

-- | A unique identifier for a fleet to be deleted. You can use either the
-- fleet ID or ARN value.
deleteScalingPolicy_fleetId :: Lens.Lens' DeleteScalingPolicy Prelude.Text
deleteScalingPolicy_fleetId = Lens.lens (\DeleteScalingPolicy' {fleetId} -> fleetId) (\s@DeleteScalingPolicy' {} a -> s {fleetId = a} :: DeleteScalingPolicy)

instance Prelude.AWSRequest DeleteScalingPolicy where
  type
    Rs DeleteScalingPolicy =
      DeleteScalingPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteScalingPolicyResponse'

instance Prelude.Hashable DeleteScalingPolicy

instance Prelude.NFData DeleteScalingPolicy

instance Prelude.ToHeaders DeleteScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.DeleteScalingPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteScalingPolicy where
  toJSON DeleteScalingPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("FleetId" Prelude..= fleetId)
          ]
      )

instance Prelude.ToPath DeleteScalingPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScalingPolicyResponse' smart constructor.
data DeleteScalingPolicyResponse = DeleteScalingPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScalingPolicyResponse ::
  DeleteScalingPolicyResponse
newDeleteScalingPolicyResponse =
  DeleteScalingPolicyResponse'

instance Prelude.NFData DeleteScalingPolicyResponse

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
-- Module      : Amazonka.GameLift.DeleteScalingPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet scaling policy. Once deleted, the policy is no longer in
-- force and GameLift removes all record of it. To delete a scaling policy,
-- specify both the scaling policy name and the fleet ID it is associated
-- with.
--
-- To temporarily suspend scaling policies, use
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StopFleetActions.html StopFleetActions>.
-- This operation suspends all policies for the fleet.
module Amazonka.GameLift.DeleteScalingPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteScalingPolicy' smart constructor.
data DeleteScalingPolicy = DeleteScalingPolicy'
  { -- | A descriptive label that is associated with a fleet\'s scaling policy.
    -- Policy names do not need to be unique.
    name :: Prelude.Text,
    -- | A unique identifier for the fleet to be deleted. You can use either the
    -- fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteScalingPolicy_name' - A descriptive label that is associated with a fleet\'s scaling policy.
-- Policy names do not need to be unique.
--
-- 'fleetId', 'deleteScalingPolicy_fleetId' - A unique identifier for the fleet to be deleted. You can use either the
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

-- | A descriptive label that is associated with a fleet\'s scaling policy.
-- Policy names do not need to be unique.
deleteScalingPolicy_name :: Lens.Lens' DeleteScalingPolicy Prelude.Text
deleteScalingPolicy_name = Lens.lens (\DeleteScalingPolicy' {name} -> name) (\s@DeleteScalingPolicy' {} a -> s {name = a} :: DeleteScalingPolicy)

-- | A unique identifier for the fleet to be deleted. You can use either the
-- fleet ID or ARN value.
deleteScalingPolicy_fleetId :: Lens.Lens' DeleteScalingPolicy Prelude.Text
deleteScalingPolicy_fleetId = Lens.lens (\DeleteScalingPolicy' {fleetId} -> fleetId) (\s@DeleteScalingPolicy' {} a -> s {fleetId = a} :: DeleteScalingPolicy)

instance Core.AWSRequest DeleteScalingPolicy where
  type
    AWSResponse DeleteScalingPolicy =
      DeleteScalingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteScalingPolicyResponse'

instance Prelude.Hashable DeleteScalingPolicy where
  hashWithSalt _salt DeleteScalingPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DeleteScalingPolicy where
  rnf DeleteScalingPolicy' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders DeleteScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DeleteScalingPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteScalingPolicy where
  toJSON DeleteScalingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("FleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath DeleteScalingPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScalingPolicyResponse' smart constructor.
data DeleteScalingPolicyResponse = DeleteScalingPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScalingPolicyResponse ::
  DeleteScalingPolicyResponse
newDeleteScalingPolicyResponse =
  DeleteScalingPolicyResponse'

instance Prelude.NFData DeleteScalingPolicyResponse where
  rnf _ = ()

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
-- Module      : Amazonka.AutoScalingPlans.DeleteScalingPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling plan.
--
-- Deleting a scaling plan deletes the underlying ScalingInstruction for
-- all of the scalable resources that are covered by the plan.
--
-- If the plan has launched resources or has scaling activities in
-- progress, you must delete those resources separately.
module Amazonka.AutoScalingPlans.DeleteScalingPlan
  ( -- * Creating a Request
    DeleteScalingPlan (..),
    newDeleteScalingPlan,

    -- * Request Lenses
    deleteScalingPlan_scalingPlanName,
    deleteScalingPlan_scalingPlanVersion,

    -- * Destructuring the Response
    DeleteScalingPlanResponse (..),
    newDeleteScalingPlanResponse,

    -- * Response Lenses
    deleteScalingPlanResponse_httpStatus,
  )
where

import Amazonka.AutoScalingPlans.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteScalingPlan' smart constructor.
data DeleteScalingPlan = DeleteScalingPlan'
  { -- | The name of the scaling plan.
    scalingPlanName :: Prelude.Text,
    -- | The version number of the scaling plan. Currently, the only valid value
    -- is @1@.
    scalingPlanVersion :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScalingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingPlanName', 'deleteScalingPlan_scalingPlanName' - The name of the scaling plan.
--
-- 'scalingPlanVersion', 'deleteScalingPlan_scalingPlanVersion' - The version number of the scaling plan. Currently, the only valid value
-- is @1@.
newDeleteScalingPlan ::
  -- | 'scalingPlanName'
  Prelude.Text ->
  -- | 'scalingPlanVersion'
  Prelude.Integer ->
  DeleteScalingPlan
newDeleteScalingPlan
  pScalingPlanName_
  pScalingPlanVersion_ =
    DeleteScalingPlan'
      { scalingPlanName =
          pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_
      }

-- | The name of the scaling plan.
deleteScalingPlan_scalingPlanName :: Lens.Lens' DeleteScalingPlan Prelude.Text
deleteScalingPlan_scalingPlanName = Lens.lens (\DeleteScalingPlan' {scalingPlanName} -> scalingPlanName) (\s@DeleteScalingPlan' {} a -> s {scalingPlanName = a} :: DeleteScalingPlan)

-- | The version number of the scaling plan. Currently, the only valid value
-- is @1@.
deleteScalingPlan_scalingPlanVersion :: Lens.Lens' DeleteScalingPlan Prelude.Integer
deleteScalingPlan_scalingPlanVersion = Lens.lens (\DeleteScalingPlan' {scalingPlanVersion} -> scalingPlanVersion) (\s@DeleteScalingPlan' {} a -> s {scalingPlanVersion = a} :: DeleteScalingPlan)

instance Core.AWSRequest DeleteScalingPlan where
  type
    AWSResponse DeleteScalingPlan =
      DeleteScalingPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScalingPlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteScalingPlan where
  hashWithSalt _salt DeleteScalingPlan' {..} =
    _salt
      `Prelude.hashWithSalt` scalingPlanName
      `Prelude.hashWithSalt` scalingPlanVersion

instance Prelude.NFData DeleteScalingPlan where
  rnf DeleteScalingPlan' {..} =
    Prelude.rnf scalingPlanName `Prelude.seq`
      Prelude.rnf scalingPlanVersion

instance Data.ToHeaders DeleteScalingPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AnyScaleScalingPlannerFrontendService.DeleteScalingPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteScalingPlan where
  toJSON DeleteScalingPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScalingPlanName" Data..= scalingPlanName),
            Prelude.Just
              ("ScalingPlanVersion" Data..= scalingPlanVersion)
          ]
      )

instance Data.ToPath DeleteScalingPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteScalingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScalingPlanResponse' smart constructor.
data DeleteScalingPlanResponse = DeleteScalingPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScalingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteScalingPlanResponse_httpStatus' - The response's http status code.
newDeleteScalingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteScalingPlanResponse
newDeleteScalingPlanResponse pHttpStatus_ =
  DeleteScalingPlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteScalingPlanResponse_httpStatus :: Lens.Lens' DeleteScalingPlanResponse Prelude.Int
deleteScalingPlanResponse_httpStatus = Lens.lens (\DeleteScalingPlanResponse' {httpStatus} -> httpStatus) (\s@DeleteScalingPlanResponse' {} a -> s {httpStatus = a} :: DeleteScalingPlanResponse)

instance Prelude.NFData DeleteScalingPlanResponse where
  rnf DeleteScalingPlanResponse' {..} =
    Prelude.rnf httpStatus

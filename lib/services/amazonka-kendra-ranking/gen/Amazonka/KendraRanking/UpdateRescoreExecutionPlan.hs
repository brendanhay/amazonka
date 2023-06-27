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
-- Module      : Amazonka.KendraRanking.UpdateRescoreExecutionPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a rescore execution plan. A rescore execution plan is an Amazon
-- Kendra Intelligent Ranking resource used for provisioning the @Rescore@
-- API. You can update the number of capacity units you require for Amazon
-- Kendra Intelligent Ranking to rescore or re-rank a search service\'s
-- results.
module Amazonka.KendraRanking.UpdateRescoreExecutionPlan
  ( -- * Creating a Request
    UpdateRescoreExecutionPlan (..),
    newUpdateRescoreExecutionPlan,

    -- * Request Lenses
    updateRescoreExecutionPlan_capacityUnits,
    updateRescoreExecutionPlan_description,
    updateRescoreExecutionPlan_name,
    updateRescoreExecutionPlan_id,

    -- * Destructuring the Response
    UpdateRescoreExecutionPlanResponse (..),
    newUpdateRescoreExecutionPlanResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KendraRanking.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRescoreExecutionPlan' smart constructor.
data UpdateRescoreExecutionPlan = UpdateRescoreExecutionPlan'
  { -- | You can set additional capacity units to meet the needs of your rescore
    -- execution plan. You are given a single capacity unit by default. If you
    -- want to use the default capacity, you don\'t set additional capacity
    -- units. For more information on the default capacity and additional
    -- capacity units, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | A new description for the rescore execution plan.
    description :: Prelude.Maybe Prelude.Text,
    -- | A new name for the rescore execution plan.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the rescore execution plan that you want to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRescoreExecutionPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityUnits', 'updateRescoreExecutionPlan_capacityUnits' - You can set additional capacity units to meet the needs of your rescore
-- execution plan. You are given a single capacity unit by default. If you
-- want to use the default capacity, you don\'t set additional capacity
-- units. For more information on the default capacity and additional
-- capacity units, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
--
-- 'description', 'updateRescoreExecutionPlan_description' - A new description for the rescore execution plan.
--
-- 'name', 'updateRescoreExecutionPlan_name' - A new name for the rescore execution plan.
--
-- 'id', 'updateRescoreExecutionPlan_id' - The identifier of the rescore execution plan that you want to update.
newUpdateRescoreExecutionPlan ::
  -- | 'id'
  Prelude.Text ->
  UpdateRescoreExecutionPlan
newUpdateRescoreExecutionPlan pId_ =
  UpdateRescoreExecutionPlan'
    { capacityUnits =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      id = pId_
    }

-- | You can set additional capacity units to meet the needs of your rescore
-- execution plan. You are given a single capacity unit by default. If you
-- want to use the default capacity, you don\'t set additional capacity
-- units. For more information on the default capacity and additional
-- capacity units, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
updateRescoreExecutionPlan_capacityUnits :: Lens.Lens' UpdateRescoreExecutionPlan (Prelude.Maybe CapacityUnitsConfiguration)
updateRescoreExecutionPlan_capacityUnits = Lens.lens (\UpdateRescoreExecutionPlan' {capacityUnits} -> capacityUnits) (\s@UpdateRescoreExecutionPlan' {} a -> s {capacityUnits = a} :: UpdateRescoreExecutionPlan)

-- | A new description for the rescore execution plan.
updateRescoreExecutionPlan_description :: Lens.Lens' UpdateRescoreExecutionPlan (Prelude.Maybe Prelude.Text)
updateRescoreExecutionPlan_description = Lens.lens (\UpdateRescoreExecutionPlan' {description} -> description) (\s@UpdateRescoreExecutionPlan' {} a -> s {description = a} :: UpdateRescoreExecutionPlan)

-- | A new name for the rescore execution plan.
updateRescoreExecutionPlan_name :: Lens.Lens' UpdateRescoreExecutionPlan (Prelude.Maybe Prelude.Text)
updateRescoreExecutionPlan_name = Lens.lens (\UpdateRescoreExecutionPlan' {name} -> name) (\s@UpdateRescoreExecutionPlan' {} a -> s {name = a} :: UpdateRescoreExecutionPlan)

-- | The identifier of the rescore execution plan that you want to update.
updateRescoreExecutionPlan_id :: Lens.Lens' UpdateRescoreExecutionPlan Prelude.Text
updateRescoreExecutionPlan_id = Lens.lens (\UpdateRescoreExecutionPlan' {id} -> id) (\s@UpdateRescoreExecutionPlan' {} a -> s {id = a} :: UpdateRescoreExecutionPlan)

instance Core.AWSRequest UpdateRescoreExecutionPlan where
  type
    AWSResponse UpdateRescoreExecutionPlan =
      UpdateRescoreExecutionPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateRescoreExecutionPlanResponse'

instance Prelude.Hashable UpdateRescoreExecutionPlan where
  hashWithSalt _salt UpdateRescoreExecutionPlan' {..} =
    _salt
      `Prelude.hashWithSalt` capacityUnits
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateRescoreExecutionPlan where
  rnf UpdateRescoreExecutionPlan' {..} =
    Prelude.rnf capacityUnits
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateRescoreExecutionPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraRerankingFrontendService.UpdateRescoreExecutionPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRescoreExecutionPlan where
  toJSON UpdateRescoreExecutionPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CapacityUnits" Data..=) Prelude.<$> capacityUnits,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath UpdateRescoreExecutionPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRescoreExecutionPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRescoreExecutionPlanResponse' smart constructor.
data UpdateRescoreExecutionPlanResponse = UpdateRescoreExecutionPlanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRescoreExecutionPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRescoreExecutionPlanResponse ::
  UpdateRescoreExecutionPlanResponse
newUpdateRescoreExecutionPlanResponse =
  UpdateRescoreExecutionPlanResponse'

instance
  Prelude.NFData
    UpdateRescoreExecutionPlanResponse
  where
  rnf _ = ()

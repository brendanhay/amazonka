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
-- Module      : Amazonka.KendraRanking.DeleteRescoreExecutionPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a rescore execution plan. A rescore execution plan is an Amazon
-- Kendra Intelligent Ranking resource used for provisioning the @Rescore@
-- API.
module Amazonka.KendraRanking.DeleteRescoreExecutionPlan
  ( -- * Creating a Request
    DeleteRescoreExecutionPlan (..),
    newDeleteRescoreExecutionPlan,

    -- * Request Lenses
    deleteRescoreExecutionPlan_id,

    -- * Destructuring the Response
    DeleteRescoreExecutionPlanResponse (..),
    newDeleteRescoreExecutionPlanResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KendraRanking.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRescoreExecutionPlan' smart constructor.
data DeleteRescoreExecutionPlan = DeleteRescoreExecutionPlan'
  { -- | The identifier of the rescore execution plan that you want to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRescoreExecutionPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteRescoreExecutionPlan_id' - The identifier of the rescore execution plan that you want to delete.
newDeleteRescoreExecutionPlan ::
  -- | 'id'
  Prelude.Text ->
  DeleteRescoreExecutionPlan
newDeleteRescoreExecutionPlan pId_ =
  DeleteRescoreExecutionPlan' {id = pId_}

-- | The identifier of the rescore execution plan that you want to delete.
deleteRescoreExecutionPlan_id :: Lens.Lens' DeleteRescoreExecutionPlan Prelude.Text
deleteRescoreExecutionPlan_id = Lens.lens (\DeleteRescoreExecutionPlan' {id} -> id) (\s@DeleteRescoreExecutionPlan' {} a -> s {id = a} :: DeleteRescoreExecutionPlan)

instance Core.AWSRequest DeleteRescoreExecutionPlan where
  type
    AWSResponse DeleteRescoreExecutionPlan =
      DeleteRescoreExecutionPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteRescoreExecutionPlanResponse'

instance Prelude.Hashable DeleteRescoreExecutionPlan where
  hashWithSalt _salt DeleteRescoreExecutionPlan' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteRescoreExecutionPlan where
  rnf DeleteRescoreExecutionPlan' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteRescoreExecutionPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraRerankingFrontendService.DeleteRescoreExecutionPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRescoreExecutionPlan where
  toJSON DeleteRescoreExecutionPlan' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("Id" Data..= id)])

instance Data.ToPath DeleteRescoreExecutionPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRescoreExecutionPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRescoreExecutionPlanResponse' smart constructor.
data DeleteRescoreExecutionPlanResponse = DeleteRescoreExecutionPlanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRescoreExecutionPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRescoreExecutionPlanResponse ::
  DeleteRescoreExecutionPlanResponse
newDeleteRescoreExecutionPlanResponse =
  DeleteRescoreExecutionPlanResponse'

instance
  Prelude.NFData
    DeleteRescoreExecutionPlanResponse
  where
  rnf _ = ()

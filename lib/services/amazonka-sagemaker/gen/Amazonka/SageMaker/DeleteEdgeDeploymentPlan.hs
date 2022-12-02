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
-- Module      : Amazonka.SageMaker.DeleteEdgeDeploymentPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an edge deployment plan if (and only if) all the stages in the
-- plan are inactive or there are no stages in the plan.
module Amazonka.SageMaker.DeleteEdgeDeploymentPlan
  ( -- * Creating a Request
    DeleteEdgeDeploymentPlan (..),
    newDeleteEdgeDeploymentPlan,

    -- * Request Lenses
    deleteEdgeDeploymentPlan_edgeDeploymentPlanName,

    -- * Destructuring the Response
    DeleteEdgeDeploymentPlanResponse (..),
    newDeleteEdgeDeploymentPlanResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteEdgeDeploymentPlan' smart constructor.
data DeleteEdgeDeploymentPlan = DeleteEdgeDeploymentPlan'
  { -- | The name of the edge deployment plan to delete.
    edgeDeploymentPlanName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEdgeDeploymentPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeDeploymentPlanName', 'deleteEdgeDeploymentPlan_edgeDeploymentPlanName' - The name of the edge deployment plan to delete.
newDeleteEdgeDeploymentPlan ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  DeleteEdgeDeploymentPlan
newDeleteEdgeDeploymentPlan pEdgeDeploymentPlanName_ =
  DeleteEdgeDeploymentPlan'
    { edgeDeploymentPlanName =
        pEdgeDeploymentPlanName_
    }

-- | The name of the edge deployment plan to delete.
deleteEdgeDeploymentPlan_edgeDeploymentPlanName :: Lens.Lens' DeleteEdgeDeploymentPlan Prelude.Text
deleteEdgeDeploymentPlan_edgeDeploymentPlanName = Lens.lens (\DeleteEdgeDeploymentPlan' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@DeleteEdgeDeploymentPlan' {} a -> s {edgeDeploymentPlanName = a} :: DeleteEdgeDeploymentPlan)

instance Core.AWSRequest DeleteEdgeDeploymentPlan where
  type
    AWSResponse DeleteEdgeDeploymentPlan =
      DeleteEdgeDeploymentPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteEdgeDeploymentPlanResponse'

instance Prelude.Hashable DeleteEdgeDeploymentPlan where
  hashWithSalt _salt DeleteEdgeDeploymentPlan' {..} =
    _salt `Prelude.hashWithSalt` edgeDeploymentPlanName

instance Prelude.NFData DeleteEdgeDeploymentPlan where
  rnf DeleteEdgeDeploymentPlan' {..} =
    Prelude.rnf edgeDeploymentPlanName

instance Data.ToHeaders DeleteEdgeDeploymentPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteEdgeDeploymentPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEdgeDeploymentPlan where
  toJSON DeleteEdgeDeploymentPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Data..= edgeDeploymentPlanName
              )
          ]
      )

instance Data.ToPath DeleteEdgeDeploymentPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEdgeDeploymentPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEdgeDeploymentPlanResponse' smart constructor.
data DeleteEdgeDeploymentPlanResponse = DeleteEdgeDeploymentPlanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEdgeDeploymentPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEdgeDeploymentPlanResponse ::
  DeleteEdgeDeploymentPlanResponse
newDeleteEdgeDeploymentPlanResponse =
  DeleteEdgeDeploymentPlanResponse'

instance
  Prelude.NFData
    DeleteEdgeDeploymentPlanResponse
  where
  rnf _ = ()

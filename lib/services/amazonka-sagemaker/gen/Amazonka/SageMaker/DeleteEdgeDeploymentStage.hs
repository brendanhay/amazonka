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
-- Module      : Amazonka.SageMaker.DeleteEdgeDeploymentStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a stage in an edge deployment plan if (and only if) the stage is
-- inactive.
module Amazonka.SageMaker.DeleteEdgeDeploymentStage
  ( -- * Creating a Request
    DeleteEdgeDeploymentStage (..),
    newDeleteEdgeDeploymentStage,

    -- * Request Lenses
    deleteEdgeDeploymentStage_edgeDeploymentPlanName,
    deleteEdgeDeploymentStage_stageName,

    -- * Destructuring the Response
    DeleteEdgeDeploymentStageResponse (..),
    newDeleteEdgeDeploymentStageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteEdgeDeploymentStage' smart constructor.
data DeleteEdgeDeploymentStage = DeleteEdgeDeploymentStage'
  { -- | The name of the edge deployment plan from which the stage will be
    -- deleted.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEdgeDeploymentStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeDeploymentPlanName', 'deleteEdgeDeploymentStage_edgeDeploymentPlanName' - The name of the edge deployment plan from which the stage will be
-- deleted.
--
-- 'stageName', 'deleteEdgeDeploymentStage_stageName' - The name of the stage.
newDeleteEdgeDeploymentStage ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  DeleteEdgeDeploymentStage
newDeleteEdgeDeploymentStage
  pEdgeDeploymentPlanName_
  pStageName_ =
    DeleteEdgeDeploymentStage'
      { edgeDeploymentPlanName =
          pEdgeDeploymentPlanName_,
        stageName = pStageName_
      }

-- | The name of the edge deployment plan from which the stage will be
-- deleted.
deleteEdgeDeploymentStage_edgeDeploymentPlanName :: Lens.Lens' DeleteEdgeDeploymentStage Prelude.Text
deleteEdgeDeploymentStage_edgeDeploymentPlanName = Lens.lens (\DeleteEdgeDeploymentStage' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@DeleteEdgeDeploymentStage' {} a -> s {edgeDeploymentPlanName = a} :: DeleteEdgeDeploymentStage)

-- | The name of the stage.
deleteEdgeDeploymentStage_stageName :: Lens.Lens' DeleteEdgeDeploymentStage Prelude.Text
deleteEdgeDeploymentStage_stageName = Lens.lens (\DeleteEdgeDeploymentStage' {stageName} -> stageName) (\s@DeleteEdgeDeploymentStage' {} a -> s {stageName = a} :: DeleteEdgeDeploymentStage)

instance Core.AWSRequest DeleteEdgeDeploymentStage where
  type
    AWSResponse DeleteEdgeDeploymentStage =
      DeleteEdgeDeploymentStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteEdgeDeploymentStageResponse'

instance Prelude.Hashable DeleteEdgeDeploymentStage where
  hashWithSalt _salt DeleteEdgeDeploymentStage' {..} =
    _salt
      `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData DeleteEdgeDeploymentStage where
  rnf DeleteEdgeDeploymentStage' {..} =
    Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders DeleteEdgeDeploymentStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteEdgeDeploymentStage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEdgeDeploymentStage where
  toJSON DeleteEdgeDeploymentStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Data..= edgeDeploymentPlanName
              ),
            Prelude.Just ("StageName" Data..= stageName)
          ]
      )

instance Data.ToPath DeleteEdgeDeploymentStage where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEdgeDeploymentStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEdgeDeploymentStageResponse' smart constructor.
data DeleteEdgeDeploymentStageResponse = DeleteEdgeDeploymentStageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEdgeDeploymentStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEdgeDeploymentStageResponse ::
  DeleteEdgeDeploymentStageResponse
newDeleteEdgeDeploymentStageResponse =
  DeleteEdgeDeploymentStageResponse'

instance
  Prelude.NFData
    DeleteEdgeDeploymentStageResponse
  where
  rnf _ = ()

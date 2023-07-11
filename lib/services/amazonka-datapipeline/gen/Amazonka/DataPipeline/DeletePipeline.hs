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
-- Module      : Amazonka.DataPipeline.DeletePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a pipeline, its pipeline definition, and its run history. AWS
-- Data Pipeline attempts to cancel instances associated with the pipeline
-- that are currently being processed by task runners.
--
-- Deleting a pipeline cannot be undone. You cannot query or restore a
-- deleted pipeline. To temporarily pause a pipeline instead of deleting
-- it, call SetStatus with the status set to @PAUSE@ on individual
-- components. Components that are paused by SetStatus can be resumed.
module Amazonka.DataPipeline.DeletePipeline
  ( -- * Creating a Request
    DeletePipeline (..),
    newDeletePipeline,

    -- * Request Lenses
    deletePipeline_pipelineId,

    -- * Destructuring the Response
    DeletePipelineResponse (..),
    newDeletePipelineResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeletePipeline.
--
-- /See:/ 'newDeletePipeline' smart constructor.
data DeletePipeline = DeletePipeline'
  { -- | The ID of the pipeline.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'deletePipeline_pipelineId' - The ID of the pipeline.
newDeletePipeline ::
  -- | 'pipelineId'
  Prelude.Text ->
  DeletePipeline
newDeletePipeline pPipelineId_ =
  DeletePipeline' {pipelineId = pPipelineId_}

-- | The ID of the pipeline.
deletePipeline_pipelineId :: Lens.Lens' DeletePipeline Prelude.Text
deletePipeline_pipelineId = Lens.lens (\DeletePipeline' {pipelineId} -> pipelineId) (\s@DeletePipeline' {} a -> s {pipelineId = a} :: DeletePipeline)

instance Core.AWSRequest DeletePipeline where
  type
    AWSResponse DeletePipeline =
      DeletePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeletePipelineResponse'

instance Prelude.Hashable DeletePipeline where
  hashWithSalt _salt DeletePipeline' {..} =
    _salt `Prelude.hashWithSalt` pipelineId

instance Prelude.NFData DeletePipeline where
  rnf DeletePipeline' {..} = Prelude.rnf pipelineId

instance Data.ToHeaders DeletePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DataPipeline.DeletePipeline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePipeline where
  toJSON DeletePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("pipelineId" Data..= pipelineId)]
      )

instance Data.ToPath DeletePipeline where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePipelineResponse ::
  DeletePipelineResponse
newDeletePipelineResponse = DeletePipelineResponse'

instance Prelude.NFData DeletePipelineResponse where
  rnf _ = ()

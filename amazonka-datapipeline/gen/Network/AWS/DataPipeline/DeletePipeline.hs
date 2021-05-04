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
-- Module      : Network.AWS.DataPipeline.DeletePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.DataPipeline.DeletePipeline
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

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeletePipeline.
--
-- /See:/ 'newDeletePipeline' smart constructor.
data DeletePipeline = DeletePipeline'
  { -- | The ID of the pipeline.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeletePipeline where
  type Rs DeletePipeline = DeletePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeletePipelineResponse'

instance Prelude.Hashable DeletePipeline

instance Prelude.NFData DeletePipeline

instance Prelude.ToHeaders DeletePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DataPipeline.DeletePipeline" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeletePipeline where
  toJSON DeletePipeline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("pipelineId" Prelude..= pipelineId)]
      )

instance Prelude.ToPath DeletePipeline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePipelineResponse ::
  DeletePipelineResponse
newDeletePipelineResponse = DeletePipelineResponse'

instance Prelude.NFData DeletePipelineResponse

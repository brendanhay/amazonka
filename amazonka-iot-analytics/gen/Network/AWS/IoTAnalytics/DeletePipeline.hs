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
-- Module      : Network.AWS.IoTAnalytics.DeletePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
module Network.AWS.IoTAnalytics.DeletePipeline
  ( -- * Creating a Request
    DeletePipeline (..),
    newDeletePipeline,

    -- * Request Lenses
    deletePipeline_pipelineName,

    -- * Destructuring the Response
    DeletePipelineResponse (..),
    newDeletePipelineResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePipeline' smart constructor.
data DeletePipeline = DeletePipeline'
  { -- | The name of the pipeline to delete.
    pipelineName :: Prelude.Text
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
-- 'pipelineName', 'deletePipeline_pipelineName' - The name of the pipeline to delete.
newDeletePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  DeletePipeline
newDeletePipeline pPipelineName_ =
  DeletePipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to delete.
deletePipeline_pipelineName :: Lens.Lens' DeletePipeline Prelude.Text
deletePipeline_pipelineName = Lens.lens (\DeletePipeline' {pipelineName} -> pipelineName) (\s@DeletePipeline' {} a -> s {pipelineName = a} :: DeletePipeline)

instance Prelude.AWSRequest DeletePipeline where
  type Rs DeletePipeline = DeletePipelineResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeletePipelineResponse'

instance Prelude.Hashable DeletePipeline

instance Prelude.NFData DeletePipeline

instance Prelude.ToHeaders DeletePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeletePipeline where
  toPath DeletePipeline' {..} =
    Prelude.mconcat
      ["/pipelines/", Prelude.toBS pipelineName]

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

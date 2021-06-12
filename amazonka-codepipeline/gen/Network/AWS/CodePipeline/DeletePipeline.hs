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
-- Module      : Network.AWS.CodePipeline.DeletePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
module Network.AWS.CodePipeline.DeletePipeline
  ( -- * Creating a Request
    DeletePipeline (..),
    newDeletePipeline,

    -- * Request Lenses
    deletePipeline_name,

    -- * Destructuring the Response
    DeletePipelineResponse (..),
    newDeletePipelineResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeletePipeline@ action.
--
-- /See:/ 'newDeletePipeline' smart constructor.
data DeletePipeline = DeletePipeline'
  { -- | The name of the pipeline to be deleted.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deletePipeline_name' - The name of the pipeline to be deleted.
newDeletePipeline ::
  -- | 'name'
  Core.Text ->
  DeletePipeline
newDeletePipeline pName_ =
  DeletePipeline' {name = pName_}

-- | The name of the pipeline to be deleted.
deletePipeline_name :: Lens.Lens' DeletePipeline Core.Text
deletePipeline_name = Lens.lens (\DeletePipeline' {name} -> name) (\s@DeletePipeline' {} a -> s {name = a} :: DeletePipeline)

instance Core.AWSRequest DeletePipeline where
  type
    AWSResponse DeletePipeline =
      DeletePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeletePipelineResponse'

instance Core.Hashable DeletePipeline

instance Core.NFData DeletePipeline

instance Core.ToHeaders DeletePipeline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.DeletePipeline" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePipeline where
  toJSON DeletePipeline' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.ToPath DeletePipeline where
  toPath = Core.const "/"

instance Core.ToQuery DeletePipeline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePipelineResponse ::
  DeletePipelineResponse
newDeletePipelineResponse = DeletePipelineResponse'

instance Core.NFData DeletePipelineResponse

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
-- Module      : Network.AWS.CodePipeline.GetPipelineState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the state of a pipeline, including the stages
-- and actions.
--
-- Values returned in the @revisionId@ and @revisionUrl@ fields indicate
-- the source revision information, such as the commit ID, for the current
-- state.
module Network.AWS.CodePipeline.GetPipelineState
  ( -- * Creating a Request
    GetPipelineState (..),
    newGetPipelineState,

    -- * Request Lenses
    getPipelineState_name,

    -- * Destructuring the Response
    GetPipelineStateResponse (..),
    newGetPipelineStateResponse,

    -- * Response Lenses
    getPipelineStateResponse_stageStates,
    getPipelineStateResponse_created,
    getPipelineStateResponse_pipelineVersion,
    getPipelineStateResponse_updated,
    getPipelineStateResponse_pipelineName,
    getPipelineStateResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipelineState@ action.
--
-- /See:/ 'newGetPipelineState' smart constructor.
data GetPipelineState = GetPipelineState'
  { -- | The name of the pipeline about which you want to get information.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPipelineState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getPipelineState_name' - The name of the pipeline about which you want to get information.
newGetPipelineState ::
  -- | 'name'
  Core.Text ->
  GetPipelineState
newGetPipelineState pName_ =
  GetPipelineState' {name = pName_}

-- | The name of the pipeline about which you want to get information.
getPipelineState_name :: Lens.Lens' GetPipelineState Core.Text
getPipelineState_name = Lens.lens (\GetPipelineState' {name} -> name) (\s@GetPipelineState' {} a -> s {name = a} :: GetPipelineState)

instance Core.AWSRequest GetPipelineState where
  type
    AWSResponse GetPipelineState =
      GetPipelineStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineStateResponse'
            Core.<$> (x Core..?> "stageStates" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "created")
            Core.<*> (x Core..?> "pipelineVersion")
            Core.<*> (x Core..?> "updated")
            Core.<*> (x Core..?> "pipelineName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPipelineState

instance Core.NFData GetPipelineState

instance Core.ToHeaders GetPipelineState where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetPipelineState" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPipelineState where
  toJSON GetPipelineState' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.ToPath GetPipelineState where
  toPath = Core.const "/"

instance Core.ToQuery GetPipelineState where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetPipelineState@ action.
--
-- /See:/ 'newGetPipelineStateResponse' smart constructor.
data GetPipelineStateResponse = GetPipelineStateResponse'
  { -- | A list of the pipeline stage output information, including stage name,
    -- state, most recent run details, whether the stage is disabled, and other
    -- data.
    stageStates :: Core.Maybe [StageState],
    -- | The date and time the pipeline was created, in timestamp format.
    created :: Core.Maybe Core.POSIX,
    -- | The version number of the pipeline.
    --
    -- A newly created pipeline is always assigned a version number of @1@.
    pipelineVersion :: Core.Maybe Core.Natural,
    -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Core.Maybe Core.POSIX,
    -- | The name of the pipeline for which you want to get the state.
    pipelineName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPipelineStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageStates', 'getPipelineStateResponse_stageStates' - A list of the pipeline stage output information, including stage name,
-- state, most recent run details, whether the stage is disabled, and other
-- data.
--
-- 'created', 'getPipelineStateResponse_created' - The date and time the pipeline was created, in timestamp format.
--
-- 'pipelineVersion', 'getPipelineStateResponse_pipelineVersion' - The version number of the pipeline.
--
-- A newly created pipeline is always assigned a version number of @1@.
--
-- 'updated', 'getPipelineStateResponse_updated' - The date and time the pipeline was last updated, in timestamp format.
--
-- 'pipelineName', 'getPipelineStateResponse_pipelineName' - The name of the pipeline for which you want to get the state.
--
-- 'httpStatus', 'getPipelineStateResponse_httpStatus' - The response's http status code.
newGetPipelineStateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPipelineStateResponse
newGetPipelineStateResponse pHttpStatus_ =
  GetPipelineStateResponse'
    { stageStates =
        Core.Nothing,
      created = Core.Nothing,
      pipelineVersion = Core.Nothing,
      updated = Core.Nothing,
      pipelineName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the pipeline stage output information, including stage name,
-- state, most recent run details, whether the stage is disabled, and other
-- data.
getPipelineStateResponse_stageStates :: Lens.Lens' GetPipelineStateResponse (Core.Maybe [StageState])
getPipelineStateResponse_stageStates = Lens.lens (\GetPipelineStateResponse' {stageStates} -> stageStates) (\s@GetPipelineStateResponse' {} a -> s {stageStates = a} :: GetPipelineStateResponse) Core.. Lens.mapping Lens._Coerce

-- | The date and time the pipeline was created, in timestamp format.
getPipelineStateResponse_created :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Core.UTCTime)
getPipelineStateResponse_created = Lens.lens (\GetPipelineStateResponse' {created} -> created) (\s@GetPipelineStateResponse' {} a -> s {created = a} :: GetPipelineStateResponse) Core.. Lens.mapping Core._Time

-- | The version number of the pipeline.
--
-- A newly created pipeline is always assigned a version number of @1@.
getPipelineStateResponse_pipelineVersion :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Core.Natural)
getPipelineStateResponse_pipelineVersion = Lens.lens (\GetPipelineStateResponse' {pipelineVersion} -> pipelineVersion) (\s@GetPipelineStateResponse' {} a -> s {pipelineVersion = a} :: GetPipelineStateResponse)

-- | The date and time the pipeline was last updated, in timestamp format.
getPipelineStateResponse_updated :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Core.UTCTime)
getPipelineStateResponse_updated = Lens.lens (\GetPipelineStateResponse' {updated} -> updated) (\s@GetPipelineStateResponse' {} a -> s {updated = a} :: GetPipelineStateResponse) Core.. Lens.mapping Core._Time

-- | The name of the pipeline for which you want to get the state.
getPipelineStateResponse_pipelineName :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Core.Text)
getPipelineStateResponse_pipelineName = Lens.lens (\GetPipelineStateResponse' {pipelineName} -> pipelineName) (\s@GetPipelineStateResponse' {} a -> s {pipelineName = a} :: GetPipelineStateResponse)

-- | The response's http status code.
getPipelineStateResponse_httpStatus :: Lens.Lens' GetPipelineStateResponse Core.Int
getPipelineStateResponse_httpStatus = Lens.lens (\GetPipelineStateResponse' {httpStatus} -> httpStatus) (\s@GetPipelineStateResponse' {} a -> s {httpStatus = a} :: GetPipelineStateResponse)

instance Core.NFData GetPipelineStateResponse

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
-- Module      : Network.AWS.IoTAnalytics.CreatePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline. A pipeline consumes messages from a channel and
-- allows you to process the messages before storing them in a data store.
-- You must specify both a @channel@ and a @datastore@ activity and,
-- optionally, as many as 23 additional activities in the
-- @pipelineActivities@ array.
module Network.AWS.IoTAnalytics.CreatePipeline
  ( -- * Creating a Request
    CreatePipeline (..),
    newCreatePipeline,

    -- * Request Lenses
    createPipeline_tags,
    createPipeline_pipelineName,
    createPipeline_pipelineActivities,

    -- * Destructuring the Response
    CreatePipelineResponse (..),
    newCreatePipelineResponse,

    -- * Response Lenses
    createPipelineResponse_pipelineArn,
    createPipelineResponse_pipelineName,
    createPipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | Metadata which can be used to manage the pipeline.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The name of the pipeline.
    pipelineName :: Core.Text,
    -- | A list of @PipelineActivity@ objects. Activities perform transformations
    -- on your messages, such as removing, renaming or adding message
    -- attributes; filtering messages based on attribute values; invoking your
    -- Lambda functions on messages for advanced processing; or performing
    -- mathematical transformations to normalize device data.
    --
    -- The list can be 2-25 @PipelineActivity@ objects and must contain both a
    -- @channel@ and a @datastore@ activity. Each entry in the list must
    -- contain only one activity. For example:
    --
    -- @pipelineActivities = [ { \"channel\": { ... } }, { \"lambda\": { ... } }, ... ]@
    pipelineActivities :: Core.NonEmpty PipelineActivity
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPipeline_tags' - Metadata which can be used to manage the pipeline.
--
-- 'pipelineName', 'createPipeline_pipelineName' - The name of the pipeline.
--
-- 'pipelineActivities', 'createPipeline_pipelineActivities' - A list of @PipelineActivity@ objects. Activities perform transformations
-- on your messages, such as removing, renaming or adding message
-- attributes; filtering messages based on attribute values; invoking your
-- Lambda functions on messages for advanced processing; or performing
-- mathematical transformations to normalize device data.
--
-- The list can be 2-25 @PipelineActivity@ objects and must contain both a
-- @channel@ and a @datastore@ activity. Each entry in the list must
-- contain only one activity. For example:
--
-- @pipelineActivities = [ { \"channel\": { ... } }, { \"lambda\": { ... } }, ... ]@
newCreatePipeline ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'pipelineActivities'
  Core.NonEmpty PipelineActivity ->
  CreatePipeline
newCreatePipeline pPipelineName_ pPipelineActivities_ =
  CreatePipeline'
    { tags = Core.Nothing,
      pipelineName = pPipelineName_,
      pipelineActivities =
        Lens._Coerce Lens.# pPipelineActivities_
    }

-- | Metadata which can be used to manage the pipeline.
createPipeline_tags :: Lens.Lens' CreatePipeline (Core.Maybe (Core.NonEmpty Tag))
createPipeline_tags = Lens.lens (\CreatePipeline' {tags} -> tags) (\s@CreatePipeline' {} a -> s {tags = a} :: CreatePipeline) Core.. Lens.mapping Lens._Coerce

-- | The name of the pipeline.
createPipeline_pipelineName :: Lens.Lens' CreatePipeline Core.Text
createPipeline_pipelineName = Lens.lens (\CreatePipeline' {pipelineName} -> pipelineName) (\s@CreatePipeline' {} a -> s {pipelineName = a} :: CreatePipeline)

-- | A list of @PipelineActivity@ objects. Activities perform transformations
-- on your messages, such as removing, renaming or adding message
-- attributes; filtering messages based on attribute values; invoking your
-- Lambda functions on messages for advanced processing; or performing
-- mathematical transformations to normalize device data.
--
-- The list can be 2-25 @PipelineActivity@ objects and must contain both a
-- @channel@ and a @datastore@ activity. Each entry in the list must
-- contain only one activity. For example:
--
-- @pipelineActivities = [ { \"channel\": { ... } }, { \"lambda\": { ... } }, ... ]@
createPipeline_pipelineActivities :: Lens.Lens' CreatePipeline (Core.NonEmpty PipelineActivity)
createPipeline_pipelineActivities = Lens.lens (\CreatePipeline' {pipelineActivities} -> pipelineActivities) (\s@CreatePipeline' {} a -> s {pipelineActivities = a} :: CreatePipeline) Core.. Lens._Coerce

instance Core.AWSRequest CreatePipeline where
  type
    AWSResponse CreatePipeline =
      CreatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Core.<$> (x Core..?> "pipelineArn")
            Core.<*> (x Core..?> "pipelineName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePipeline

instance Core.NFData CreatePipeline

instance Core.ToHeaders CreatePipeline where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just
              ("pipelineActivities" Core..= pipelineActivities)
          ]
      )

instance Core.ToPath CreatePipeline where
  toPath = Core.const "/pipelines"

instance Core.ToQuery CreatePipeline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | The ARN of the pipeline.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The name of the pipeline.
    pipelineName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'createPipelineResponse_pipelineArn' - The ARN of the pipeline.
--
-- 'pipelineName', 'createPipelineResponse_pipelineName' - The name of the pipeline.
--
-- 'httpStatus', 'createPipelineResponse_httpStatus' - The response's http status code.
newCreatePipelineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePipelineResponse
newCreatePipelineResponse pHttpStatus_ =
  CreatePipelineResponse'
    { pipelineArn = Core.Nothing,
      pipelineName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the pipeline.
createPipelineResponse_pipelineArn :: Lens.Lens' CreatePipelineResponse (Core.Maybe Core.Text)
createPipelineResponse_pipelineArn = Lens.lens (\CreatePipelineResponse' {pipelineArn} -> pipelineArn) (\s@CreatePipelineResponse' {} a -> s {pipelineArn = a} :: CreatePipelineResponse)

-- | The name of the pipeline.
createPipelineResponse_pipelineName :: Lens.Lens' CreatePipelineResponse (Core.Maybe Core.Text)
createPipelineResponse_pipelineName = Lens.lens (\CreatePipelineResponse' {pipelineName} -> pipelineName) (\s@CreatePipelineResponse' {} a -> s {pipelineName = a} :: CreatePipelineResponse)

-- | The response's http status code.
createPipelineResponse_httpStatus :: Lens.Lens' CreatePipelineResponse Core.Int
createPipelineResponse_httpStatus = Lens.lens (\CreatePipelineResponse' {httpStatus} -> httpStatus) (\s@CreatePipelineResponse' {} a -> s {httpStatus = a} :: CreatePipelineResponse)

instance Core.NFData CreatePipelineResponse

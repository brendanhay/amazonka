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
-- Module      : Network.AWS.IoTAnalytics.UpdatePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a pipeline. You must specify both a @channel@
-- and a @datastore@ activity and, optionally, as many as 23 additional
-- activities in the @pipelineActivities@ array.
module Network.AWS.IoTAnalytics.UpdatePipeline
  ( -- * Creating a Request
    UpdatePipeline (..),
    newUpdatePipeline,

    -- * Request Lenses
    updatePipeline_pipelineName,
    updatePipeline_pipelineActivities,

    -- * Destructuring the Response
    UpdatePipelineResponse (..),
    newUpdatePipelineResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | The name of the pipeline to update.
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
-- Create a value of 'UpdatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'updatePipeline_pipelineName' - The name of the pipeline to update.
--
-- 'pipelineActivities', 'updatePipeline_pipelineActivities' - A list of @PipelineActivity@ objects. Activities perform transformations
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
newUpdatePipeline ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'pipelineActivities'
  Core.NonEmpty PipelineActivity ->
  UpdatePipeline
newUpdatePipeline pPipelineName_ pPipelineActivities_ =
  UpdatePipeline'
    { pipelineName = pPipelineName_,
      pipelineActivities =
        Lens._Coerce Lens.# pPipelineActivities_
    }

-- | The name of the pipeline to update.
updatePipeline_pipelineName :: Lens.Lens' UpdatePipeline Core.Text
updatePipeline_pipelineName = Lens.lens (\UpdatePipeline' {pipelineName} -> pipelineName) (\s@UpdatePipeline' {} a -> s {pipelineName = a} :: UpdatePipeline)

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
updatePipeline_pipelineActivities :: Lens.Lens' UpdatePipeline (Core.NonEmpty PipelineActivity)
updatePipeline_pipelineActivities = Lens.lens (\UpdatePipeline' {pipelineActivities} -> pipelineActivities) (\s@UpdatePipeline' {} a -> s {pipelineActivities = a} :: UpdatePipeline) Core.. Lens._Coerce

instance Core.AWSRequest UpdatePipeline where
  type
    AWSResponse UpdatePipeline =
      UpdatePipelineResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdatePipelineResponse'

instance Core.Hashable UpdatePipeline

instance Core.NFData UpdatePipeline

instance Core.ToHeaders UpdatePipeline where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("pipelineActivities" Core..= pipelineActivities)
          ]
      )

instance Core.ToPath UpdatePipeline where
  toPath UpdatePipeline' {..} =
    Core.mconcat
      ["/pipelines/", Core.toBS pipelineName]

instance Core.ToQuery UpdatePipeline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdatePipelineResponse ::
  UpdatePipelineResponse
newUpdatePipelineResponse = UpdatePipelineResponse'

instance Core.NFData UpdatePipelineResponse

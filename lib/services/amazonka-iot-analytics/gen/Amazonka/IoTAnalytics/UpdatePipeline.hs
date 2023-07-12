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
-- Module      : Amazonka.IoTAnalytics.UpdatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a pipeline. You must specify both a @channel@
-- and a @datastore@ activity and, optionally, as many as 23 additional
-- activities in the @pipelineActivities@ array.
module Amazonka.IoTAnalytics.UpdatePipeline
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | The name of the pipeline to update.
    pipelineName :: Prelude.Text,
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
    pipelineActivities :: Prelude.NonEmpty PipelineActivity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'pipelineActivities'
  Prelude.NonEmpty PipelineActivity ->
  UpdatePipeline
newUpdatePipeline pPipelineName_ pPipelineActivities_ =
  UpdatePipeline'
    { pipelineName = pPipelineName_,
      pipelineActivities =
        Lens.coerced Lens.# pPipelineActivities_
    }

-- | The name of the pipeline to update.
updatePipeline_pipelineName :: Lens.Lens' UpdatePipeline Prelude.Text
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
updatePipeline_pipelineActivities :: Lens.Lens' UpdatePipeline (Prelude.NonEmpty PipelineActivity)
updatePipeline_pipelineActivities = Lens.lens (\UpdatePipeline' {pipelineActivities} -> pipelineActivities) (\s@UpdatePipeline' {} a -> s {pipelineActivities = a} :: UpdatePipeline) Prelude.. Lens.coerced

instance Core.AWSRequest UpdatePipeline where
  type
    AWSResponse UpdatePipeline =
      UpdatePipelineResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdatePipelineResponse'

instance Prelude.Hashable UpdatePipeline where
  hashWithSalt _salt UpdatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` pipelineActivities

instance Prelude.NFData UpdatePipeline where
  rnf UpdatePipeline' {..} =
    Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf pipelineActivities

instance Data.ToHeaders UpdatePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pipelineActivities" Data..= pipelineActivities)
          ]
      )

instance Data.ToPath UpdatePipeline where
  toPath UpdatePipeline' {..} =
    Prelude.mconcat
      ["/pipelines/", Data.toBS pipelineName]

instance Data.ToQuery UpdatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdatePipelineResponse ::
  UpdatePipelineResponse
newUpdatePipelineResponse = UpdatePipelineResponse'

instance Prelude.NFData UpdatePipelineResponse where
  rnf _ = ()

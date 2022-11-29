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
-- Module      : Amazonka.CodePipeline.GetPipelineState
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodePipeline.GetPipelineState
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
    getPipelineStateResponse_updated,
    getPipelineStateResponse_created,
    getPipelineStateResponse_pipelineVersion,
    getPipelineStateResponse_pipelineName,
    getPipelineStateResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetPipelineState@ action.
--
-- /See:/ 'newGetPipelineState' smart constructor.
data GetPipelineState = GetPipelineState'
  { -- | The name of the pipeline about which you want to get information.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetPipelineState
newGetPipelineState pName_ =
  GetPipelineState' {name = pName_}

-- | The name of the pipeline about which you want to get information.
getPipelineState_name :: Lens.Lens' GetPipelineState Prelude.Text
getPipelineState_name = Lens.lens (\GetPipelineState' {name} -> name) (\s@GetPipelineState' {} a -> s {name = a} :: GetPipelineState)

instance Core.AWSRequest GetPipelineState where
  type
    AWSResponse GetPipelineState =
      GetPipelineStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineStateResponse'
            Prelude.<$> (x Core..?> "stageStates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "updated")
            Prelude.<*> (x Core..?> "created")
            Prelude.<*> (x Core..?> "pipelineVersion")
            Prelude.<*> (x Core..?> "pipelineName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipelineState where
  hashWithSalt _salt GetPipelineState' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetPipelineState where
  rnf GetPipelineState' {..} = Prelude.rnf name

instance Core.ToHeaders GetPipelineState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetPipelineState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPipelineState where
  toJSON GetPipelineState' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath GetPipelineState where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPipelineState where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetPipelineState@ action.
--
-- /See:/ 'newGetPipelineStateResponse' smart constructor.
data GetPipelineStateResponse = GetPipelineStateResponse'
  { -- | A list of the pipeline stage output information, including stage name,
    -- state, most recent run details, whether the stage is disabled, and other
    -- data.
    stageStates :: Prelude.Maybe [StageState],
    -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Prelude.Maybe Core.POSIX,
    -- | The date and time the pipeline was created, in timestamp format.
    created :: Prelude.Maybe Core.POSIX,
    -- | The version number of the pipeline.
    --
    -- A newly created pipeline is always assigned a version number of @1@.
    pipelineVersion :: Prelude.Maybe Prelude.Natural,
    -- | The name of the pipeline for which you want to get the state.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'updated', 'getPipelineStateResponse_updated' - The date and time the pipeline was last updated, in timestamp format.
--
-- 'created', 'getPipelineStateResponse_created' - The date and time the pipeline was created, in timestamp format.
--
-- 'pipelineVersion', 'getPipelineStateResponse_pipelineVersion' - The version number of the pipeline.
--
-- A newly created pipeline is always assigned a version number of @1@.
--
-- 'pipelineName', 'getPipelineStateResponse_pipelineName' - The name of the pipeline for which you want to get the state.
--
-- 'httpStatus', 'getPipelineStateResponse_httpStatus' - The response's http status code.
newGetPipelineStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPipelineStateResponse
newGetPipelineStateResponse pHttpStatus_ =
  GetPipelineStateResponse'
    { stageStates =
        Prelude.Nothing,
      updated = Prelude.Nothing,
      created = Prelude.Nothing,
      pipelineVersion = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the pipeline stage output information, including stage name,
-- state, most recent run details, whether the stage is disabled, and other
-- data.
getPipelineStateResponse_stageStates :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe [StageState])
getPipelineStateResponse_stageStates = Lens.lens (\GetPipelineStateResponse' {stageStates} -> stageStates) (\s@GetPipelineStateResponse' {} a -> s {stageStates = a} :: GetPipelineStateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the pipeline was last updated, in timestamp format.
getPipelineStateResponse_updated :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.UTCTime)
getPipelineStateResponse_updated = Lens.lens (\GetPipelineStateResponse' {updated} -> updated) (\s@GetPipelineStateResponse' {} a -> s {updated = a} :: GetPipelineStateResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time the pipeline was created, in timestamp format.
getPipelineStateResponse_created :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.UTCTime)
getPipelineStateResponse_created = Lens.lens (\GetPipelineStateResponse' {created} -> created) (\s@GetPipelineStateResponse' {} a -> s {created = a} :: GetPipelineStateResponse) Prelude.. Lens.mapping Core._Time

-- | The version number of the pipeline.
--
-- A newly created pipeline is always assigned a version number of @1@.
getPipelineStateResponse_pipelineVersion :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.Natural)
getPipelineStateResponse_pipelineVersion = Lens.lens (\GetPipelineStateResponse' {pipelineVersion} -> pipelineVersion) (\s@GetPipelineStateResponse' {} a -> s {pipelineVersion = a} :: GetPipelineStateResponse)

-- | The name of the pipeline for which you want to get the state.
getPipelineStateResponse_pipelineName :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.Text)
getPipelineStateResponse_pipelineName = Lens.lens (\GetPipelineStateResponse' {pipelineName} -> pipelineName) (\s@GetPipelineStateResponse' {} a -> s {pipelineName = a} :: GetPipelineStateResponse)

-- | The response's http status code.
getPipelineStateResponse_httpStatus :: Lens.Lens' GetPipelineStateResponse Prelude.Int
getPipelineStateResponse_httpStatus = Lens.lens (\GetPipelineStateResponse' {httpStatus} -> httpStatus) (\s@GetPipelineStateResponse' {} a -> s {httpStatus = a} :: GetPipelineStateResponse)

instance Prelude.NFData GetPipelineStateResponse where
  rnf GetPipelineStateResponse' {..} =
    Prelude.rnf stageStates
      `Prelude.seq` Prelude.rnf updated
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf pipelineVersion
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf httpStatus

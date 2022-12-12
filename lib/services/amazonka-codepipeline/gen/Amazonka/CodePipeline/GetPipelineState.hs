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
    getPipelineStateResponse_created,
    getPipelineStateResponse_pipelineName,
    getPipelineStateResponse_pipelineVersion,
    getPipelineStateResponse_stageStates,
    getPipelineStateResponse_updated,
    getPipelineStateResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "created")
            Prelude.<*> (x Data..?> "pipelineName")
            Prelude.<*> (x Data..?> "pipelineVersion")
            Prelude.<*> (x Data..?> "stageStates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "updated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipelineState where
  hashWithSalt _salt GetPipelineState' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetPipelineState where
  rnf GetPipelineState' {..} = Prelude.rnf name

instance Data.ToHeaders GetPipelineState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.GetPipelineState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPipelineState where
  toJSON GetPipelineState' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath GetPipelineState where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPipelineState where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetPipelineState@ action.
--
-- /See:/ 'newGetPipelineStateResponse' smart constructor.
data GetPipelineStateResponse = GetPipelineStateResponse'
  { -- | The date and time the pipeline was created, in timestamp format.
    created :: Prelude.Maybe Data.POSIX,
    -- | The name of the pipeline for which you want to get the state.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the pipeline.
    --
    -- A newly created pipeline is always assigned a version number of @1@.
    pipelineVersion :: Prelude.Maybe Prelude.Natural,
    -- | A list of the pipeline stage output information, including stage name,
    -- state, most recent run details, whether the stage is disabled, and other
    -- data.
    stageStates :: Prelude.Maybe [StageState],
    -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Prelude.Maybe Data.POSIX,
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
-- 'created', 'getPipelineStateResponse_created' - The date and time the pipeline was created, in timestamp format.
--
-- 'pipelineName', 'getPipelineStateResponse_pipelineName' - The name of the pipeline for which you want to get the state.
--
-- 'pipelineVersion', 'getPipelineStateResponse_pipelineVersion' - The version number of the pipeline.
--
-- A newly created pipeline is always assigned a version number of @1@.
--
-- 'stageStates', 'getPipelineStateResponse_stageStates' - A list of the pipeline stage output information, including stage name,
-- state, most recent run details, whether the stage is disabled, and other
-- data.
--
-- 'updated', 'getPipelineStateResponse_updated' - The date and time the pipeline was last updated, in timestamp format.
--
-- 'httpStatus', 'getPipelineStateResponse_httpStatus' - The response's http status code.
newGetPipelineStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPipelineStateResponse
newGetPipelineStateResponse pHttpStatus_ =
  GetPipelineStateResponse'
    { created =
        Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      pipelineVersion = Prelude.Nothing,
      stageStates = Prelude.Nothing,
      updated = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time the pipeline was created, in timestamp format.
getPipelineStateResponse_created :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.UTCTime)
getPipelineStateResponse_created = Lens.lens (\GetPipelineStateResponse' {created} -> created) (\s@GetPipelineStateResponse' {} a -> s {created = a} :: GetPipelineStateResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the pipeline for which you want to get the state.
getPipelineStateResponse_pipelineName :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.Text)
getPipelineStateResponse_pipelineName = Lens.lens (\GetPipelineStateResponse' {pipelineName} -> pipelineName) (\s@GetPipelineStateResponse' {} a -> s {pipelineName = a} :: GetPipelineStateResponse)

-- | The version number of the pipeline.
--
-- A newly created pipeline is always assigned a version number of @1@.
getPipelineStateResponse_pipelineVersion :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.Natural)
getPipelineStateResponse_pipelineVersion = Lens.lens (\GetPipelineStateResponse' {pipelineVersion} -> pipelineVersion) (\s@GetPipelineStateResponse' {} a -> s {pipelineVersion = a} :: GetPipelineStateResponse)

-- | A list of the pipeline stage output information, including stage name,
-- state, most recent run details, whether the stage is disabled, and other
-- data.
getPipelineStateResponse_stageStates :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe [StageState])
getPipelineStateResponse_stageStates = Lens.lens (\GetPipelineStateResponse' {stageStates} -> stageStates) (\s@GetPipelineStateResponse' {} a -> s {stageStates = a} :: GetPipelineStateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the pipeline was last updated, in timestamp format.
getPipelineStateResponse_updated :: Lens.Lens' GetPipelineStateResponse (Prelude.Maybe Prelude.UTCTime)
getPipelineStateResponse_updated = Lens.lens (\GetPipelineStateResponse' {updated} -> updated) (\s@GetPipelineStateResponse' {} a -> s {updated = a} :: GetPipelineStateResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getPipelineStateResponse_httpStatus :: Lens.Lens' GetPipelineStateResponse Prelude.Int
getPipelineStateResponse_httpStatus = Lens.lens (\GetPipelineStateResponse' {httpStatus} -> httpStatus) (\s@GetPipelineStateResponse' {} a -> s {httpStatus = a} :: GetPipelineStateResponse)

instance Prelude.NFData GetPipelineStateResponse where
  rnf GetPipelineStateResponse' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf pipelineVersion
      `Prelude.seq` Prelude.rnf stageStates
      `Prelude.seq` Prelude.rnf updated
      `Prelude.seq` Prelude.rnf httpStatus

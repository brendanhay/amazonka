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
-- Module      : Network.AWS.CodePipeline.GetPipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata, structure, stages, and actions of a pipeline. Can
-- be used to return the entire structure of a pipeline in JSON format,
-- which can then be modified and used to update the pipeline structure
-- with UpdatePipeline.
module Network.AWS.CodePipeline.GetPipeline
  ( -- * Creating a Request
    GetPipeline (..),
    newGetPipeline,

    -- * Request Lenses
    getPipeline_version,
    getPipeline_name,

    -- * Destructuring the Response
    GetPipelineResponse (..),
    newGetPipelineResponse,

    -- * Response Lenses
    getPipelineResponse_metadata,
    getPipelineResponse_pipeline,
    getPipelineResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipeline@ action.
--
-- /See:/ 'newGetPipeline' smart constructor.
data GetPipeline = GetPipeline'
  { -- | The version number of the pipeline. If you do not specify a version,
    -- defaults to the current version.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The name of the pipeline for which you want to get information. Pipeline
    -- names must be unique under an AWS user account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getPipeline_version' - The version number of the pipeline. If you do not specify a version,
-- defaults to the current version.
--
-- 'name', 'getPipeline_name' - The name of the pipeline for which you want to get information. Pipeline
-- names must be unique under an AWS user account.
newGetPipeline ::
  -- | 'name'
  Prelude.Text ->
  GetPipeline
newGetPipeline pName_ =
  GetPipeline'
    { version = Prelude.Nothing,
      name = pName_
    }

-- | The version number of the pipeline. If you do not specify a version,
-- defaults to the current version.
getPipeline_version :: Lens.Lens' GetPipeline (Prelude.Maybe Prelude.Natural)
getPipeline_version = Lens.lens (\GetPipeline' {version} -> version) (\s@GetPipeline' {} a -> s {version = a} :: GetPipeline)

-- | The name of the pipeline for which you want to get information. Pipeline
-- names must be unique under an AWS user account.
getPipeline_name :: Lens.Lens' GetPipeline Prelude.Text
getPipeline_name = Lens.lens (\GetPipeline' {name} -> name) (\s@GetPipeline' {} a -> s {name = a} :: GetPipeline)

instance Core.AWSRequest GetPipeline where
  type AWSResponse GetPipeline = GetPipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineResponse'
            Prelude.<$> (x Core..?> "metadata")
            Prelude.<*> (x Core..?> "pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipeline

instance Prelude.NFData GetPipeline

instance Core.ToHeaders GetPipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetPipeline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPipeline where
  toJSON GetPipeline' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("version" Core..=) Prelude.<$> version,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath GetPipeline where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetPipeline@ action.
--
-- /See:/ 'newGetPipelineResponse' smart constructor.
data GetPipelineResponse = GetPipelineResponse'
  { -- | Represents the pipeline metadata information returned as part of the
    -- output of a @GetPipeline@ action.
    metadata :: Prelude.Maybe PipelineMetadata,
    -- | Represents the structure of actions and stages to be performed in the
    -- pipeline.
    pipeline :: Prelude.Maybe PipelineDeclaration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getPipelineResponse_metadata' - Represents the pipeline metadata information returned as part of the
-- output of a @GetPipeline@ action.
--
-- 'pipeline', 'getPipelineResponse_pipeline' - Represents the structure of actions and stages to be performed in the
-- pipeline.
--
-- 'httpStatus', 'getPipelineResponse_httpStatus' - The response's http status code.
newGetPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPipelineResponse
newGetPipelineResponse pHttpStatus_ =
  GetPipelineResponse'
    { metadata = Prelude.Nothing,
      pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the pipeline metadata information returned as part of the
-- output of a @GetPipeline@ action.
getPipelineResponse_metadata :: Lens.Lens' GetPipelineResponse (Prelude.Maybe PipelineMetadata)
getPipelineResponse_metadata = Lens.lens (\GetPipelineResponse' {metadata} -> metadata) (\s@GetPipelineResponse' {} a -> s {metadata = a} :: GetPipelineResponse)

-- | Represents the structure of actions and stages to be performed in the
-- pipeline.
getPipelineResponse_pipeline :: Lens.Lens' GetPipelineResponse (Prelude.Maybe PipelineDeclaration)
getPipelineResponse_pipeline = Lens.lens (\GetPipelineResponse' {pipeline} -> pipeline) (\s@GetPipelineResponse' {} a -> s {pipeline = a} :: GetPipelineResponse)

-- | The response's http status code.
getPipelineResponse_httpStatus :: Lens.Lens' GetPipelineResponse Prelude.Int
getPipelineResponse_httpStatus = Lens.lens (\GetPipelineResponse' {httpStatus} -> httpStatus) (\s@GetPipelineResponse' {} a -> s {httpStatus = a} :: GetPipelineResponse)

instance Prelude.NFData GetPipelineResponse

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
-- Module      : Amazonka.CodePipeline.GetPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata, structure, stages, and actions of a pipeline. Can
-- be used to return the entire structure of a pipeline in JSON format,
-- which can then be modified and used to update the pipeline structure
-- with UpdatePipeline.
module Amazonka.CodePipeline.GetPipeline
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

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineResponse'
            Prelude.<$> (x Data..?> "metadata")
            Prelude.<*> (x Data..?> "pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipeline where
  hashWithSalt _salt GetPipeline' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetPipeline where
  rnf GetPipeline' {..} =
    Prelude.rnf version `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetPipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.GetPipeline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPipeline where
  toJSON GetPipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("version" Data..=) Prelude.<$> version,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath GetPipeline where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPipeline where
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

instance Prelude.NFData GetPipelineResponse where
  rnf GetPipelineResponse' {..} =
    Prelude.rnf metadata `Prelude.seq`
      Prelude.rnf pipeline `Prelude.seq`
        Prelude.rnf httpStatus

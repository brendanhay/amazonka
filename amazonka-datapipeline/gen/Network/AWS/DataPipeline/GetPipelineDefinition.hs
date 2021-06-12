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
-- Module      : Network.AWS.DataPipeline.GetPipelineDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the definition of the specified pipeline. You can call
-- @GetPipelineDefinition@ to retrieve the pipeline definition that you
-- provided using PutPipelineDefinition.
module Network.AWS.DataPipeline.GetPipelineDefinition
  ( -- * Creating a Request
    GetPipelineDefinition (..),
    newGetPipelineDefinition,

    -- * Request Lenses
    getPipelineDefinition_version,
    getPipelineDefinition_pipelineId,

    -- * Destructuring the Response
    GetPipelineDefinitionResponse (..),
    newGetPipelineDefinitionResponse,

    -- * Response Lenses
    getPipelineDefinitionResponse_parameterValues,
    getPipelineDefinitionResponse_parameterObjects,
    getPipelineDefinitionResponse_pipelineObjects,
    getPipelineDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for GetPipelineDefinition.
--
-- /See:/ 'newGetPipelineDefinition' smart constructor.
data GetPipelineDefinition = GetPipelineDefinition'
  { -- | The version of the pipeline definition to retrieve. Set this parameter
    -- to @latest@ (default) to use the last definition saved to the pipeline
    -- or @active@ to use the last definition that was activated.
    version :: Core.Maybe Core.Text,
    -- | The ID of the pipeline.
    pipelineId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPipelineDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getPipelineDefinition_version' - The version of the pipeline definition to retrieve. Set this parameter
-- to @latest@ (default) to use the last definition saved to the pipeline
-- or @active@ to use the last definition that was activated.
--
-- 'pipelineId', 'getPipelineDefinition_pipelineId' - The ID of the pipeline.
newGetPipelineDefinition ::
  -- | 'pipelineId'
  Core.Text ->
  GetPipelineDefinition
newGetPipelineDefinition pPipelineId_ =
  GetPipelineDefinition'
    { version = Core.Nothing,
      pipelineId = pPipelineId_
    }

-- | The version of the pipeline definition to retrieve. Set this parameter
-- to @latest@ (default) to use the last definition saved to the pipeline
-- or @active@ to use the last definition that was activated.
getPipelineDefinition_version :: Lens.Lens' GetPipelineDefinition (Core.Maybe Core.Text)
getPipelineDefinition_version = Lens.lens (\GetPipelineDefinition' {version} -> version) (\s@GetPipelineDefinition' {} a -> s {version = a} :: GetPipelineDefinition)

-- | The ID of the pipeline.
getPipelineDefinition_pipelineId :: Lens.Lens' GetPipelineDefinition Core.Text
getPipelineDefinition_pipelineId = Lens.lens (\GetPipelineDefinition' {pipelineId} -> pipelineId) (\s@GetPipelineDefinition' {} a -> s {pipelineId = a} :: GetPipelineDefinition)

instance Core.AWSRequest GetPipelineDefinition where
  type
    AWSResponse GetPipelineDefinition =
      GetPipelineDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineDefinitionResponse'
            Core.<$> (x Core..?> "parameterValues" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "parameterObjects" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "pipelineObjects" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPipelineDefinition

instance Core.NFData GetPipelineDefinition

instance Core.ToHeaders GetPipelineDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.GetPipelineDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPipelineDefinition where
  toJSON GetPipelineDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("version" Core..=) Core.<$> version,
            Core.Just ("pipelineId" Core..= pipelineId)
          ]
      )

instance Core.ToPath GetPipelineDefinition where
  toPath = Core.const "/"

instance Core.ToQuery GetPipelineDefinition where
  toQuery = Core.const Core.mempty

-- | Contains the output of GetPipelineDefinition.
--
-- /See:/ 'newGetPipelineDefinitionResponse' smart constructor.
data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse'
  { -- | The parameter values used in the pipeline definition.
    parameterValues :: Core.Maybe [ParameterValue],
    -- | The parameter objects used in the pipeline definition.
    parameterObjects :: Core.Maybe [ParameterObject],
    -- | The objects defined in the pipeline.
    pipelineObjects :: Core.Maybe [PipelineObject],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPipelineDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValues', 'getPipelineDefinitionResponse_parameterValues' - The parameter values used in the pipeline definition.
--
-- 'parameterObjects', 'getPipelineDefinitionResponse_parameterObjects' - The parameter objects used in the pipeline definition.
--
-- 'pipelineObjects', 'getPipelineDefinitionResponse_pipelineObjects' - The objects defined in the pipeline.
--
-- 'httpStatus', 'getPipelineDefinitionResponse_httpStatus' - The response's http status code.
newGetPipelineDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPipelineDefinitionResponse
newGetPipelineDefinitionResponse pHttpStatus_ =
  GetPipelineDefinitionResponse'
    { parameterValues =
        Core.Nothing,
      parameterObjects = Core.Nothing,
      pipelineObjects = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The parameter values used in the pipeline definition.
getPipelineDefinitionResponse_parameterValues :: Lens.Lens' GetPipelineDefinitionResponse (Core.Maybe [ParameterValue])
getPipelineDefinitionResponse_parameterValues = Lens.lens (\GetPipelineDefinitionResponse' {parameterValues} -> parameterValues) (\s@GetPipelineDefinitionResponse' {} a -> s {parameterValues = a} :: GetPipelineDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The parameter objects used in the pipeline definition.
getPipelineDefinitionResponse_parameterObjects :: Lens.Lens' GetPipelineDefinitionResponse (Core.Maybe [ParameterObject])
getPipelineDefinitionResponse_parameterObjects = Lens.lens (\GetPipelineDefinitionResponse' {parameterObjects} -> parameterObjects) (\s@GetPipelineDefinitionResponse' {} a -> s {parameterObjects = a} :: GetPipelineDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The objects defined in the pipeline.
getPipelineDefinitionResponse_pipelineObjects :: Lens.Lens' GetPipelineDefinitionResponse (Core.Maybe [PipelineObject])
getPipelineDefinitionResponse_pipelineObjects = Lens.lens (\GetPipelineDefinitionResponse' {pipelineObjects} -> pipelineObjects) (\s@GetPipelineDefinitionResponse' {} a -> s {pipelineObjects = a} :: GetPipelineDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getPipelineDefinitionResponse_httpStatus :: Lens.Lens' GetPipelineDefinitionResponse Core.Int
getPipelineDefinitionResponse_httpStatus = Lens.lens (\GetPipelineDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetPipelineDefinitionResponse' {} a -> s {httpStatus = a} :: GetPipelineDefinitionResponse)

instance Core.NFData GetPipelineDefinitionResponse

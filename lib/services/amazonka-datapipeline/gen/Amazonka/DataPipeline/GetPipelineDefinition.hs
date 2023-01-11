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
-- Module      : Amazonka.DataPipeline.GetPipelineDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the definition of the specified pipeline. You can call
-- @GetPipelineDefinition@ to retrieve the pipeline definition that you
-- provided using PutPipelineDefinition.
module Amazonka.DataPipeline.GetPipelineDefinition
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
    getPipelineDefinitionResponse_parameterObjects,
    getPipelineDefinitionResponse_parameterValues,
    getPipelineDefinitionResponse_pipelineObjects,
    getPipelineDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for GetPipelineDefinition.
--
-- /See:/ 'newGetPipelineDefinition' smart constructor.
data GetPipelineDefinition = GetPipelineDefinition'
  { -- | The version of the pipeline definition to retrieve. Set this parameter
    -- to @latest@ (default) to use the last definition saved to the pipeline
    -- or @active@ to use the last definition that was activated.
    version :: Prelude.Maybe Prelude.Text,
    -- | The ID of the pipeline.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetPipelineDefinition
newGetPipelineDefinition pPipelineId_ =
  GetPipelineDefinition'
    { version = Prelude.Nothing,
      pipelineId = pPipelineId_
    }

-- | The version of the pipeline definition to retrieve. Set this parameter
-- to @latest@ (default) to use the last definition saved to the pipeline
-- or @active@ to use the last definition that was activated.
getPipelineDefinition_version :: Lens.Lens' GetPipelineDefinition (Prelude.Maybe Prelude.Text)
getPipelineDefinition_version = Lens.lens (\GetPipelineDefinition' {version} -> version) (\s@GetPipelineDefinition' {} a -> s {version = a} :: GetPipelineDefinition)

-- | The ID of the pipeline.
getPipelineDefinition_pipelineId :: Lens.Lens' GetPipelineDefinition Prelude.Text
getPipelineDefinition_pipelineId = Lens.lens (\GetPipelineDefinition' {pipelineId} -> pipelineId) (\s@GetPipelineDefinition' {} a -> s {pipelineId = a} :: GetPipelineDefinition)

instance Core.AWSRequest GetPipelineDefinition where
  type
    AWSResponse GetPipelineDefinition =
      GetPipelineDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineDefinitionResponse'
            Prelude.<$> ( x Data..?> "parameterObjects"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "parameterValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "pipelineObjects"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipelineDefinition where
  hashWithSalt _salt GetPipelineDefinition' {..} =
    _salt `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` pipelineId

instance Prelude.NFData GetPipelineDefinition where
  rnf GetPipelineDefinition' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf pipelineId

instance Data.ToHeaders GetPipelineDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DataPipeline.GetPipelineDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPipelineDefinition where
  toJSON GetPipelineDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("version" Data..=) Prelude.<$> version,
            Prelude.Just ("pipelineId" Data..= pipelineId)
          ]
      )

instance Data.ToPath GetPipelineDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPipelineDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of GetPipelineDefinition.
--
-- /See:/ 'newGetPipelineDefinitionResponse' smart constructor.
data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse'
  { -- | The parameter objects used in the pipeline definition.
    parameterObjects :: Prelude.Maybe [ParameterObject],
    -- | The parameter values used in the pipeline definition.
    parameterValues :: Prelude.Maybe [ParameterValue],
    -- | The objects defined in the pipeline.
    pipelineObjects :: Prelude.Maybe [PipelineObject],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipelineDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterObjects', 'getPipelineDefinitionResponse_parameterObjects' - The parameter objects used in the pipeline definition.
--
-- 'parameterValues', 'getPipelineDefinitionResponse_parameterValues' - The parameter values used in the pipeline definition.
--
-- 'pipelineObjects', 'getPipelineDefinitionResponse_pipelineObjects' - The objects defined in the pipeline.
--
-- 'httpStatus', 'getPipelineDefinitionResponse_httpStatus' - The response's http status code.
newGetPipelineDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPipelineDefinitionResponse
newGetPipelineDefinitionResponse pHttpStatus_ =
  GetPipelineDefinitionResponse'
    { parameterObjects =
        Prelude.Nothing,
      parameterValues = Prelude.Nothing,
      pipelineObjects = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The parameter objects used in the pipeline definition.
getPipelineDefinitionResponse_parameterObjects :: Lens.Lens' GetPipelineDefinitionResponse (Prelude.Maybe [ParameterObject])
getPipelineDefinitionResponse_parameterObjects = Lens.lens (\GetPipelineDefinitionResponse' {parameterObjects} -> parameterObjects) (\s@GetPipelineDefinitionResponse' {} a -> s {parameterObjects = a} :: GetPipelineDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The parameter values used in the pipeline definition.
getPipelineDefinitionResponse_parameterValues :: Lens.Lens' GetPipelineDefinitionResponse (Prelude.Maybe [ParameterValue])
getPipelineDefinitionResponse_parameterValues = Lens.lens (\GetPipelineDefinitionResponse' {parameterValues} -> parameterValues) (\s@GetPipelineDefinitionResponse' {} a -> s {parameterValues = a} :: GetPipelineDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The objects defined in the pipeline.
getPipelineDefinitionResponse_pipelineObjects :: Lens.Lens' GetPipelineDefinitionResponse (Prelude.Maybe [PipelineObject])
getPipelineDefinitionResponse_pipelineObjects = Lens.lens (\GetPipelineDefinitionResponse' {pipelineObjects} -> pipelineObjects) (\s@GetPipelineDefinitionResponse' {} a -> s {pipelineObjects = a} :: GetPipelineDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPipelineDefinitionResponse_httpStatus :: Lens.Lens' GetPipelineDefinitionResponse Prelude.Int
getPipelineDefinitionResponse_httpStatus = Lens.lens (\GetPipelineDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetPipelineDefinitionResponse' {} a -> s {httpStatus = a} :: GetPipelineDefinitionResponse)

instance Prelude.NFData GetPipelineDefinitionResponse where
  rnf GetPipelineDefinitionResponse' {..} =
    Prelude.rnf parameterObjects
      `Prelude.seq` Prelude.rnf parameterValues
      `Prelude.seq` Prelude.rnf pipelineObjects
      `Prelude.seq` Prelude.rnf httpStatus

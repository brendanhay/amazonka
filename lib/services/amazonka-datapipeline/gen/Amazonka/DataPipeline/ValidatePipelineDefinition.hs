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
-- Module      : Amazonka.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline definition to ensure that it is well
-- formed and can be run without error.
module Amazonka.DataPipeline.ValidatePipelineDefinition
  ( -- * Creating a Request
    ValidatePipelineDefinition (..),
    newValidatePipelineDefinition,

    -- * Request Lenses
    validatePipelineDefinition_parameterObjects,
    validatePipelineDefinition_parameterValues,
    validatePipelineDefinition_pipelineId,
    validatePipelineDefinition_pipelineObjects,

    -- * Destructuring the Response
    ValidatePipelineDefinitionResponse (..),
    newValidatePipelineDefinitionResponse,

    -- * Response Lenses
    validatePipelineDefinitionResponse_validationErrors,
    validatePipelineDefinitionResponse_validationWarnings,
    validatePipelineDefinitionResponse_httpStatus,
    validatePipelineDefinitionResponse_errored,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ValidatePipelineDefinition.
--
-- /See:/ 'newValidatePipelineDefinition' smart constructor.
data ValidatePipelineDefinition = ValidatePipelineDefinition'
  { -- | The parameter objects used with the pipeline.
    parameterObjects :: Prelude.Maybe [ParameterObject],
    -- | The parameter values used with the pipeline.
    parameterValues :: Prelude.Maybe [ParameterValue],
    -- | The ID of the pipeline.
    pipelineId :: Prelude.Text,
    -- | The objects that define the pipeline changes to validate against the
    -- pipeline.
    pipelineObjects :: [PipelineObject]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidatePipelineDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterObjects', 'validatePipelineDefinition_parameterObjects' - The parameter objects used with the pipeline.
--
-- 'parameterValues', 'validatePipelineDefinition_parameterValues' - The parameter values used with the pipeline.
--
-- 'pipelineId', 'validatePipelineDefinition_pipelineId' - The ID of the pipeline.
--
-- 'pipelineObjects', 'validatePipelineDefinition_pipelineObjects' - The objects that define the pipeline changes to validate against the
-- pipeline.
newValidatePipelineDefinition ::
  -- | 'pipelineId'
  Prelude.Text ->
  ValidatePipelineDefinition
newValidatePipelineDefinition pPipelineId_ =
  ValidatePipelineDefinition'
    { parameterObjects =
        Prelude.Nothing,
      parameterValues = Prelude.Nothing,
      pipelineId = pPipelineId_,
      pipelineObjects = Prelude.mempty
    }

-- | The parameter objects used with the pipeline.
validatePipelineDefinition_parameterObjects :: Lens.Lens' ValidatePipelineDefinition (Prelude.Maybe [ParameterObject])
validatePipelineDefinition_parameterObjects = Lens.lens (\ValidatePipelineDefinition' {parameterObjects} -> parameterObjects) (\s@ValidatePipelineDefinition' {} a -> s {parameterObjects = a} :: ValidatePipelineDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The parameter values used with the pipeline.
validatePipelineDefinition_parameterValues :: Lens.Lens' ValidatePipelineDefinition (Prelude.Maybe [ParameterValue])
validatePipelineDefinition_parameterValues = Lens.lens (\ValidatePipelineDefinition' {parameterValues} -> parameterValues) (\s@ValidatePipelineDefinition' {} a -> s {parameterValues = a} :: ValidatePipelineDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the pipeline.
validatePipelineDefinition_pipelineId :: Lens.Lens' ValidatePipelineDefinition Prelude.Text
validatePipelineDefinition_pipelineId = Lens.lens (\ValidatePipelineDefinition' {pipelineId} -> pipelineId) (\s@ValidatePipelineDefinition' {} a -> s {pipelineId = a} :: ValidatePipelineDefinition)

-- | The objects that define the pipeline changes to validate against the
-- pipeline.
validatePipelineDefinition_pipelineObjects :: Lens.Lens' ValidatePipelineDefinition [PipelineObject]
validatePipelineDefinition_pipelineObjects = Lens.lens (\ValidatePipelineDefinition' {pipelineObjects} -> pipelineObjects) (\s@ValidatePipelineDefinition' {} a -> s {pipelineObjects = a} :: ValidatePipelineDefinition) Prelude.. Lens.coerced

instance Core.AWSRequest ValidatePipelineDefinition where
  type
    AWSResponse ValidatePipelineDefinition =
      ValidatePipelineDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidatePipelineDefinitionResponse'
            Prelude.<$> ( x Data..?> "validationErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "validationWarnings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "errored")
      )

instance Prelude.Hashable ValidatePipelineDefinition where
  hashWithSalt _salt ValidatePipelineDefinition' {..} =
    _salt `Prelude.hashWithSalt` parameterObjects
      `Prelude.hashWithSalt` parameterValues
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` pipelineObjects

instance Prelude.NFData ValidatePipelineDefinition where
  rnf ValidatePipelineDefinition' {..} =
    Prelude.rnf parameterObjects
      `Prelude.seq` Prelude.rnf parameterValues
      `Prelude.seq` Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf pipelineObjects

instance Data.ToHeaders ValidatePipelineDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DataPipeline.ValidatePipelineDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ValidatePipelineDefinition where
  toJSON ValidatePipelineDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parameterObjects" Data..=)
              Prelude.<$> parameterObjects,
            ("parameterValues" Data..=)
              Prelude.<$> parameterValues,
            Prelude.Just ("pipelineId" Data..= pipelineId),
            Prelude.Just
              ("pipelineObjects" Data..= pipelineObjects)
          ]
      )

instance Data.ToPath ValidatePipelineDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery ValidatePipelineDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of ValidatePipelineDefinition.
--
-- /See:/ 'newValidatePipelineDefinitionResponse' smart constructor.
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse'
  { -- | Any validation errors that were found.
    validationErrors :: Prelude.Maybe [ValidationError],
    -- | Any validation warnings that were found.
    validationWarnings :: Prelude.Maybe [ValidationWarning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Indicates whether there were validation errors.
    errored :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidatePipelineDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationErrors', 'validatePipelineDefinitionResponse_validationErrors' - Any validation errors that were found.
--
-- 'validationWarnings', 'validatePipelineDefinitionResponse_validationWarnings' - Any validation warnings that were found.
--
-- 'httpStatus', 'validatePipelineDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'errored', 'validatePipelineDefinitionResponse_errored' - Indicates whether there were validation errors.
newValidatePipelineDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'errored'
  Prelude.Bool ->
  ValidatePipelineDefinitionResponse
newValidatePipelineDefinitionResponse
  pHttpStatus_
  pErrored_ =
    ValidatePipelineDefinitionResponse'
      { validationErrors =
          Prelude.Nothing,
        validationWarnings = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        errored = pErrored_
      }

-- | Any validation errors that were found.
validatePipelineDefinitionResponse_validationErrors :: Lens.Lens' ValidatePipelineDefinitionResponse (Prelude.Maybe [ValidationError])
validatePipelineDefinitionResponse_validationErrors = Lens.lens (\ValidatePipelineDefinitionResponse' {validationErrors} -> validationErrors) (\s@ValidatePipelineDefinitionResponse' {} a -> s {validationErrors = a} :: ValidatePipelineDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Any validation warnings that were found.
validatePipelineDefinitionResponse_validationWarnings :: Lens.Lens' ValidatePipelineDefinitionResponse (Prelude.Maybe [ValidationWarning])
validatePipelineDefinitionResponse_validationWarnings = Lens.lens (\ValidatePipelineDefinitionResponse' {validationWarnings} -> validationWarnings) (\s@ValidatePipelineDefinitionResponse' {} a -> s {validationWarnings = a} :: ValidatePipelineDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
validatePipelineDefinitionResponse_httpStatus :: Lens.Lens' ValidatePipelineDefinitionResponse Prelude.Int
validatePipelineDefinitionResponse_httpStatus = Lens.lens (\ValidatePipelineDefinitionResponse' {httpStatus} -> httpStatus) (\s@ValidatePipelineDefinitionResponse' {} a -> s {httpStatus = a} :: ValidatePipelineDefinitionResponse)

-- | Indicates whether there were validation errors.
validatePipelineDefinitionResponse_errored :: Lens.Lens' ValidatePipelineDefinitionResponse Prelude.Bool
validatePipelineDefinitionResponse_errored = Lens.lens (\ValidatePipelineDefinitionResponse' {errored} -> errored) (\s@ValidatePipelineDefinitionResponse' {} a -> s {errored = a} :: ValidatePipelineDefinitionResponse)

instance
  Prelude.NFData
    ValidatePipelineDefinitionResponse
  where
  rnf ValidatePipelineDefinitionResponse' {..} =
    Prelude.rnf validationErrors
      `Prelude.seq` Prelude.rnf validationWarnings
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errored

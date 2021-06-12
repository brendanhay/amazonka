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
-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline definition to ensure that it is well
-- formed and can be run without error.
module Network.AWS.DataPipeline.ValidatePipelineDefinition
  ( -- * Creating a Request
    ValidatePipelineDefinition (..),
    newValidatePipelineDefinition,

    -- * Request Lenses
    validatePipelineDefinition_parameterValues,
    validatePipelineDefinition_parameterObjects,
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

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ValidatePipelineDefinition.
--
-- /See:/ 'newValidatePipelineDefinition' smart constructor.
data ValidatePipelineDefinition = ValidatePipelineDefinition'
  { -- | The parameter values used with the pipeline.
    parameterValues :: Core.Maybe [ParameterValue],
    -- | The parameter objects used with the pipeline.
    parameterObjects :: Core.Maybe [ParameterObject],
    -- | The ID of the pipeline.
    pipelineId :: Core.Text,
    -- | The objects that define the pipeline changes to validate against the
    -- pipeline.
    pipelineObjects :: [PipelineObject]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidatePipelineDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValues', 'validatePipelineDefinition_parameterValues' - The parameter values used with the pipeline.
--
-- 'parameterObjects', 'validatePipelineDefinition_parameterObjects' - The parameter objects used with the pipeline.
--
-- 'pipelineId', 'validatePipelineDefinition_pipelineId' - The ID of the pipeline.
--
-- 'pipelineObjects', 'validatePipelineDefinition_pipelineObjects' - The objects that define the pipeline changes to validate against the
-- pipeline.
newValidatePipelineDefinition ::
  -- | 'pipelineId'
  Core.Text ->
  ValidatePipelineDefinition
newValidatePipelineDefinition pPipelineId_ =
  ValidatePipelineDefinition'
    { parameterValues =
        Core.Nothing,
      parameterObjects = Core.Nothing,
      pipelineId = pPipelineId_,
      pipelineObjects = Core.mempty
    }

-- | The parameter values used with the pipeline.
validatePipelineDefinition_parameterValues :: Lens.Lens' ValidatePipelineDefinition (Core.Maybe [ParameterValue])
validatePipelineDefinition_parameterValues = Lens.lens (\ValidatePipelineDefinition' {parameterValues} -> parameterValues) (\s@ValidatePipelineDefinition' {} a -> s {parameterValues = a} :: ValidatePipelineDefinition) Core.. Lens.mapping Lens._Coerce

-- | The parameter objects used with the pipeline.
validatePipelineDefinition_parameterObjects :: Lens.Lens' ValidatePipelineDefinition (Core.Maybe [ParameterObject])
validatePipelineDefinition_parameterObjects = Lens.lens (\ValidatePipelineDefinition' {parameterObjects} -> parameterObjects) (\s@ValidatePipelineDefinition' {} a -> s {parameterObjects = a} :: ValidatePipelineDefinition) Core.. Lens.mapping Lens._Coerce

-- | The ID of the pipeline.
validatePipelineDefinition_pipelineId :: Lens.Lens' ValidatePipelineDefinition Core.Text
validatePipelineDefinition_pipelineId = Lens.lens (\ValidatePipelineDefinition' {pipelineId} -> pipelineId) (\s@ValidatePipelineDefinition' {} a -> s {pipelineId = a} :: ValidatePipelineDefinition)

-- | The objects that define the pipeline changes to validate against the
-- pipeline.
validatePipelineDefinition_pipelineObjects :: Lens.Lens' ValidatePipelineDefinition [PipelineObject]
validatePipelineDefinition_pipelineObjects = Lens.lens (\ValidatePipelineDefinition' {pipelineObjects} -> pipelineObjects) (\s@ValidatePipelineDefinition' {} a -> s {pipelineObjects = a} :: ValidatePipelineDefinition) Core.. Lens._Coerce

instance Core.AWSRequest ValidatePipelineDefinition where
  type
    AWSResponse ValidatePipelineDefinition =
      ValidatePipelineDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidatePipelineDefinitionResponse'
            Core.<$> (x Core..?> "validationErrors" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "validationWarnings"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "errored")
      )

instance Core.Hashable ValidatePipelineDefinition

instance Core.NFData ValidatePipelineDefinition

instance Core.ToHeaders ValidatePipelineDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.ValidatePipelineDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ValidatePipelineDefinition where
  toJSON ValidatePipelineDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("parameterValues" Core..=)
              Core.<$> parameterValues,
            ("parameterObjects" Core..=)
              Core.<$> parameterObjects,
            Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just
              ("pipelineObjects" Core..= pipelineObjects)
          ]
      )

instance Core.ToPath ValidatePipelineDefinition where
  toPath = Core.const "/"

instance Core.ToQuery ValidatePipelineDefinition where
  toQuery = Core.const Core.mempty

-- | Contains the output of ValidatePipelineDefinition.
--
-- /See:/ 'newValidatePipelineDefinitionResponse' smart constructor.
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse'
  { -- | Any validation errors that were found.
    validationErrors :: Core.Maybe [ValidationError],
    -- | Any validation warnings that were found.
    validationWarnings :: Core.Maybe [ValidationWarning],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Indicates whether there were validation errors.
    errored :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'errored'
  Core.Bool ->
  ValidatePipelineDefinitionResponse
newValidatePipelineDefinitionResponse
  pHttpStatus_
  pErrored_ =
    ValidatePipelineDefinitionResponse'
      { validationErrors =
          Core.Nothing,
        validationWarnings = Core.Nothing,
        httpStatus = pHttpStatus_,
        errored = pErrored_
      }

-- | Any validation errors that were found.
validatePipelineDefinitionResponse_validationErrors :: Lens.Lens' ValidatePipelineDefinitionResponse (Core.Maybe [ValidationError])
validatePipelineDefinitionResponse_validationErrors = Lens.lens (\ValidatePipelineDefinitionResponse' {validationErrors} -> validationErrors) (\s@ValidatePipelineDefinitionResponse' {} a -> s {validationErrors = a} :: ValidatePipelineDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | Any validation warnings that were found.
validatePipelineDefinitionResponse_validationWarnings :: Lens.Lens' ValidatePipelineDefinitionResponse (Core.Maybe [ValidationWarning])
validatePipelineDefinitionResponse_validationWarnings = Lens.lens (\ValidatePipelineDefinitionResponse' {validationWarnings} -> validationWarnings) (\s@ValidatePipelineDefinitionResponse' {} a -> s {validationWarnings = a} :: ValidatePipelineDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
validatePipelineDefinitionResponse_httpStatus :: Lens.Lens' ValidatePipelineDefinitionResponse Core.Int
validatePipelineDefinitionResponse_httpStatus = Lens.lens (\ValidatePipelineDefinitionResponse' {httpStatus} -> httpStatus) (\s@ValidatePipelineDefinitionResponse' {} a -> s {httpStatus = a} :: ValidatePipelineDefinitionResponse)

-- | Indicates whether there were validation errors.
validatePipelineDefinitionResponse_errored :: Lens.Lens' ValidatePipelineDefinitionResponse Core.Bool
validatePipelineDefinitionResponse_errored = Lens.lens (\ValidatePipelineDefinitionResponse' {errored} -> errored) (\s@ValidatePipelineDefinitionResponse' {} a -> s {errored = a} :: ValidatePipelineDefinitionResponse)

instance
  Core.NFData
    ValidatePipelineDefinitionResponse

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
-- Module      : Network.AWS.DataPipeline.PutPipelineDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tasks, schedules, and preconditions to the specified pipeline. You
-- can use @PutPipelineDefinition@ to populate a new pipeline.
--
-- @PutPipelineDefinition@ also validates the configuration as it adds it
-- to the pipeline. Changes to the pipeline are saved unless one of the
-- following three validation errors exists in the pipeline.
--
-- 1.  An object is missing a name or identifier field.
-- 2.  A string or reference field is empty.
-- 3.  The number of objects in the pipeline exceeds the maximum allowed
--     objects.
-- 4.  The pipeline is in a FINISHED state.
--
-- Pipeline object definitions are passed to the @PutPipelineDefinition@
-- action and returned by the GetPipelineDefinition action.
module Network.AWS.DataPipeline.PutPipelineDefinition
  ( -- * Creating a Request
    PutPipelineDefinition (..),
    newPutPipelineDefinition,

    -- * Request Lenses
    putPipelineDefinition_parameterValues,
    putPipelineDefinition_parameterObjects,
    putPipelineDefinition_pipelineId,
    putPipelineDefinition_pipelineObjects,

    -- * Destructuring the Response
    PutPipelineDefinitionResponse (..),
    newPutPipelineDefinitionResponse,

    -- * Response Lenses
    putPipelineDefinitionResponse_validationErrors,
    putPipelineDefinitionResponse_validationWarnings,
    putPipelineDefinitionResponse_httpStatus,
    putPipelineDefinitionResponse_errored,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for PutPipelineDefinition.
--
-- /See:/ 'newPutPipelineDefinition' smart constructor.
data PutPipelineDefinition = PutPipelineDefinition'
  { -- | The parameter values used with the pipeline.
    parameterValues :: Prelude.Maybe [ParameterValue],
    -- | The parameter objects used with the pipeline.
    parameterObjects :: Prelude.Maybe [ParameterObject],
    -- | The ID of the pipeline.
    pipelineId :: Prelude.Text,
    -- | The objects that define the pipeline. These objects overwrite the
    -- existing pipeline definition.
    pipelineObjects :: [PipelineObject]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPipelineDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValues', 'putPipelineDefinition_parameterValues' - The parameter values used with the pipeline.
--
-- 'parameterObjects', 'putPipelineDefinition_parameterObjects' - The parameter objects used with the pipeline.
--
-- 'pipelineId', 'putPipelineDefinition_pipelineId' - The ID of the pipeline.
--
-- 'pipelineObjects', 'putPipelineDefinition_pipelineObjects' - The objects that define the pipeline. These objects overwrite the
-- existing pipeline definition.
newPutPipelineDefinition ::
  -- | 'pipelineId'
  Prelude.Text ->
  PutPipelineDefinition
newPutPipelineDefinition pPipelineId_ =
  PutPipelineDefinition'
    { parameterValues =
        Prelude.Nothing,
      parameterObjects = Prelude.Nothing,
      pipelineId = pPipelineId_,
      pipelineObjects = Prelude.mempty
    }

-- | The parameter values used with the pipeline.
putPipelineDefinition_parameterValues :: Lens.Lens' PutPipelineDefinition (Prelude.Maybe [ParameterValue])
putPipelineDefinition_parameterValues = Lens.lens (\PutPipelineDefinition' {parameterValues} -> parameterValues) (\s@PutPipelineDefinition' {} a -> s {parameterValues = a} :: PutPipelineDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The parameter objects used with the pipeline.
putPipelineDefinition_parameterObjects :: Lens.Lens' PutPipelineDefinition (Prelude.Maybe [ParameterObject])
putPipelineDefinition_parameterObjects = Lens.lens (\PutPipelineDefinition' {parameterObjects} -> parameterObjects) (\s@PutPipelineDefinition' {} a -> s {parameterObjects = a} :: PutPipelineDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the pipeline.
putPipelineDefinition_pipelineId :: Lens.Lens' PutPipelineDefinition Prelude.Text
putPipelineDefinition_pipelineId = Lens.lens (\PutPipelineDefinition' {pipelineId} -> pipelineId) (\s@PutPipelineDefinition' {} a -> s {pipelineId = a} :: PutPipelineDefinition)

-- | The objects that define the pipeline. These objects overwrite the
-- existing pipeline definition.
putPipelineDefinition_pipelineObjects :: Lens.Lens' PutPipelineDefinition [PipelineObject]
putPipelineDefinition_pipelineObjects = Lens.lens (\PutPipelineDefinition' {pipelineObjects} -> pipelineObjects) (\s@PutPipelineDefinition' {} a -> s {pipelineObjects = a} :: PutPipelineDefinition) Prelude.. Lens._Coerce

instance Core.AWSRequest PutPipelineDefinition where
  type
    AWSResponse PutPipelineDefinition =
      PutPipelineDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPipelineDefinitionResponse'
            Prelude.<$> ( x Core..?> "validationErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "validationWarnings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "errored")
      )

instance Prelude.Hashable PutPipelineDefinition

instance Prelude.NFData PutPipelineDefinition

instance Core.ToHeaders PutPipelineDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.PutPipelineDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutPipelineDefinition where
  toJSON PutPipelineDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("parameterValues" Core..=)
              Prelude.<$> parameterValues,
            ("parameterObjects" Core..=)
              Prelude.<$> parameterObjects,
            Prelude.Just ("pipelineId" Core..= pipelineId),
            Prelude.Just
              ("pipelineObjects" Core..= pipelineObjects)
          ]
      )

instance Core.ToPath PutPipelineDefinition where
  toPath = Prelude.const "/"

instance Core.ToQuery PutPipelineDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of PutPipelineDefinition.
--
-- /See:/ 'newPutPipelineDefinitionResponse' smart constructor.
data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse'
  { -- | The validation errors that are associated with the objects defined in
    -- @pipelineObjects@.
    validationErrors :: Prelude.Maybe [ValidationError],
    -- | The validation warnings that are associated with the objects defined in
    -- @pipelineObjects@.
    validationWarnings :: Prelude.Maybe [ValidationWarning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Indicates whether there were validation errors, and the pipeline
    -- definition is stored but cannot be activated until you correct the
    -- pipeline and call @PutPipelineDefinition@ to commit the corrected
    -- pipeline.
    errored :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPipelineDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationErrors', 'putPipelineDefinitionResponse_validationErrors' - The validation errors that are associated with the objects defined in
-- @pipelineObjects@.
--
-- 'validationWarnings', 'putPipelineDefinitionResponse_validationWarnings' - The validation warnings that are associated with the objects defined in
-- @pipelineObjects@.
--
-- 'httpStatus', 'putPipelineDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'errored', 'putPipelineDefinitionResponse_errored' - Indicates whether there were validation errors, and the pipeline
-- definition is stored but cannot be activated until you correct the
-- pipeline and call @PutPipelineDefinition@ to commit the corrected
-- pipeline.
newPutPipelineDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'errored'
  Prelude.Bool ->
  PutPipelineDefinitionResponse
newPutPipelineDefinitionResponse
  pHttpStatus_
  pErrored_ =
    PutPipelineDefinitionResponse'
      { validationErrors =
          Prelude.Nothing,
        validationWarnings = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        errored = pErrored_
      }

-- | The validation errors that are associated with the objects defined in
-- @pipelineObjects@.
putPipelineDefinitionResponse_validationErrors :: Lens.Lens' PutPipelineDefinitionResponse (Prelude.Maybe [ValidationError])
putPipelineDefinitionResponse_validationErrors = Lens.lens (\PutPipelineDefinitionResponse' {validationErrors} -> validationErrors) (\s@PutPipelineDefinitionResponse' {} a -> s {validationErrors = a} :: PutPipelineDefinitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The validation warnings that are associated with the objects defined in
-- @pipelineObjects@.
putPipelineDefinitionResponse_validationWarnings :: Lens.Lens' PutPipelineDefinitionResponse (Prelude.Maybe [ValidationWarning])
putPipelineDefinitionResponse_validationWarnings = Lens.lens (\PutPipelineDefinitionResponse' {validationWarnings} -> validationWarnings) (\s@PutPipelineDefinitionResponse' {} a -> s {validationWarnings = a} :: PutPipelineDefinitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putPipelineDefinitionResponse_httpStatus :: Lens.Lens' PutPipelineDefinitionResponse Prelude.Int
putPipelineDefinitionResponse_httpStatus = Lens.lens (\PutPipelineDefinitionResponse' {httpStatus} -> httpStatus) (\s@PutPipelineDefinitionResponse' {} a -> s {httpStatus = a} :: PutPipelineDefinitionResponse)

-- | Indicates whether there were validation errors, and the pipeline
-- definition is stored but cannot be activated until you correct the
-- pipeline and call @PutPipelineDefinition@ to commit the corrected
-- pipeline.
putPipelineDefinitionResponse_errored :: Lens.Lens' PutPipelineDefinitionResponse Prelude.Bool
putPipelineDefinitionResponse_errored = Lens.lens (\PutPipelineDefinitionResponse' {errored} -> errored) (\s@PutPipelineDefinitionResponse' {} a -> s {errored = a} :: PutPipelineDefinitionResponse)

instance Prelude.NFData PutPipelineDefinitionResponse

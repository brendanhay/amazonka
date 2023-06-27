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
-- Module      : Amazonka.OsIs.ValidatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks whether an OpenSearch Ingestion pipeline configuration is valid
-- prior to creation. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/creating-pipeline.html Creating Amazon OpenSearch Ingestion pipelines>.
module Amazonka.OsIs.ValidatePipeline
  ( -- * Creating a Request
    ValidatePipeline (..),
    newValidatePipeline,

    -- * Request Lenses
    validatePipeline_pipelineConfigurationBody,

    -- * Destructuring the Response
    ValidatePipelineResponse (..),
    newValidatePipelineResponse,

    -- * Response Lenses
    validatePipelineResponse_errors,
    validatePipelineResponse_isValid,
    validatePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidatePipeline' smart constructor.
data ValidatePipeline = ValidatePipeline'
  { -- | The pipeline configuration in YAML format. The command accepts the
    -- pipeline configuration as a string or within a .yaml file. If you
    -- provide the configuration as a string, each new line must be escaped
    -- with @\\n@.
    pipelineConfigurationBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineConfigurationBody', 'validatePipeline_pipelineConfigurationBody' - The pipeline configuration in YAML format. The command accepts the
-- pipeline configuration as a string or within a .yaml file. If you
-- provide the configuration as a string, each new line must be escaped
-- with @\\n@.
newValidatePipeline ::
  -- | 'pipelineConfigurationBody'
  Prelude.Text ->
  ValidatePipeline
newValidatePipeline pPipelineConfigurationBody_ =
  ValidatePipeline'
    { pipelineConfigurationBody =
        pPipelineConfigurationBody_
    }

-- | The pipeline configuration in YAML format. The command accepts the
-- pipeline configuration as a string or within a .yaml file. If you
-- provide the configuration as a string, each new line must be escaped
-- with @\\n@.
validatePipeline_pipelineConfigurationBody :: Lens.Lens' ValidatePipeline Prelude.Text
validatePipeline_pipelineConfigurationBody = Lens.lens (\ValidatePipeline' {pipelineConfigurationBody} -> pipelineConfigurationBody) (\s@ValidatePipeline' {} a -> s {pipelineConfigurationBody = a} :: ValidatePipeline)

instance Core.AWSRequest ValidatePipeline where
  type
    AWSResponse ValidatePipeline =
      ValidatePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidatePipelineResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "isValid")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ValidatePipeline where
  hashWithSalt _salt ValidatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineConfigurationBody

instance Prelude.NFData ValidatePipeline where
  rnf ValidatePipeline' {..} =
    Prelude.rnf pipelineConfigurationBody

instance Data.ToHeaders ValidatePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ValidatePipeline where
  toJSON ValidatePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PipelineConfigurationBody"
                  Data..= pipelineConfigurationBody
              )
          ]
      )

instance Data.ToPath ValidatePipeline where
  toPath =
    Prelude.const "/2022-01-01/osis/validatePipeline"

instance Data.ToQuery ValidatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidatePipelineResponse' smart constructor.
data ValidatePipelineResponse = ValidatePipelineResponse'
  { -- | A list of errors if the configuration is invalid.
    errors :: Prelude.Maybe [ValidationMessage],
    -- | A boolean indicating whether or not the pipeline configuration is valid.
    isValid :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'validatePipelineResponse_errors' - A list of errors if the configuration is invalid.
--
-- 'isValid', 'validatePipelineResponse_isValid' - A boolean indicating whether or not the pipeline configuration is valid.
--
-- 'httpStatus', 'validatePipelineResponse_httpStatus' - The response's http status code.
newValidatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidatePipelineResponse
newValidatePipelineResponse pHttpStatus_ =
  ValidatePipelineResponse'
    { errors = Prelude.Nothing,
      isValid = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors if the configuration is invalid.
validatePipelineResponse_errors :: Lens.Lens' ValidatePipelineResponse (Prelude.Maybe [ValidationMessage])
validatePipelineResponse_errors = Lens.lens (\ValidatePipelineResponse' {errors} -> errors) (\s@ValidatePipelineResponse' {} a -> s {errors = a} :: ValidatePipelineResponse) Prelude.. Lens.mapping Lens.coerced

-- | A boolean indicating whether or not the pipeline configuration is valid.
validatePipelineResponse_isValid :: Lens.Lens' ValidatePipelineResponse (Prelude.Maybe Prelude.Bool)
validatePipelineResponse_isValid = Lens.lens (\ValidatePipelineResponse' {isValid} -> isValid) (\s@ValidatePipelineResponse' {} a -> s {isValid = a} :: ValidatePipelineResponse)

-- | The response's http status code.
validatePipelineResponse_httpStatus :: Lens.Lens' ValidatePipelineResponse Prelude.Int
validatePipelineResponse_httpStatus = Lens.lens (\ValidatePipelineResponse' {httpStatus} -> httpStatus) (\s@ValidatePipelineResponse' {} a -> s {httpStatus = a} :: ValidatePipelineResponse)

instance Prelude.NFData ValidatePipelineResponse where
  rnf ValidatePipelineResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf isValid
      `Prelude.seq` Prelude.rnf httpStatus

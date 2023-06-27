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
-- Module      : Amazonka.OsIs.UpdatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an OpenSearch Ingestion pipeline. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/update-pipeline.html Updating Amazon OpenSearch Ingestion pipelines>.
module Amazonka.OsIs.UpdatePipeline
  ( -- * Creating a Request
    UpdatePipeline (..),
    newUpdatePipeline,

    -- * Request Lenses
    updatePipeline_logPublishingOptions,
    updatePipeline_maxUnits,
    updatePipeline_minUnits,
    updatePipeline_pipelineConfigurationBody,
    updatePipeline_pipelineName,

    -- * Destructuring the Response
    UpdatePipelineResponse (..),
    newUpdatePipelineResponse,

    -- * Response Lenses
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | Key-value pairs to configure log publishing.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptions,
    -- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs)
    maxUnits :: Prelude.Maybe Prelude.Natural,
    -- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
    minUnits :: Prelude.Maybe Prelude.Natural,
    -- | The pipeline configuration in YAML format. The command accepts the
    -- pipeline configuration as a string or within a .yaml file. If you
    -- provide the configuration as a string, each new line must be escaped
    -- with @\\n@.
    pipelineConfigurationBody :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline to update.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPublishingOptions', 'updatePipeline_logPublishingOptions' - Key-value pairs to configure log publishing.
--
-- 'maxUnits', 'updatePipeline_maxUnits' - The maximum pipeline capacity, in Ingestion Compute Units (ICUs)
--
-- 'minUnits', 'updatePipeline_minUnits' - The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
--
-- 'pipelineConfigurationBody', 'updatePipeline_pipelineConfigurationBody' - The pipeline configuration in YAML format. The command accepts the
-- pipeline configuration as a string or within a .yaml file. If you
-- provide the configuration as a string, each new line must be escaped
-- with @\\n@.
--
-- 'pipelineName', 'updatePipeline_pipelineName' - The name of the pipeline to update.
newUpdatePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  UpdatePipeline
newUpdatePipeline pPipelineName_ =
  UpdatePipeline'
    { logPublishingOptions =
        Prelude.Nothing,
      maxUnits = Prelude.Nothing,
      minUnits = Prelude.Nothing,
      pipelineConfigurationBody = Prelude.Nothing,
      pipelineName = pPipelineName_
    }

-- | Key-value pairs to configure log publishing.
updatePipeline_logPublishingOptions :: Lens.Lens' UpdatePipeline (Prelude.Maybe LogPublishingOptions)
updatePipeline_logPublishingOptions = Lens.lens (\UpdatePipeline' {logPublishingOptions} -> logPublishingOptions) (\s@UpdatePipeline' {} a -> s {logPublishingOptions = a} :: UpdatePipeline)

-- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs)
updatePipeline_maxUnits :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Natural)
updatePipeline_maxUnits = Lens.lens (\UpdatePipeline' {maxUnits} -> maxUnits) (\s@UpdatePipeline' {} a -> s {maxUnits = a} :: UpdatePipeline)

-- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
updatePipeline_minUnits :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Natural)
updatePipeline_minUnits = Lens.lens (\UpdatePipeline' {minUnits} -> minUnits) (\s@UpdatePipeline' {} a -> s {minUnits = a} :: UpdatePipeline)

-- | The pipeline configuration in YAML format. The command accepts the
-- pipeline configuration as a string or within a .yaml file. If you
-- provide the configuration as a string, each new line must be escaped
-- with @\\n@.
updatePipeline_pipelineConfigurationBody :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_pipelineConfigurationBody = Lens.lens (\UpdatePipeline' {pipelineConfigurationBody} -> pipelineConfigurationBody) (\s@UpdatePipeline' {} a -> s {pipelineConfigurationBody = a} :: UpdatePipeline)

-- | The name of the pipeline to update.
updatePipeline_pipelineName :: Lens.Lens' UpdatePipeline Prelude.Text
updatePipeline_pipelineName = Lens.lens (\UpdatePipeline' {pipelineName} -> pipelineName) (\s@UpdatePipeline' {} a -> s {pipelineName = a} :: UpdatePipeline)

instance Core.AWSRequest UpdatePipeline where
  type
    AWSResponse UpdatePipeline =
      UpdatePipelineResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipeline where
  hashWithSalt _salt UpdatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` maxUnits
      `Prelude.hashWithSalt` minUnits
      `Prelude.hashWithSalt` pipelineConfigurationBody
      `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData UpdatePipeline where
  rnf UpdatePipeline' {..} =
    Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf maxUnits
      `Prelude.seq` Prelude.rnf minUnits
      `Prelude.seq` Prelude.rnf pipelineConfigurationBody
      `Prelude.seq` Prelude.rnf pipelineName

instance Data.ToHeaders UpdatePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogPublishingOptions" Data..=)
              Prelude.<$> logPublishingOptions,
            ("MaxUnits" Data..=) Prelude.<$> maxUnits,
            ("MinUnits" Data..=) Prelude.<$> minUnits,
            ("PipelineConfigurationBody" Data..=)
              Prelude.<$> pipelineConfigurationBody
          ]
      )

instance Data.ToPath UpdatePipeline where
  toPath UpdatePipeline' {..} =
    Prelude.mconcat
      [ "/2022-01-01/osis/updatePipeline/",
        Data.toBS pipelineName
      ]

instance Data.ToQuery UpdatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | Container for information about the updated pipeline.
    pipeline :: Prelude.Maybe Pipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'updatePipelineResponse_pipeline' - Container for information about the updated pipeline.
--
-- 'httpStatus', 'updatePipelineResponse_httpStatus' - The response's http status code.
newUpdatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePipelineResponse
newUpdatePipelineResponse pHttpStatus_ =
  UpdatePipelineResponse'
    { pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Container for information about the updated pipeline.
updatePipelineResponse_pipeline :: Lens.Lens' UpdatePipelineResponse (Prelude.Maybe Pipeline)
updatePipelineResponse_pipeline = Lens.lens (\UpdatePipelineResponse' {pipeline} -> pipeline) (\s@UpdatePipelineResponse' {} a -> s {pipeline = a} :: UpdatePipelineResponse)

-- | The response's http status code.
updatePipelineResponse_httpStatus :: Lens.Lens' UpdatePipelineResponse Prelude.Int
updatePipelineResponse_httpStatus = Lens.lens (\UpdatePipelineResponse' {httpStatus} -> httpStatus) (\s@UpdatePipelineResponse' {} a -> s {httpStatus = a} :: UpdatePipelineResponse)

instance Prelude.NFData UpdatePipelineResponse where
  rnf UpdatePipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf httpStatus

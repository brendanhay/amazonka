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
-- Module      : Amazonka.OsIs.CreatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an OpenSearch Ingestion pipeline. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/creating-pipeline.html Creating Amazon OpenSearch Ingestion pipelines>.
module Amazonka.OsIs.CreatePipeline
  ( -- * Creating a Request
    CreatePipeline (..),
    newCreatePipeline,

    -- * Request Lenses
    createPipeline_logPublishingOptions,
    createPipeline_tags,
    createPipeline_vpcOptions,
    createPipeline_pipelineName,
    createPipeline_minUnits,
    createPipeline_maxUnits,
    createPipeline_pipelineConfigurationBody,

    -- * Destructuring the Response
    CreatePipelineResponse (..),
    newCreatePipelineResponse,

    -- * Response Lenses
    createPipelineResponse_pipeline,
    createPipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | Key-value pairs to configure log publishing.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptions,
    -- | List of tags to add to the pipeline upon creation.
    tags :: Prelude.Maybe [Tag],
    -- | Container for the values required to configure VPC access for the
    -- pipeline. If you don\'t specify these values, OpenSearch Ingestion
    -- creates the pipeline with a public endpoint.
    vpcOptions :: Prelude.Maybe VpcOptions,
    -- | The name of the OpenSearch Ingestion pipeline to create. Pipeline names
    -- are unique across the pipelines owned by an account within an Amazon Web
    -- Services Region.
    pipelineName :: Prelude.Text,
    -- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
    minUnits :: Prelude.Natural,
    -- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
    maxUnits :: Prelude.Natural,
    -- | The pipeline configuration in YAML format. The command accepts the
    -- pipeline configuration as a string or within a .yaml file. If you
    -- provide the configuration as a string, each new line must be escaped
    -- with @\\n@.
    pipelineConfigurationBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPublishingOptions', 'createPipeline_logPublishingOptions' - Key-value pairs to configure log publishing.
--
-- 'tags', 'createPipeline_tags' - List of tags to add to the pipeline upon creation.
--
-- 'vpcOptions', 'createPipeline_vpcOptions' - Container for the values required to configure VPC access for the
-- pipeline. If you don\'t specify these values, OpenSearch Ingestion
-- creates the pipeline with a public endpoint.
--
-- 'pipelineName', 'createPipeline_pipelineName' - The name of the OpenSearch Ingestion pipeline to create. Pipeline names
-- are unique across the pipelines owned by an account within an Amazon Web
-- Services Region.
--
-- 'minUnits', 'createPipeline_minUnits' - The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
--
-- 'maxUnits', 'createPipeline_maxUnits' - The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
--
-- 'pipelineConfigurationBody', 'createPipeline_pipelineConfigurationBody' - The pipeline configuration in YAML format. The command accepts the
-- pipeline configuration as a string or within a .yaml file. If you
-- provide the configuration as a string, each new line must be escaped
-- with @\\n@.
newCreatePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  -- | 'minUnits'
  Prelude.Natural ->
  -- | 'maxUnits'
  Prelude.Natural ->
  -- | 'pipelineConfigurationBody'
  Prelude.Text ->
  CreatePipeline
newCreatePipeline
  pPipelineName_
  pMinUnits_
  pMaxUnits_
  pPipelineConfigurationBody_ =
    CreatePipeline'
      { logPublishingOptions =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcOptions = Prelude.Nothing,
        pipelineName = pPipelineName_,
        minUnits = pMinUnits_,
        maxUnits = pMaxUnits_,
        pipelineConfigurationBody =
          pPipelineConfigurationBody_
      }

-- | Key-value pairs to configure log publishing.
createPipeline_logPublishingOptions :: Lens.Lens' CreatePipeline (Prelude.Maybe LogPublishingOptions)
createPipeline_logPublishingOptions = Lens.lens (\CreatePipeline' {logPublishingOptions} -> logPublishingOptions) (\s@CreatePipeline' {} a -> s {logPublishingOptions = a} :: CreatePipeline)

-- | List of tags to add to the pipeline upon creation.
createPipeline_tags :: Lens.Lens' CreatePipeline (Prelude.Maybe [Tag])
createPipeline_tags = Lens.lens (\CreatePipeline' {tags} -> tags) (\s@CreatePipeline' {} a -> s {tags = a} :: CreatePipeline) Prelude.. Lens.mapping Lens.coerced

-- | Container for the values required to configure VPC access for the
-- pipeline. If you don\'t specify these values, OpenSearch Ingestion
-- creates the pipeline with a public endpoint.
createPipeline_vpcOptions :: Lens.Lens' CreatePipeline (Prelude.Maybe VpcOptions)
createPipeline_vpcOptions = Lens.lens (\CreatePipeline' {vpcOptions} -> vpcOptions) (\s@CreatePipeline' {} a -> s {vpcOptions = a} :: CreatePipeline)

-- | The name of the OpenSearch Ingestion pipeline to create. Pipeline names
-- are unique across the pipelines owned by an account within an Amazon Web
-- Services Region.
createPipeline_pipelineName :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_pipelineName = Lens.lens (\CreatePipeline' {pipelineName} -> pipelineName) (\s@CreatePipeline' {} a -> s {pipelineName = a} :: CreatePipeline)

-- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
createPipeline_minUnits :: Lens.Lens' CreatePipeline Prelude.Natural
createPipeline_minUnits = Lens.lens (\CreatePipeline' {minUnits} -> minUnits) (\s@CreatePipeline' {} a -> s {minUnits = a} :: CreatePipeline)

-- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
createPipeline_maxUnits :: Lens.Lens' CreatePipeline Prelude.Natural
createPipeline_maxUnits = Lens.lens (\CreatePipeline' {maxUnits} -> maxUnits) (\s@CreatePipeline' {} a -> s {maxUnits = a} :: CreatePipeline)

-- | The pipeline configuration in YAML format. The command accepts the
-- pipeline configuration as a string or within a .yaml file. If you
-- provide the configuration as a string, each new line must be escaped
-- with @\\n@.
createPipeline_pipelineConfigurationBody :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_pipelineConfigurationBody = Lens.lens (\CreatePipeline' {pipelineConfigurationBody} -> pipelineConfigurationBody) (\s@CreatePipeline' {} a -> s {pipelineConfigurationBody = a} :: CreatePipeline)

instance Core.AWSRequest CreatePipeline where
  type
    AWSResponse CreatePipeline =
      CreatePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePipeline where
  hashWithSalt _salt CreatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcOptions
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` minUnits
      `Prelude.hashWithSalt` maxUnits
      `Prelude.hashWithSalt` pipelineConfigurationBody

instance Prelude.NFData CreatePipeline where
  rnf CreatePipeline' {..} =
    Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcOptions
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf minUnits
      `Prelude.seq` Prelude.rnf maxUnits
      `Prelude.seq` Prelude.rnf pipelineConfigurationBody

instance Data.ToHeaders CreatePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogPublishingOptions" Data..=)
              Prelude.<$> logPublishingOptions,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcOptions" Data..=) Prelude.<$> vpcOptions,
            Prelude.Just ("PipelineName" Data..= pipelineName),
            Prelude.Just ("MinUnits" Data..= minUnits),
            Prelude.Just ("MaxUnits" Data..= maxUnits),
            Prelude.Just
              ( "PipelineConfigurationBody"
                  Data..= pipelineConfigurationBody
              )
          ]
      )

instance Data.ToPath CreatePipeline where
  toPath =
    Prelude.const "/2022-01-01/osis/createPipeline"

instance Data.ToQuery CreatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | Container for information about the created pipeline.
    pipeline :: Prelude.Maybe Pipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'createPipelineResponse_pipeline' - Container for information about the created pipeline.
--
-- 'httpStatus', 'createPipelineResponse_httpStatus' - The response's http status code.
newCreatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePipelineResponse
newCreatePipelineResponse pHttpStatus_ =
  CreatePipelineResponse'
    { pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Container for information about the created pipeline.
createPipelineResponse_pipeline :: Lens.Lens' CreatePipelineResponse (Prelude.Maybe Pipeline)
createPipelineResponse_pipeline = Lens.lens (\CreatePipelineResponse' {pipeline} -> pipeline) (\s@CreatePipelineResponse' {} a -> s {pipeline = a} :: CreatePipelineResponse)

-- | The response's http status code.
createPipelineResponse_httpStatus :: Lens.Lens' CreatePipelineResponse Prelude.Int
createPipelineResponse_httpStatus = Lens.lens (\CreatePipelineResponse' {httpStatus} -> httpStatus) (\s@CreatePipelineResponse' {} a -> s {httpStatus = a} :: CreatePipelineResponse)

instance Prelude.NFData CreatePipelineResponse where
  rnf CreatePipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf httpStatus

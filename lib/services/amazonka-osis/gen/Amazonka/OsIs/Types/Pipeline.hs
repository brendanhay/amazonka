{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OsIs.Types.Pipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.Pipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types.LogPublishingOptions
import Amazonka.OsIs.Types.PipelineStatus
import Amazonka.OsIs.Types.PipelineStatusReason
import Amazonka.OsIs.Types.VpcEndpoint
import qualified Amazonka.Prelude as Prelude

-- | Information about an existing OpenSearch Ingestion pipeline.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | The date and time when the pipeline was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ingestion endpoints for the pipeline, which you can send data to.
    ingestEndpointUrls :: Prelude.Maybe [Prelude.Text],
    -- | The date and time when the pipeline was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Key-value pairs that represent log publishing settings.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptions,
    -- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
    maxUnits :: Prelude.Maybe Prelude.Int,
    -- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
    minUnits :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The Data Prepper pipeline configuration in YAML format.
    pipelineConfigurationBody :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the pipeline.
    status :: Prelude.Maybe PipelineStatus,
    -- | The reason for the current status of the pipeline.
    statusReason :: Prelude.Maybe PipelineStatusReason,
    -- | The VPC interface endpoints that have access to the pipeline.
    vpcEndpoints :: Prelude.Maybe [VpcEndpoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Pipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'pipeline_createdAt' - The date and time when the pipeline was created.
--
-- 'ingestEndpointUrls', 'pipeline_ingestEndpointUrls' - The ingestion endpoints for the pipeline, which you can send data to.
--
-- 'lastUpdatedAt', 'pipeline_lastUpdatedAt' - The date and time when the pipeline was last updated.
--
-- 'logPublishingOptions', 'pipeline_logPublishingOptions' - Key-value pairs that represent log publishing settings.
--
-- 'maxUnits', 'pipeline_maxUnits' - The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
--
-- 'minUnits', 'pipeline_minUnits' - The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
--
-- 'pipelineArn', 'pipeline_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineConfigurationBody', 'pipeline_pipelineConfigurationBody' - The Data Prepper pipeline configuration in YAML format.
--
-- 'pipelineName', 'pipeline_pipelineName' - The name of the pipeline.
--
-- 'status', 'pipeline_status' - The current status of the pipeline.
--
-- 'statusReason', 'pipeline_statusReason' - The reason for the current status of the pipeline.
--
-- 'vpcEndpoints', 'pipeline_vpcEndpoints' - The VPC interface endpoints that have access to the pipeline.
newPipeline ::
  Pipeline
newPipeline =
  Pipeline'
    { createdAt = Prelude.Nothing,
      ingestEndpointUrls = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      maxUnits = Prelude.Nothing,
      minUnits = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineConfigurationBody = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      vpcEndpoints = Prelude.Nothing
    }

-- | The date and time when the pipeline was created.
pipeline_createdAt :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_createdAt = Lens.lens (\Pipeline' {createdAt} -> createdAt) (\s@Pipeline' {} a -> s {createdAt = a} :: Pipeline) Prelude.. Lens.mapping Data._Time

-- | The ingestion endpoints for the pipeline, which you can send data to.
pipeline_ingestEndpointUrls :: Lens.Lens' Pipeline (Prelude.Maybe [Prelude.Text])
pipeline_ingestEndpointUrls = Lens.lens (\Pipeline' {ingestEndpointUrls} -> ingestEndpointUrls) (\s@Pipeline' {} a -> s {ingestEndpointUrls = a} :: Pipeline) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the pipeline was last updated.
pipeline_lastUpdatedAt :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastUpdatedAt = Lens.lens (\Pipeline' {lastUpdatedAt} -> lastUpdatedAt) (\s@Pipeline' {} a -> s {lastUpdatedAt = a} :: Pipeline) Prelude.. Lens.mapping Data._Time

-- | Key-value pairs that represent log publishing settings.
pipeline_logPublishingOptions :: Lens.Lens' Pipeline (Prelude.Maybe LogPublishingOptions)
pipeline_logPublishingOptions = Lens.lens (\Pipeline' {logPublishingOptions} -> logPublishingOptions) (\s@Pipeline' {} a -> s {logPublishingOptions = a} :: Pipeline)

-- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
pipeline_maxUnits :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Int)
pipeline_maxUnits = Lens.lens (\Pipeline' {maxUnits} -> maxUnits) (\s@Pipeline' {} a -> s {maxUnits = a} :: Pipeline)

-- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
pipeline_minUnits :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Int)
pipeline_minUnits = Lens.lens (\Pipeline' {minUnits} -> minUnits) (\s@Pipeline' {} a -> s {minUnits = a} :: Pipeline)

-- | The Amazon Resource Name (ARN) of the pipeline.
pipeline_pipelineArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineArn = Lens.lens (\Pipeline' {pipelineArn} -> pipelineArn) (\s@Pipeline' {} a -> s {pipelineArn = a} :: Pipeline)

-- | The Data Prepper pipeline configuration in YAML format.
pipeline_pipelineConfigurationBody :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineConfigurationBody = Lens.lens (\Pipeline' {pipelineConfigurationBody} -> pipelineConfigurationBody) (\s@Pipeline' {} a -> s {pipelineConfigurationBody = a} :: Pipeline)

-- | The name of the pipeline.
pipeline_pipelineName :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineName = Lens.lens (\Pipeline' {pipelineName} -> pipelineName) (\s@Pipeline' {} a -> s {pipelineName = a} :: Pipeline)

-- | The current status of the pipeline.
pipeline_status :: Lens.Lens' Pipeline (Prelude.Maybe PipelineStatus)
pipeline_status = Lens.lens (\Pipeline' {status} -> status) (\s@Pipeline' {} a -> s {status = a} :: Pipeline)

-- | The reason for the current status of the pipeline.
pipeline_statusReason :: Lens.Lens' Pipeline (Prelude.Maybe PipelineStatusReason)
pipeline_statusReason = Lens.lens (\Pipeline' {statusReason} -> statusReason) (\s@Pipeline' {} a -> s {statusReason = a} :: Pipeline)

-- | The VPC interface endpoints that have access to the pipeline.
pipeline_vpcEndpoints :: Lens.Lens' Pipeline (Prelude.Maybe [VpcEndpoint])
pipeline_vpcEndpoints = Lens.lens (\Pipeline' {vpcEndpoints} -> vpcEndpoints) (\s@Pipeline' {} a -> s {vpcEndpoints = a} :: Pipeline) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Pipeline where
  parseJSON =
    Data.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> ( x
                            Data..:? "IngestEndpointUrls"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "LogPublishingOptions")
            Prelude.<*> (x Data..:? "MaxUnits")
            Prelude.<*> (x Data..:? "MinUnits")
            Prelude.<*> (x Data..:? "PipelineArn")
            Prelude.<*> (x Data..:? "PipelineConfigurationBody")
            Prelude.<*> (x Data..:? "PipelineName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusReason")
            Prelude.<*> (x Data..:? "VpcEndpoints" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Pipeline where
  hashWithSalt _salt Pipeline' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` ingestEndpointUrls
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` maxUnits
      `Prelude.hashWithSalt` minUnits
      `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineConfigurationBody
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` vpcEndpoints

instance Prelude.NFData Pipeline where
  rnf Pipeline' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ingestEndpointUrls
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf maxUnits
      `Prelude.seq` Prelude.rnf minUnits
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineConfigurationBody
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf vpcEndpoints

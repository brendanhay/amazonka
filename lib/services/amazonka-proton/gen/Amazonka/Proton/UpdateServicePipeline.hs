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
-- Module      : Amazonka.Proton.UpdateServicePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the service pipeline.
--
-- There are four modes for updating a service pipeline. The
-- @deploymentType@ field defines the mode.
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     new spec that you provide. Only requested parameters are updated.
--     /Don’t/ include major or minor version parameters when you use this
--     @deployment-type@.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template by default. You can specify a different major
--     version that\'s higher than the major version in use and a minor
--     version.
module Amazonka.Proton.UpdateServicePipeline
  ( -- * Creating a Request
    UpdateServicePipeline (..),
    newUpdateServicePipeline,

    -- * Request Lenses
    updateServicePipeline_templateMajorVersion,
    updateServicePipeline_templateMinorVersion,
    updateServicePipeline_deploymentType,
    updateServicePipeline_serviceName,
    updateServicePipeline_spec,

    -- * Destructuring the Response
    UpdateServicePipelineResponse (..),
    newUpdateServicePipelineResponse,

    -- * Response Lenses
    updateServicePipelineResponse_httpStatus,
    updateServicePipelineResponse_pipeline,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServicePipeline' smart constructor.
data UpdateServicePipeline = UpdateServicePipeline'
  { -- | The major version of the service template that was used to create the
    -- service that the pipeline is associated with.
    templateMajorVersion :: Prelude.Maybe Prelude.Text,
    -- | The minor version of the service template that was used to create the
    -- service that the pipeline is associated with.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The deployment type.
    --
    -- There are four modes for updating a service pipeline. The
    -- @deploymentType@ field defines the mode.
    --
    -- []
    --     @NONE@
    --
    --     In this mode, a deployment /doesn\'t/ occur. Only the requested
    --     metadata parameters are updated.
    --
    -- []
    --     @CURRENT_VERSION@
    --
    --     In this mode, the service pipeline is deployed and updated with the
    --     new spec that you provide. Only requested parameters are updated.
    --     /Don’t/ include major or minor version parameters when you use this
    --     @deployment-type@.
    --
    -- []
    --     @MINOR_VERSION@
    --
    --     In this mode, the service pipeline is deployed and updated with the
    --     published, recommended (latest) minor version of the current major
    --     version in use, by default. You can specify a different minor
    --     version of the current major version in use.
    --
    -- []
    --     @MAJOR_VERSION@
    --
    --     In this mode, the service pipeline is deployed and updated with the
    --     published, recommended (latest) major and minor version of the
    --     current template, by default. You can specify a different major
    --     version that\'s higher than the major version in use and a minor
    --     version.
    deploymentType :: DeploymentUpdateType,
    -- | The name of the service to that the pipeline is associated with.
    serviceName :: Prelude.Text,
    -- | The spec for the service pipeline to update.
    spec :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServicePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateMajorVersion', 'updateServicePipeline_templateMajorVersion' - The major version of the service template that was used to create the
-- service that the pipeline is associated with.
--
-- 'templateMinorVersion', 'updateServicePipeline_templateMinorVersion' - The minor version of the service template that was used to create the
-- service that the pipeline is associated with.
--
-- 'deploymentType', 'updateServicePipeline_deploymentType' - The deployment type.
--
-- There are four modes for updating a service pipeline. The
-- @deploymentType@ field defines the mode.
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     new spec that you provide. Only requested parameters are updated.
--     /Don’t/ include major or minor version parameters when you use this
--     @deployment-type@.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template, by default. You can specify a different major
--     version that\'s higher than the major version in use and a minor
--     version.
--
-- 'serviceName', 'updateServicePipeline_serviceName' - The name of the service to that the pipeline is associated with.
--
-- 'spec', 'updateServicePipeline_spec' - The spec for the service pipeline to update.
newUpdateServicePipeline ::
  -- | 'deploymentType'
  DeploymentUpdateType ->
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'spec'
  Prelude.Text ->
  UpdateServicePipeline
newUpdateServicePipeline
  pDeploymentType_
  pServiceName_
  pSpec_ =
    UpdateServicePipeline'
      { templateMajorVersion =
          Prelude.Nothing,
        templateMinorVersion = Prelude.Nothing,
        deploymentType = pDeploymentType_,
        serviceName = pServiceName_,
        spec = Data._Sensitive Lens.# pSpec_
      }

-- | The major version of the service template that was used to create the
-- service that the pipeline is associated with.
updateServicePipeline_templateMajorVersion :: Lens.Lens' UpdateServicePipeline (Prelude.Maybe Prelude.Text)
updateServicePipeline_templateMajorVersion = Lens.lens (\UpdateServicePipeline' {templateMajorVersion} -> templateMajorVersion) (\s@UpdateServicePipeline' {} a -> s {templateMajorVersion = a} :: UpdateServicePipeline)

-- | The minor version of the service template that was used to create the
-- service that the pipeline is associated with.
updateServicePipeline_templateMinorVersion :: Lens.Lens' UpdateServicePipeline (Prelude.Maybe Prelude.Text)
updateServicePipeline_templateMinorVersion = Lens.lens (\UpdateServicePipeline' {templateMinorVersion} -> templateMinorVersion) (\s@UpdateServicePipeline' {} a -> s {templateMinorVersion = a} :: UpdateServicePipeline)

-- | The deployment type.
--
-- There are four modes for updating a service pipeline. The
-- @deploymentType@ field defines the mode.
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     new spec that you provide. Only requested parameters are updated.
--     /Don’t/ include major or minor version parameters when you use this
--     @deployment-type@.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the service pipeline is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template, by default. You can specify a different major
--     version that\'s higher than the major version in use and a minor
--     version.
updateServicePipeline_deploymentType :: Lens.Lens' UpdateServicePipeline DeploymentUpdateType
updateServicePipeline_deploymentType = Lens.lens (\UpdateServicePipeline' {deploymentType} -> deploymentType) (\s@UpdateServicePipeline' {} a -> s {deploymentType = a} :: UpdateServicePipeline)

-- | The name of the service to that the pipeline is associated with.
updateServicePipeline_serviceName :: Lens.Lens' UpdateServicePipeline Prelude.Text
updateServicePipeline_serviceName = Lens.lens (\UpdateServicePipeline' {serviceName} -> serviceName) (\s@UpdateServicePipeline' {} a -> s {serviceName = a} :: UpdateServicePipeline)

-- | The spec for the service pipeline to update.
updateServicePipeline_spec :: Lens.Lens' UpdateServicePipeline Prelude.Text
updateServicePipeline_spec = Lens.lens (\UpdateServicePipeline' {spec} -> spec) (\s@UpdateServicePipeline' {} a -> s {spec = a} :: UpdateServicePipeline) Prelude.. Data._Sensitive

instance Core.AWSRequest UpdateServicePipeline where
  type
    AWSResponse UpdateServicePipeline =
      UpdateServicePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServicePipelineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "pipeline")
      )

instance Prelude.Hashable UpdateServicePipeline where
  hashWithSalt _salt UpdateServicePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` spec

instance Prelude.NFData UpdateServicePipeline where
  rnf UpdateServicePipeline' {..} =
    Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf spec

instance Data.ToHeaders UpdateServicePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateServicePipeline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServicePipeline where
  toJSON UpdateServicePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("templateMajorVersion" Data..=)
              Prelude.<$> templateMajorVersion,
            ("templateMinorVersion" Data..=)
              Prelude.<$> templateMinorVersion,
            Prelude.Just
              ("deploymentType" Data..= deploymentType),
            Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("spec" Data..= spec)
          ]
      )

instance Data.ToPath UpdateServicePipeline where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateServicePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServicePipelineResponse' smart constructor.
data UpdateServicePipelineResponse = UpdateServicePipelineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The pipeline details that are returned by Proton.
    pipeline :: ServicePipeline
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServicePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServicePipelineResponse_httpStatus' - The response's http status code.
--
-- 'pipeline', 'updateServicePipelineResponse_pipeline' - The pipeline details that are returned by Proton.
newUpdateServicePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pipeline'
  ServicePipeline ->
  UpdateServicePipelineResponse
newUpdateServicePipelineResponse
  pHttpStatus_
  pPipeline_ =
    UpdateServicePipelineResponse'
      { httpStatus =
          pHttpStatus_,
        pipeline = pPipeline_
      }

-- | The response's http status code.
updateServicePipelineResponse_httpStatus :: Lens.Lens' UpdateServicePipelineResponse Prelude.Int
updateServicePipelineResponse_httpStatus = Lens.lens (\UpdateServicePipelineResponse' {httpStatus} -> httpStatus) (\s@UpdateServicePipelineResponse' {} a -> s {httpStatus = a} :: UpdateServicePipelineResponse)

-- | The pipeline details that are returned by Proton.
updateServicePipelineResponse_pipeline :: Lens.Lens' UpdateServicePipelineResponse ServicePipeline
updateServicePipelineResponse_pipeline = Lens.lens (\UpdateServicePipelineResponse' {pipeline} -> pipeline) (\s@UpdateServicePipelineResponse' {} a -> s {pipeline = a} :: UpdateServicePipelineResponse)

instance Prelude.NFData UpdateServicePipelineResponse where
  rnf UpdateServicePipelineResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pipeline

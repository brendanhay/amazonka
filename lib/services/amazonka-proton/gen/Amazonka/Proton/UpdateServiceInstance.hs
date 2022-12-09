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
-- Module      : Amazonka.Proton.UpdateServiceInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a service instance.
--
-- There are a few modes for updating a service instance. The
-- @deploymentType@ field defines the mode.
--
-- You can\'t update a service instance while its deployment status, or the
-- deployment status of a component attached to it, is @IN_PROGRESS@.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
module Amazonka.Proton.UpdateServiceInstance
  ( -- * Creating a Request
    UpdateServiceInstance (..),
    newUpdateServiceInstance,

    -- * Request Lenses
    updateServiceInstance_spec,
    updateServiceInstance_templateMajorVersion,
    updateServiceInstance_templateMinorVersion,
    updateServiceInstance_deploymentType,
    updateServiceInstance_name,
    updateServiceInstance_serviceName,

    -- * Destructuring the Response
    UpdateServiceInstanceResponse (..),
    newUpdateServiceInstanceResponse,

    -- * Response Lenses
    updateServiceInstanceResponse_httpStatus,
    updateServiceInstanceResponse_serviceInstance,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceInstance' smart constructor.
data UpdateServiceInstance = UpdateServiceInstance'
  { -- | The formatted specification that defines the service instance update.
    spec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The major version of the service template to update.
    templateMajorVersion :: Prelude.Maybe Prelude.Text,
    -- | The minor version of the service template to update.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The deployment type. It defines the mode for updating a service
    -- instance, as follows:
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
    --     In this mode, the service instance is deployed and updated with the
    --     new spec that you provide. Only requested parameters are updated.
    --     /Don’t/ include major or minor version parameters when you use this
    --     deployment type.
    --
    -- []
    --     @MINOR_VERSION@
    --
    --     In this mode, the service instance is deployed and updated with the
    --     published, recommended (latest) minor version of the current major
    --     version in use, by default. You can also specify a different minor
    --     version of the current major version in use.
    --
    -- []
    --     @MAJOR_VERSION@
    --
    --     In this mode, the service instance is deployed and updated with the
    --     published, recommended (latest) major and minor version of the
    --     current template, by default. You can specify a different major
    --     version that\'s higher than the major version in use and a minor
    --     version.
    deploymentType :: DeploymentUpdateType,
    -- | The name of the service instance to update.
    name :: Prelude.Text,
    -- | The name of the service that the service instance belongs to.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spec', 'updateServiceInstance_spec' - The formatted specification that defines the service instance update.
--
-- 'templateMajorVersion', 'updateServiceInstance_templateMajorVersion' - The major version of the service template to update.
--
-- 'templateMinorVersion', 'updateServiceInstance_templateMinorVersion' - The minor version of the service template to update.
--
-- 'deploymentType', 'updateServiceInstance_deploymentType' - The deployment type. It defines the mode for updating a service
-- instance, as follows:
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
--     In this mode, the service instance is deployed and updated with the
--     new spec that you provide. Only requested parameters are updated.
--     /Don’t/ include major or minor version parameters when you use this
--     deployment type.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the service instance is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can also specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the service instance is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template, by default. You can specify a different major
--     version that\'s higher than the major version in use and a minor
--     version.
--
-- 'name', 'updateServiceInstance_name' - The name of the service instance to update.
--
-- 'serviceName', 'updateServiceInstance_serviceName' - The name of the service that the service instance belongs to.
newUpdateServiceInstance ::
  -- | 'deploymentType'
  DeploymentUpdateType ->
  -- | 'name'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  UpdateServiceInstance
newUpdateServiceInstance
  pDeploymentType_
  pName_
  pServiceName_ =
    UpdateServiceInstance'
      { spec = Prelude.Nothing,
        templateMajorVersion = Prelude.Nothing,
        templateMinorVersion = Prelude.Nothing,
        deploymentType = pDeploymentType_,
        name = pName_,
        serviceName = pServiceName_
      }

-- | The formatted specification that defines the service instance update.
updateServiceInstance_spec :: Lens.Lens' UpdateServiceInstance (Prelude.Maybe Prelude.Text)
updateServiceInstance_spec = Lens.lens (\UpdateServiceInstance' {spec} -> spec) (\s@UpdateServiceInstance' {} a -> s {spec = a} :: UpdateServiceInstance) Prelude.. Lens.mapping Data._Sensitive

-- | The major version of the service template to update.
updateServiceInstance_templateMajorVersion :: Lens.Lens' UpdateServiceInstance (Prelude.Maybe Prelude.Text)
updateServiceInstance_templateMajorVersion = Lens.lens (\UpdateServiceInstance' {templateMajorVersion} -> templateMajorVersion) (\s@UpdateServiceInstance' {} a -> s {templateMajorVersion = a} :: UpdateServiceInstance)

-- | The minor version of the service template to update.
updateServiceInstance_templateMinorVersion :: Lens.Lens' UpdateServiceInstance (Prelude.Maybe Prelude.Text)
updateServiceInstance_templateMinorVersion = Lens.lens (\UpdateServiceInstance' {templateMinorVersion} -> templateMinorVersion) (\s@UpdateServiceInstance' {} a -> s {templateMinorVersion = a} :: UpdateServiceInstance)

-- | The deployment type. It defines the mode for updating a service
-- instance, as follows:
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
--     In this mode, the service instance is deployed and updated with the
--     new spec that you provide. Only requested parameters are updated.
--     /Don’t/ include major or minor version parameters when you use this
--     deployment type.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the service instance is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can also specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the service instance is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template, by default. You can specify a different major
--     version that\'s higher than the major version in use and a minor
--     version.
updateServiceInstance_deploymentType :: Lens.Lens' UpdateServiceInstance DeploymentUpdateType
updateServiceInstance_deploymentType = Lens.lens (\UpdateServiceInstance' {deploymentType} -> deploymentType) (\s@UpdateServiceInstance' {} a -> s {deploymentType = a} :: UpdateServiceInstance)

-- | The name of the service instance to update.
updateServiceInstance_name :: Lens.Lens' UpdateServiceInstance Prelude.Text
updateServiceInstance_name = Lens.lens (\UpdateServiceInstance' {name} -> name) (\s@UpdateServiceInstance' {} a -> s {name = a} :: UpdateServiceInstance)

-- | The name of the service that the service instance belongs to.
updateServiceInstance_serviceName :: Lens.Lens' UpdateServiceInstance Prelude.Text
updateServiceInstance_serviceName = Lens.lens (\UpdateServiceInstance' {serviceName} -> serviceName) (\s@UpdateServiceInstance' {} a -> s {serviceName = a} :: UpdateServiceInstance)

instance Core.AWSRequest UpdateServiceInstance where
  type
    AWSResponse UpdateServiceInstance =
      UpdateServiceInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "serviceInstance")
      )

instance Prelude.Hashable UpdateServiceInstance where
  hashWithSalt _salt UpdateServiceInstance' {..} =
    _salt `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData UpdateServiceInstance where
  rnf UpdateServiceInstance' {..} =
    Prelude.rnf spec
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders UpdateServiceInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateServiceInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServiceInstance where
  toJSON UpdateServiceInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("spec" Data..=) Prelude.<$> spec,
            ("templateMajorVersion" Data..=)
              Prelude.<$> templateMajorVersion,
            ("templateMinorVersion" Data..=)
              Prelude.<$> templateMinorVersion,
            Prelude.Just
              ("deploymentType" Data..= deploymentType),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath UpdateServiceInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateServiceInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceInstanceResponse' smart constructor.
data UpdateServiceInstanceResponse = UpdateServiceInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service instance summary data that\'s returned by Proton.
    serviceInstance :: ServiceInstance
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServiceInstanceResponse_httpStatus' - The response's http status code.
--
-- 'serviceInstance', 'updateServiceInstanceResponse_serviceInstance' - The service instance summary data that\'s returned by Proton.
newUpdateServiceInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceInstance'
  ServiceInstance ->
  UpdateServiceInstanceResponse
newUpdateServiceInstanceResponse
  pHttpStatus_
  pServiceInstance_ =
    UpdateServiceInstanceResponse'
      { httpStatus =
          pHttpStatus_,
        serviceInstance = pServiceInstance_
      }

-- | The response's http status code.
updateServiceInstanceResponse_httpStatus :: Lens.Lens' UpdateServiceInstanceResponse Prelude.Int
updateServiceInstanceResponse_httpStatus = Lens.lens (\UpdateServiceInstanceResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceInstanceResponse' {} a -> s {httpStatus = a} :: UpdateServiceInstanceResponse)

-- | The service instance summary data that\'s returned by Proton.
updateServiceInstanceResponse_serviceInstance :: Lens.Lens' UpdateServiceInstanceResponse ServiceInstance
updateServiceInstanceResponse_serviceInstance = Lens.lens (\UpdateServiceInstanceResponse' {serviceInstance} -> serviceInstance) (\s@UpdateServiceInstanceResponse' {} a -> s {serviceInstance = a} :: UpdateServiceInstanceResponse)

instance Prelude.NFData UpdateServiceInstanceResponse where
  rnf UpdateServiceInstanceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceInstance

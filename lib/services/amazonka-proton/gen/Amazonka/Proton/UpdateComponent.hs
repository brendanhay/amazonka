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
-- Module      : Amazonka.Proton.UpdateComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a component.
--
-- There are a few modes for updating a component. The @deploymentType@
-- field defines the mode.
--
-- You can\'t update a component while its deployment status, or the
-- deployment status of a service instance attached to it, is
-- @IN_PROGRESS@.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
module Amazonka.Proton.UpdateComponent
  ( -- * Creating a Request
    UpdateComponent (..),
    newUpdateComponent,

    -- * Request Lenses
    updateComponent_serviceSpec,
    updateComponent_templateFile,
    updateComponent_description,
    updateComponent_serviceName,
    updateComponent_serviceInstanceName,
    updateComponent_deploymentType,
    updateComponent_name,

    -- * Destructuring the Response
    UpdateComponentResponse (..),
    newUpdateComponentResponse,

    -- * Response Lenses
    updateComponentResponse_httpStatus,
    updateComponentResponse_component,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateComponent' smart constructor.
data UpdateComponent = UpdateComponent'
  { -- | The service spec that you want the component to use to access service
    -- inputs. Set this only when the component is attached to a service
    -- instance.
    serviceSpec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A path to the Infrastructure as Code (IaC) file describing
    -- infrastructure that a custom component provisions.
    --
    -- Components support a single IaC file, even if you use Terraform as your
    -- template language.
    templateFile :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An optional customer-provided description of the component.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the service that @serviceInstanceName@ is associated with.
    -- Don\'t specify to keep the component\'s current service instance
    -- attachment. Specify an empty string to detach the component from the
    -- service instance it\'s attached to. Specify non-empty values for both
    -- @serviceInstanceName@ and @serviceName@ or for neither of them.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the service instance that you want to attach this component
    -- to. Don\'t specify to keep the component\'s current service instance
    -- attachment. Specify an empty string to detach the component from the
    -- service instance it\'s attached to. Specify non-empty values for both
    -- @serviceInstanceName@ and @serviceName@ or for neither of them.
    serviceInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The deployment type. It defines the mode for updating a component, as
    -- follows:
    --
    -- []
    --     @NONE@
    --
    --     In this mode, a deployment /doesn\'t/ occur. Only the requested
    --     metadata parameters are updated. You can only specify @description@
    --     in this mode.
    --
    -- []
    --     @CURRENT_VERSION@
    --
    --     In this mode, the component is deployed and updated with the new
    --     @serviceSpec@, @templateSource@, and\/or @type@ that you provide.
    --     Only requested parameters are updated.
    deploymentType :: ComponentDeploymentUpdateType,
    -- | The name of the component to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSpec', 'updateComponent_serviceSpec' - The service spec that you want the component to use to access service
-- inputs. Set this only when the component is attached to a service
-- instance.
--
-- 'templateFile', 'updateComponent_templateFile' - A path to the Infrastructure as Code (IaC) file describing
-- infrastructure that a custom component provisions.
--
-- Components support a single IaC file, even if you use Terraform as your
-- template language.
--
-- 'description', 'updateComponent_description' - An optional customer-provided description of the component.
--
-- 'serviceName', 'updateComponent_serviceName' - The name of the service that @serviceInstanceName@ is associated with.
-- Don\'t specify to keep the component\'s current service instance
-- attachment. Specify an empty string to detach the component from the
-- service instance it\'s attached to. Specify non-empty values for both
-- @serviceInstanceName@ and @serviceName@ or for neither of them.
--
-- 'serviceInstanceName', 'updateComponent_serviceInstanceName' - The name of the service instance that you want to attach this component
-- to. Don\'t specify to keep the component\'s current service instance
-- attachment. Specify an empty string to detach the component from the
-- service instance it\'s attached to. Specify non-empty values for both
-- @serviceInstanceName@ and @serviceName@ or for neither of them.
--
-- 'deploymentType', 'updateComponent_deploymentType' - The deployment type. It defines the mode for updating a component, as
-- follows:
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated. You can only specify @description@
--     in this mode.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the component is deployed and updated with the new
--     @serviceSpec@, @templateSource@, and\/or @type@ that you provide.
--     Only requested parameters are updated.
--
-- 'name', 'updateComponent_name' - The name of the component to update.
newUpdateComponent ::
  -- | 'deploymentType'
  ComponentDeploymentUpdateType ->
  -- | 'name'
  Prelude.Text ->
  UpdateComponent
newUpdateComponent pDeploymentType_ pName_ =
  UpdateComponent'
    { serviceSpec = Prelude.Nothing,
      templateFile = Prelude.Nothing,
      description = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceInstanceName = Prelude.Nothing,
      deploymentType = pDeploymentType_,
      name = pName_
    }

-- | The service spec that you want the component to use to access service
-- inputs. Set this only when the component is attached to a service
-- instance.
updateComponent_serviceSpec :: Lens.Lens' UpdateComponent (Prelude.Maybe Prelude.Text)
updateComponent_serviceSpec = Lens.lens (\UpdateComponent' {serviceSpec} -> serviceSpec) (\s@UpdateComponent' {} a -> s {serviceSpec = a} :: UpdateComponent) Prelude.. Lens.mapping Data._Sensitive

-- | A path to the Infrastructure as Code (IaC) file describing
-- infrastructure that a custom component provisions.
--
-- Components support a single IaC file, even if you use Terraform as your
-- template language.
updateComponent_templateFile :: Lens.Lens' UpdateComponent (Prelude.Maybe Prelude.Text)
updateComponent_templateFile = Lens.lens (\UpdateComponent' {templateFile} -> templateFile) (\s@UpdateComponent' {} a -> s {templateFile = a} :: UpdateComponent) Prelude.. Lens.mapping Data._Sensitive

-- | An optional customer-provided description of the component.
updateComponent_description :: Lens.Lens' UpdateComponent (Prelude.Maybe Prelude.Text)
updateComponent_description = Lens.lens (\UpdateComponent' {description} -> description) (\s@UpdateComponent' {} a -> s {description = a} :: UpdateComponent) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the service that @serviceInstanceName@ is associated with.
-- Don\'t specify to keep the component\'s current service instance
-- attachment. Specify an empty string to detach the component from the
-- service instance it\'s attached to. Specify non-empty values for both
-- @serviceInstanceName@ and @serviceName@ or for neither of them.
updateComponent_serviceName :: Lens.Lens' UpdateComponent (Prelude.Maybe Prelude.Text)
updateComponent_serviceName = Lens.lens (\UpdateComponent' {serviceName} -> serviceName) (\s@UpdateComponent' {} a -> s {serviceName = a} :: UpdateComponent)

-- | The name of the service instance that you want to attach this component
-- to. Don\'t specify to keep the component\'s current service instance
-- attachment. Specify an empty string to detach the component from the
-- service instance it\'s attached to. Specify non-empty values for both
-- @serviceInstanceName@ and @serviceName@ or for neither of them.
updateComponent_serviceInstanceName :: Lens.Lens' UpdateComponent (Prelude.Maybe Prelude.Text)
updateComponent_serviceInstanceName = Lens.lens (\UpdateComponent' {serviceInstanceName} -> serviceInstanceName) (\s@UpdateComponent' {} a -> s {serviceInstanceName = a} :: UpdateComponent)

-- | The deployment type. It defines the mode for updating a component, as
-- follows:
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated. You can only specify @description@
--     in this mode.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the component is deployed and updated with the new
--     @serviceSpec@, @templateSource@, and\/or @type@ that you provide.
--     Only requested parameters are updated.
updateComponent_deploymentType :: Lens.Lens' UpdateComponent ComponentDeploymentUpdateType
updateComponent_deploymentType = Lens.lens (\UpdateComponent' {deploymentType} -> deploymentType) (\s@UpdateComponent' {} a -> s {deploymentType = a} :: UpdateComponent)

-- | The name of the component to update.
updateComponent_name :: Lens.Lens' UpdateComponent Prelude.Text
updateComponent_name = Lens.lens (\UpdateComponent' {name} -> name) (\s@UpdateComponent' {} a -> s {name = a} :: UpdateComponent)

instance Core.AWSRequest UpdateComponent where
  type
    AWSResponse UpdateComponent =
      UpdateComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateComponentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "component")
      )

instance Prelude.Hashable UpdateComponent where
  hashWithSalt _salt UpdateComponent' {..} =
    _salt `Prelude.hashWithSalt` serviceSpec
      `Prelude.hashWithSalt` templateFile
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceInstanceName
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateComponent where
  rnf UpdateComponent' {..} =
    Prelude.rnf serviceSpec
      `Prelude.seq` Prelude.rnf templateFile
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateComponent where
  toJSON UpdateComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("serviceSpec" Data..=) Prelude.<$> serviceSpec,
            ("templateFile" Data..=) Prelude.<$> templateFile,
            ("description" Data..=) Prelude.<$> description,
            ("serviceName" Data..=) Prelude.<$> serviceName,
            ("serviceInstanceName" Data..=)
              Prelude.<$> serviceInstanceName,
            Prelude.Just
              ("deploymentType" Data..= deploymentType),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateComponentResponse' smart constructor.
data UpdateComponentResponse = UpdateComponentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the updated component.
    component :: Component
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateComponentResponse_httpStatus' - The response's http status code.
--
-- 'component', 'updateComponentResponse_component' - The detailed data of the updated component.
newUpdateComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'component'
  Component ->
  UpdateComponentResponse
newUpdateComponentResponse pHttpStatus_ pComponent_ =
  UpdateComponentResponse'
    { httpStatus = pHttpStatus_,
      component = pComponent_
    }

-- | The response's http status code.
updateComponentResponse_httpStatus :: Lens.Lens' UpdateComponentResponse Prelude.Int
updateComponentResponse_httpStatus = Lens.lens (\UpdateComponentResponse' {httpStatus} -> httpStatus) (\s@UpdateComponentResponse' {} a -> s {httpStatus = a} :: UpdateComponentResponse)

-- | The detailed data of the updated component.
updateComponentResponse_component :: Lens.Lens' UpdateComponentResponse Component
updateComponentResponse_component = Lens.lens (\UpdateComponentResponse' {component} -> component) (\s@UpdateComponentResponse' {} a -> s {component = a} :: UpdateComponentResponse)

instance Prelude.NFData UpdateComponentResponse where
  rnf UpdateComponentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf component

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
-- Module      : Amazonka.SageMaker.UpdateEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys the new @EndpointConfig@ specified in the request, switches to
-- using newly created endpoint, and then deletes resources provisioned for
-- the endpoint using the previous @EndpointConfig@ (there is no
-- availability loss).
--
-- When SageMaker receives the request, it sets the endpoint status to
-- @Updating@. After updating the endpoint, it sets the status to
-- @InService@. To check the status of an endpoint, use the
-- DescribeEndpoint API.
--
-- You must not delete an @EndpointConfig@ in use by an endpoint that is
-- live or while the @UpdateEndpoint@ or @CreateEndpoint@ operations are
-- being performed on the endpoint. To update an endpoint, you must create
-- a new @EndpointConfig@.
--
-- If you delete the @EndpointConfig@ of an endpoint that is active or
-- being created or updated you may lose visibility into the instance type
-- the endpoint is using. The endpoint must be deleted in order to stop
-- incurring charges.
module Amazonka.SageMaker.UpdateEndpoint
  ( -- * Creating a Request
    UpdateEndpoint (..),
    newUpdateEndpoint,

    -- * Request Lenses
    updateEndpoint_retainDeploymentConfig,
    updateEndpoint_retainAllVariantProperties,
    updateEndpoint_deploymentConfig,
    updateEndpoint_excludeRetainedVariantProperties,
    updateEndpoint_endpointName,
    updateEndpoint_endpointConfigName,

    -- * Destructuring the Response
    UpdateEndpointResponse (..),
    newUpdateEndpointResponse,

    -- * Response Lenses
    updateEndpointResponse_httpStatus,
    updateEndpointResponse_endpointArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | Specifies whether to reuse the last deployment configuration. The
    -- default value is false (the configuration is not reused).
    retainDeploymentConfig :: Prelude.Maybe Prelude.Bool,
    -- | When updating endpoint resources, enables or disables the retention of
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_VariantProperty.html variant properties>,
    -- such as the instance count or the variant weight. To retain the variant
    -- properties of an endpoint when updating it, set
    -- @RetainAllVariantProperties@ to @true@. To use the variant properties
    -- specified in a new @EndpointConfig@ call when updating an endpoint, set
    -- @RetainAllVariantProperties@ to @false@. The default is @false@.
    retainAllVariantProperties :: Prelude.Maybe Prelude.Bool,
    -- | The deployment configuration for an endpoint, which contains the desired
    -- deployment strategy and rollback configurations.
    deploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | When you are updating endpoint resources with
    -- UpdateEndpointInput$RetainAllVariantProperties, whose value is set to
    -- @true@, @ExcludeRetainedVariantProperties@ specifies the list of type
    -- VariantProperty to override with the values provided by
    -- @EndpointConfig@. If you don\'t specify a value for
    -- @ExcludeAllVariantProperties@, no variant properties are overridden.
    excludeRetainedVariantProperties :: Prelude.Maybe [VariantProperty],
    -- | The name of the endpoint whose configuration you want to update.
    endpointName :: Prelude.Text,
    -- | The name of the new endpoint configuration.
    endpointConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retainDeploymentConfig', 'updateEndpoint_retainDeploymentConfig' - Specifies whether to reuse the last deployment configuration. The
-- default value is false (the configuration is not reused).
--
-- 'retainAllVariantProperties', 'updateEndpoint_retainAllVariantProperties' - When updating endpoint resources, enables or disables the retention of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_VariantProperty.html variant properties>,
-- such as the instance count or the variant weight. To retain the variant
-- properties of an endpoint when updating it, set
-- @RetainAllVariantProperties@ to @true@. To use the variant properties
-- specified in a new @EndpointConfig@ call when updating an endpoint, set
-- @RetainAllVariantProperties@ to @false@. The default is @false@.
--
-- 'deploymentConfig', 'updateEndpoint_deploymentConfig' - The deployment configuration for an endpoint, which contains the desired
-- deployment strategy and rollback configurations.
--
-- 'excludeRetainedVariantProperties', 'updateEndpoint_excludeRetainedVariantProperties' - When you are updating endpoint resources with
-- UpdateEndpointInput$RetainAllVariantProperties, whose value is set to
-- @true@, @ExcludeRetainedVariantProperties@ specifies the list of type
-- VariantProperty to override with the values provided by
-- @EndpointConfig@. If you don\'t specify a value for
-- @ExcludeAllVariantProperties@, no variant properties are overridden.
--
-- 'endpointName', 'updateEndpoint_endpointName' - The name of the endpoint whose configuration you want to update.
--
-- 'endpointConfigName', 'updateEndpoint_endpointConfigName' - The name of the new endpoint configuration.
newUpdateEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'endpointConfigName'
  Prelude.Text ->
  UpdateEndpoint
newUpdateEndpoint pEndpointName_ pEndpointConfigName_ =
  UpdateEndpoint'
    { retainDeploymentConfig =
        Prelude.Nothing,
      retainAllVariantProperties = Prelude.Nothing,
      deploymentConfig = Prelude.Nothing,
      excludeRetainedVariantProperties = Prelude.Nothing,
      endpointName = pEndpointName_,
      endpointConfigName = pEndpointConfigName_
    }

-- | Specifies whether to reuse the last deployment configuration. The
-- default value is false (the configuration is not reused).
updateEndpoint_retainDeploymentConfig :: Lens.Lens' UpdateEndpoint (Prelude.Maybe Prelude.Bool)
updateEndpoint_retainDeploymentConfig = Lens.lens (\UpdateEndpoint' {retainDeploymentConfig} -> retainDeploymentConfig) (\s@UpdateEndpoint' {} a -> s {retainDeploymentConfig = a} :: UpdateEndpoint)

-- | When updating endpoint resources, enables or disables the retention of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_VariantProperty.html variant properties>,
-- such as the instance count or the variant weight. To retain the variant
-- properties of an endpoint when updating it, set
-- @RetainAllVariantProperties@ to @true@. To use the variant properties
-- specified in a new @EndpointConfig@ call when updating an endpoint, set
-- @RetainAllVariantProperties@ to @false@. The default is @false@.
updateEndpoint_retainAllVariantProperties :: Lens.Lens' UpdateEndpoint (Prelude.Maybe Prelude.Bool)
updateEndpoint_retainAllVariantProperties = Lens.lens (\UpdateEndpoint' {retainAllVariantProperties} -> retainAllVariantProperties) (\s@UpdateEndpoint' {} a -> s {retainAllVariantProperties = a} :: UpdateEndpoint)

-- | The deployment configuration for an endpoint, which contains the desired
-- deployment strategy and rollback configurations.
updateEndpoint_deploymentConfig :: Lens.Lens' UpdateEndpoint (Prelude.Maybe DeploymentConfig)
updateEndpoint_deploymentConfig = Lens.lens (\UpdateEndpoint' {deploymentConfig} -> deploymentConfig) (\s@UpdateEndpoint' {} a -> s {deploymentConfig = a} :: UpdateEndpoint)

-- | When you are updating endpoint resources with
-- UpdateEndpointInput$RetainAllVariantProperties, whose value is set to
-- @true@, @ExcludeRetainedVariantProperties@ specifies the list of type
-- VariantProperty to override with the values provided by
-- @EndpointConfig@. If you don\'t specify a value for
-- @ExcludeAllVariantProperties@, no variant properties are overridden.
updateEndpoint_excludeRetainedVariantProperties :: Lens.Lens' UpdateEndpoint (Prelude.Maybe [VariantProperty])
updateEndpoint_excludeRetainedVariantProperties = Lens.lens (\UpdateEndpoint' {excludeRetainedVariantProperties} -> excludeRetainedVariantProperties) (\s@UpdateEndpoint' {} a -> s {excludeRetainedVariantProperties = a} :: UpdateEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The name of the endpoint whose configuration you want to update.
updateEndpoint_endpointName :: Lens.Lens' UpdateEndpoint Prelude.Text
updateEndpoint_endpointName = Lens.lens (\UpdateEndpoint' {endpointName} -> endpointName) (\s@UpdateEndpoint' {} a -> s {endpointName = a} :: UpdateEndpoint)

-- | The name of the new endpoint configuration.
updateEndpoint_endpointConfigName :: Lens.Lens' UpdateEndpoint Prelude.Text
updateEndpoint_endpointConfigName = Lens.lens (\UpdateEndpoint' {endpointConfigName} -> endpointConfigName) (\s@UpdateEndpoint' {} a -> s {endpointConfigName = a} :: UpdateEndpoint)

instance Core.AWSRequest UpdateEndpoint where
  type
    AWSResponse UpdateEndpoint =
      UpdateEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "EndpointArn")
      )

instance Prelude.Hashable UpdateEndpoint where
  hashWithSalt _salt UpdateEndpoint' {..} =
    _salt `Prelude.hashWithSalt` retainDeploymentConfig
      `Prelude.hashWithSalt` retainAllVariantProperties
      `Prelude.hashWithSalt` deploymentConfig
      `Prelude.hashWithSalt` excludeRetainedVariantProperties
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` endpointConfigName

instance Prelude.NFData UpdateEndpoint where
  rnf UpdateEndpoint' {..} =
    Prelude.rnf retainDeploymentConfig
      `Prelude.seq` Prelude.rnf retainAllVariantProperties
      `Prelude.seq` Prelude.rnf deploymentConfig
      `Prelude.seq` Prelude.rnf excludeRetainedVariantProperties
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf endpointConfigName

instance Core.ToHeaders UpdateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RetainDeploymentConfig" Core..=)
              Prelude.<$> retainDeploymentConfig,
            ("RetainAllVariantProperties" Core..=)
              Prelude.<$> retainAllVariantProperties,
            ("DeploymentConfig" Core..=)
              Prelude.<$> deploymentConfig,
            ("ExcludeRetainedVariantProperties" Core..=)
              Prelude.<$> excludeRetainedVariantProperties,
            Prelude.Just ("EndpointName" Core..= endpointName),
            Prelude.Just
              ("EndpointConfigName" Core..= endpointConfigName)
          ]
      )

instance Core.ToPath UpdateEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEndpointResponse_httpStatus' - The response's http status code.
--
-- 'endpointArn', 'updateEndpointResponse_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
newUpdateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointArn'
  Prelude.Text ->
  UpdateEndpointResponse
newUpdateEndpointResponse pHttpStatus_ pEndpointArn_ =
  UpdateEndpointResponse'
    { httpStatus = pHttpStatus_,
      endpointArn = pEndpointArn_
    }

-- | The response's http status code.
updateEndpointResponse_httpStatus :: Lens.Lens' UpdateEndpointResponse Prelude.Int
updateEndpointResponse_httpStatus = Lens.lens (\UpdateEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointResponse' {} a -> s {httpStatus = a} :: UpdateEndpointResponse)

-- | The Amazon Resource Name (ARN) of the endpoint.
updateEndpointResponse_endpointArn :: Lens.Lens' UpdateEndpointResponse Prelude.Text
updateEndpointResponse_endpointArn = Lens.lens (\UpdateEndpointResponse' {endpointArn} -> endpointArn) (\s@UpdateEndpointResponse' {} a -> s {endpointArn = a} :: UpdateEndpointResponse)

instance Prelude.NFData UpdateEndpointResponse where
  rnf UpdateEndpointResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointArn

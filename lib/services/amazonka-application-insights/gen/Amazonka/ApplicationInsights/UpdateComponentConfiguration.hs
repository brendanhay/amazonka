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
-- Module      : Amazonka.ApplicationInsights.UpdateComponentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the monitoring configurations for the component. The
-- configuration input parameter is an escaped JSON of the configuration
-- and should match the schema of what is returned by
-- @DescribeComponentConfigurationRecommendation@.
module Amazonka.ApplicationInsights.UpdateComponentConfiguration
  ( -- * Creating a Request
    UpdateComponentConfiguration (..),
    newUpdateComponentConfiguration,

    -- * Request Lenses
    updateComponentConfiguration_autoConfigEnabled,
    updateComponentConfiguration_componentConfiguration,
    updateComponentConfiguration_monitor,
    updateComponentConfiguration_tier,
    updateComponentConfiguration_resourceGroupName,
    updateComponentConfiguration_componentName,

    -- * Destructuring the Response
    UpdateComponentConfigurationResponse (..),
    newUpdateComponentConfigurationResponse,

    -- * Response Lenses
    updateComponentConfigurationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateComponentConfiguration' smart constructor.
data UpdateComponentConfiguration = UpdateComponentConfiguration'
  { -- | Automatically configures the component by applying the recommended
    -- configurations.
    autoConfigEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The configuration settings of the component. The value is the escaped
    -- JSON of the configuration. For more information about the JSON format,
    -- see
    -- <https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/working-with-json.html Working with JSON>.
    -- You can send a request to @DescribeComponentConfigurationRecommendation@
    -- to see the recommended configuration for a component. For the complete
    -- format of the component configuration file, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/component-config.html Component Configuration>.
    componentConfiguration :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the application component is monitored.
    monitor :: Prelude.Maybe Prelude.Bool,
    -- | The tier of the application component.
    tier :: Prelude.Maybe Tier,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoConfigEnabled', 'updateComponentConfiguration_autoConfigEnabled' - Automatically configures the component by applying the recommended
-- configurations.
--
-- 'componentConfiguration', 'updateComponentConfiguration_componentConfiguration' - The configuration settings of the component. The value is the escaped
-- JSON of the configuration. For more information about the JSON format,
-- see
-- <https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/working-with-json.html Working with JSON>.
-- You can send a request to @DescribeComponentConfigurationRecommendation@
-- to see the recommended configuration for a component. For the complete
-- format of the component configuration file, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/component-config.html Component Configuration>.
--
-- 'monitor', 'updateComponentConfiguration_monitor' - Indicates whether the application component is monitored.
--
-- 'tier', 'updateComponentConfiguration_tier' - The tier of the application component.
--
-- 'resourceGroupName', 'updateComponentConfiguration_resourceGroupName' - The name of the resource group.
--
-- 'componentName', 'updateComponentConfiguration_componentName' - The name of the component.
newUpdateComponentConfiguration ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'componentName'
  Prelude.Text ->
  UpdateComponentConfiguration
newUpdateComponentConfiguration
  pResourceGroupName_
  pComponentName_ =
    UpdateComponentConfiguration'
      { autoConfigEnabled =
          Prelude.Nothing,
        componentConfiguration = Prelude.Nothing,
        monitor = Prelude.Nothing,
        tier = Prelude.Nothing,
        resourceGroupName = pResourceGroupName_,
        componentName = pComponentName_
      }

-- | Automatically configures the component by applying the recommended
-- configurations.
updateComponentConfiguration_autoConfigEnabled :: Lens.Lens' UpdateComponentConfiguration (Prelude.Maybe Prelude.Bool)
updateComponentConfiguration_autoConfigEnabled = Lens.lens (\UpdateComponentConfiguration' {autoConfigEnabled} -> autoConfigEnabled) (\s@UpdateComponentConfiguration' {} a -> s {autoConfigEnabled = a} :: UpdateComponentConfiguration)

-- | The configuration settings of the component. The value is the escaped
-- JSON of the configuration. For more information about the JSON format,
-- see
-- <https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/working-with-json.html Working with JSON>.
-- You can send a request to @DescribeComponentConfigurationRecommendation@
-- to see the recommended configuration for a component. For the complete
-- format of the component configuration file, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/component-config.html Component Configuration>.
updateComponentConfiguration_componentConfiguration :: Lens.Lens' UpdateComponentConfiguration (Prelude.Maybe Prelude.Text)
updateComponentConfiguration_componentConfiguration = Lens.lens (\UpdateComponentConfiguration' {componentConfiguration} -> componentConfiguration) (\s@UpdateComponentConfiguration' {} a -> s {componentConfiguration = a} :: UpdateComponentConfiguration)

-- | Indicates whether the application component is monitored.
updateComponentConfiguration_monitor :: Lens.Lens' UpdateComponentConfiguration (Prelude.Maybe Prelude.Bool)
updateComponentConfiguration_monitor = Lens.lens (\UpdateComponentConfiguration' {monitor} -> monitor) (\s@UpdateComponentConfiguration' {} a -> s {monitor = a} :: UpdateComponentConfiguration)

-- | The tier of the application component.
updateComponentConfiguration_tier :: Lens.Lens' UpdateComponentConfiguration (Prelude.Maybe Tier)
updateComponentConfiguration_tier = Lens.lens (\UpdateComponentConfiguration' {tier} -> tier) (\s@UpdateComponentConfiguration' {} a -> s {tier = a} :: UpdateComponentConfiguration)

-- | The name of the resource group.
updateComponentConfiguration_resourceGroupName :: Lens.Lens' UpdateComponentConfiguration Prelude.Text
updateComponentConfiguration_resourceGroupName = Lens.lens (\UpdateComponentConfiguration' {resourceGroupName} -> resourceGroupName) (\s@UpdateComponentConfiguration' {} a -> s {resourceGroupName = a} :: UpdateComponentConfiguration)

-- | The name of the component.
updateComponentConfiguration_componentName :: Lens.Lens' UpdateComponentConfiguration Prelude.Text
updateComponentConfiguration_componentName = Lens.lens (\UpdateComponentConfiguration' {componentName} -> componentName) (\s@UpdateComponentConfiguration' {} a -> s {componentName = a} :: UpdateComponentConfiguration)

instance Core.AWSRequest UpdateComponentConfiguration where
  type
    AWSResponse UpdateComponentConfiguration =
      UpdateComponentConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateComponentConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateComponentConfiguration
  where
  hashWithSalt _salt UpdateComponentConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoConfigEnabled
      `Prelude.hashWithSalt` componentConfiguration
      `Prelude.hashWithSalt` monitor
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` componentName

instance Prelude.NFData UpdateComponentConfiguration where
  rnf UpdateComponentConfiguration' {..} =
    Prelude.rnf autoConfigEnabled `Prelude.seq`
      Prelude.rnf componentConfiguration `Prelude.seq`
        Prelude.rnf monitor `Prelude.seq`
          Prelude.rnf tier `Prelude.seq`
            Prelude.rnf resourceGroupName `Prelude.seq`
              Prelude.rnf componentName

instance Data.ToHeaders UpdateComponentConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.UpdateComponentConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateComponentConfiguration where
  toJSON UpdateComponentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoConfigEnabled" Data..=)
              Prelude.<$> autoConfigEnabled,
            ("ComponentConfiguration" Data..=)
              Prelude.<$> componentConfiguration,
            ("Monitor" Data..=) Prelude.<$> monitor,
            ("Tier" Data..=) Prelude.<$> tier,
            Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName),
            Prelude.Just
              ("ComponentName" Data..= componentName)
          ]
      )

instance Data.ToPath UpdateComponentConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateComponentConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateComponentConfigurationResponse' smart constructor.
data UpdateComponentConfigurationResponse = UpdateComponentConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponentConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateComponentConfigurationResponse_httpStatus' - The response's http status code.
newUpdateComponentConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateComponentConfigurationResponse
newUpdateComponentConfigurationResponse pHttpStatus_ =
  UpdateComponentConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateComponentConfigurationResponse_httpStatus :: Lens.Lens' UpdateComponentConfigurationResponse Prelude.Int
updateComponentConfigurationResponse_httpStatus = Lens.lens (\UpdateComponentConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateComponentConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateComponentConfigurationResponse)

instance
  Prelude.NFData
    UpdateComponentConfigurationResponse
  where
  rnf UpdateComponentConfigurationResponse' {..} =
    Prelude.rnf httpStatus

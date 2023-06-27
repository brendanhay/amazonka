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
-- Module      : Amazonka.ApplicationInsights.DescribeComponentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the monitoring configuration of the component.
module Amazonka.ApplicationInsights.DescribeComponentConfiguration
  ( -- * Creating a Request
    DescribeComponentConfiguration (..),
    newDescribeComponentConfiguration,

    -- * Request Lenses
    describeComponentConfiguration_resourceGroupName,
    describeComponentConfiguration_componentName,

    -- * Destructuring the Response
    DescribeComponentConfigurationResponse (..),
    newDescribeComponentConfigurationResponse,

    -- * Response Lenses
    describeComponentConfigurationResponse_componentConfiguration,
    describeComponentConfigurationResponse_monitor,
    describeComponentConfigurationResponse_tier,
    describeComponentConfigurationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeComponentConfiguration' smart constructor.
data DescribeComponentConfiguration = DescribeComponentConfiguration'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComponentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupName', 'describeComponentConfiguration_resourceGroupName' - The name of the resource group.
--
-- 'componentName', 'describeComponentConfiguration_componentName' - The name of the component.
newDescribeComponentConfiguration ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'componentName'
  Prelude.Text ->
  DescribeComponentConfiguration
newDescribeComponentConfiguration
  pResourceGroupName_
  pComponentName_ =
    DescribeComponentConfiguration'
      { resourceGroupName =
          pResourceGroupName_,
        componentName = pComponentName_
      }

-- | The name of the resource group.
describeComponentConfiguration_resourceGroupName :: Lens.Lens' DescribeComponentConfiguration Prelude.Text
describeComponentConfiguration_resourceGroupName = Lens.lens (\DescribeComponentConfiguration' {resourceGroupName} -> resourceGroupName) (\s@DescribeComponentConfiguration' {} a -> s {resourceGroupName = a} :: DescribeComponentConfiguration)

-- | The name of the component.
describeComponentConfiguration_componentName :: Lens.Lens' DescribeComponentConfiguration Prelude.Text
describeComponentConfiguration_componentName = Lens.lens (\DescribeComponentConfiguration' {componentName} -> componentName) (\s@DescribeComponentConfiguration' {} a -> s {componentName = a} :: DescribeComponentConfiguration)

instance
  Core.AWSRequest
    DescribeComponentConfiguration
  where
  type
    AWSResponse DescribeComponentConfiguration =
      DescribeComponentConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComponentConfigurationResponse'
            Prelude.<$> (x Data..?> "ComponentConfiguration")
            Prelude.<*> (x Data..?> "Monitor")
            Prelude.<*> (x Data..?> "Tier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeComponentConfiguration
  where
  hashWithSalt
    _salt
    DescribeComponentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` resourceGroupName
        `Prelude.hashWithSalt` componentName

instance
  Prelude.NFData
    DescribeComponentConfiguration
  where
  rnf DescribeComponentConfiguration' {..} =
    Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf componentName

instance
  Data.ToHeaders
    DescribeComponentConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.DescribeComponentConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeComponentConfiguration where
  toJSON DescribeComponentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName),
            Prelude.Just
              ("ComponentName" Data..= componentName)
          ]
      )

instance Data.ToPath DescribeComponentConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeComponentConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeComponentConfigurationResponse' smart constructor.
data DescribeComponentConfigurationResponse = DescribeComponentConfigurationResponse'
  { -- | The configuration settings of the component. The value is the escaped
    -- JSON of the configuration.
    componentConfiguration :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the application component is monitored.
    monitor :: Prelude.Maybe Prelude.Bool,
    -- | The tier of the application component. Supported tiers include
    -- @DOT_NET_CORE@, @DOT_NET_WORKER@, @DOT_NET_WEB@, @SQL_SERVER@, and
    -- @DEFAULT@
    tier :: Prelude.Maybe Tier,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComponentConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentConfiguration', 'describeComponentConfigurationResponse_componentConfiguration' - The configuration settings of the component. The value is the escaped
-- JSON of the configuration.
--
-- 'monitor', 'describeComponentConfigurationResponse_monitor' - Indicates whether the application component is monitored.
--
-- 'tier', 'describeComponentConfigurationResponse_tier' - The tier of the application component. Supported tiers include
-- @DOT_NET_CORE@, @DOT_NET_WORKER@, @DOT_NET_WEB@, @SQL_SERVER@, and
-- @DEFAULT@
--
-- 'httpStatus', 'describeComponentConfigurationResponse_httpStatus' - The response's http status code.
newDescribeComponentConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeComponentConfigurationResponse
newDescribeComponentConfigurationResponse
  pHttpStatus_ =
    DescribeComponentConfigurationResponse'
      { componentConfiguration =
          Prelude.Nothing,
        monitor = Prelude.Nothing,
        tier = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The configuration settings of the component. The value is the escaped
-- JSON of the configuration.
describeComponentConfigurationResponse_componentConfiguration :: Lens.Lens' DescribeComponentConfigurationResponse (Prelude.Maybe Prelude.Text)
describeComponentConfigurationResponse_componentConfiguration = Lens.lens (\DescribeComponentConfigurationResponse' {componentConfiguration} -> componentConfiguration) (\s@DescribeComponentConfigurationResponse' {} a -> s {componentConfiguration = a} :: DescribeComponentConfigurationResponse)

-- | Indicates whether the application component is monitored.
describeComponentConfigurationResponse_monitor :: Lens.Lens' DescribeComponentConfigurationResponse (Prelude.Maybe Prelude.Bool)
describeComponentConfigurationResponse_monitor = Lens.lens (\DescribeComponentConfigurationResponse' {monitor} -> monitor) (\s@DescribeComponentConfigurationResponse' {} a -> s {monitor = a} :: DescribeComponentConfigurationResponse)

-- | The tier of the application component. Supported tiers include
-- @DOT_NET_CORE@, @DOT_NET_WORKER@, @DOT_NET_WEB@, @SQL_SERVER@, and
-- @DEFAULT@
describeComponentConfigurationResponse_tier :: Lens.Lens' DescribeComponentConfigurationResponse (Prelude.Maybe Tier)
describeComponentConfigurationResponse_tier = Lens.lens (\DescribeComponentConfigurationResponse' {tier} -> tier) (\s@DescribeComponentConfigurationResponse' {} a -> s {tier = a} :: DescribeComponentConfigurationResponse)

-- | The response's http status code.
describeComponentConfigurationResponse_httpStatus :: Lens.Lens' DescribeComponentConfigurationResponse Prelude.Int
describeComponentConfigurationResponse_httpStatus = Lens.lens (\DescribeComponentConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeComponentConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeComponentConfigurationResponse)

instance
  Prelude.NFData
    DescribeComponentConfigurationResponse
  where
  rnf DescribeComponentConfigurationResponse' {..} =
    Prelude.rnf componentConfiguration
      `Prelude.seq` Prelude.rnf monitor
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.IoTWireless.UpdateNetworkAnalyzerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update network analyzer configuration.
module Amazonka.IoTWireless.UpdateNetworkAnalyzerConfiguration
  ( -- * Creating a Request
    UpdateNetworkAnalyzerConfiguration (..),
    newUpdateNetworkAnalyzerConfiguration,

    -- * Request Lenses
    updateNetworkAnalyzerConfiguration_description,
    updateNetworkAnalyzerConfiguration_multicastGroupsToAdd,
    updateNetworkAnalyzerConfiguration_multicastGroupsToRemove,
    updateNetworkAnalyzerConfiguration_traceContent,
    updateNetworkAnalyzerConfiguration_wirelessDevicesToAdd,
    updateNetworkAnalyzerConfiguration_wirelessDevicesToRemove,
    updateNetworkAnalyzerConfiguration_wirelessGatewaysToAdd,
    updateNetworkAnalyzerConfiguration_wirelessGatewaysToRemove,
    updateNetworkAnalyzerConfiguration_configurationName,

    -- * Destructuring the Response
    UpdateNetworkAnalyzerConfigurationResponse (..),
    newUpdateNetworkAnalyzerConfigurationResponse,

    -- * Response Lenses
    updateNetworkAnalyzerConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNetworkAnalyzerConfiguration' smart constructor.
data UpdateNetworkAnalyzerConfiguration = UpdateNetworkAnalyzerConfiguration'
  { description :: Prelude.Maybe Prelude.Text,
    -- | Multicast group resources to add to the network analyzer configuration.
    -- Provide the @MulticastGroupId@ of the resource to add in the input
    -- array.
    multicastGroupsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | Multicast group resources to remove from the network analyzer
    -- configuration. Provide the @MulticastGroupId@ of the resource to remove
    -- in the input array.
    multicastGroupsToRemove :: Prelude.Maybe [Prelude.Text],
    traceContent :: Prelude.Maybe TraceContent,
    -- | Wireless device resources to add to the network analyzer configuration.
    -- Provide the @WirelessDeviceId@ of the resource to add in the input
    -- array.
    wirelessDevicesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | Wireless device resources to remove from the network analyzer
    -- configuration. Provide the @WirelessDeviceId@ of the resources to remove
    -- in the input array.
    wirelessDevicesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | Wireless gateway resources to add to the network analyzer configuration.
    -- Provide the @WirelessGatewayId@ of the resource to add in the input
    -- array.
    wirelessGatewaysToAdd :: Prelude.Maybe [Prelude.Text],
    -- | Wireless gateway resources to remove from the network analyzer
    -- configuration. Provide the @WirelessGatewayId@ of the resources to
    -- remove in the input array.
    wirelessGatewaysToRemove :: Prelude.Maybe [Prelude.Text],
    configurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkAnalyzerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateNetworkAnalyzerConfiguration_description' - Undocumented member.
--
-- 'multicastGroupsToAdd', 'updateNetworkAnalyzerConfiguration_multicastGroupsToAdd' - Multicast group resources to add to the network analyzer configuration.
-- Provide the @MulticastGroupId@ of the resource to add in the input
-- array.
--
-- 'multicastGroupsToRemove', 'updateNetworkAnalyzerConfiguration_multicastGroupsToRemove' - Multicast group resources to remove from the network analyzer
-- configuration. Provide the @MulticastGroupId@ of the resource to remove
-- in the input array.
--
-- 'traceContent', 'updateNetworkAnalyzerConfiguration_traceContent' - Undocumented member.
--
-- 'wirelessDevicesToAdd', 'updateNetworkAnalyzerConfiguration_wirelessDevicesToAdd' - Wireless device resources to add to the network analyzer configuration.
-- Provide the @WirelessDeviceId@ of the resource to add in the input
-- array.
--
-- 'wirelessDevicesToRemove', 'updateNetworkAnalyzerConfiguration_wirelessDevicesToRemove' - Wireless device resources to remove from the network analyzer
-- configuration. Provide the @WirelessDeviceId@ of the resources to remove
-- in the input array.
--
-- 'wirelessGatewaysToAdd', 'updateNetworkAnalyzerConfiguration_wirelessGatewaysToAdd' - Wireless gateway resources to add to the network analyzer configuration.
-- Provide the @WirelessGatewayId@ of the resource to add in the input
-- array.
--
-- 'wirelessGatewaysToRemove', 'updateNetworkAnalyzerConfiguration_wirelessGatewaysToRemove' - Wireless gateway resources to remove from the network analyzer
-- configuration. Provide the @WirelessGatewayId@ of the resources to
-- remove in the input array.
--
-- 'configurationName', 'updateNetworkAnalyzerConfiguration_configurationName' - Undocumented member.
newUpdateNetworkAnalyzerConfiguration ::
  -- | 'configurationName'
  Prelude.Text ->
  UpdateNetworkAnalyzerConfiguration
newUpdateNetworkAnalyzerConfiguration
  pConfigurationName_ =
    UpdateNetworkAnalyzerConfiguration'
      { description =
          Prelude.Nothing,
        multicastGroupsToAdd = Prelude.Nothing,
        multicastGroupsToRemove =
          Prelude.Nothing,
        traceContent = Prelude.Nothing,
        wirelessDevicesToAdd = Prelude.Nothing,
        wirelessDevicesToRemove =
          Prelude.Nothing,
        wirelessGatewaysToAdd = Prelude.Nothing,
        wirelessGatewaysToRemove =
          Prelude.Nothing,
        configurationName = pConfigurationName_
      }

-- | Undocumented member.
updateNetworkAnalyzerConfiguration_description :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe Prelude.Text)
updateNetworkAnalyzerConfiguration_description = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {description} -> description) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {description = a} :: UpdateNetworkAnalyzerConfiguration)

-- | Multicast group resources to add to the network analyzer configuration.
-- Provide the @MulticastGroupId@ of the resource to add in the input
-- array.
updateNetworkAnalyzerConfiguration_multicastGroupsToAdd :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
updateNetworkAnalyzerConfiguration_multicastGroupsToAdd = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {multicastGroupsToAdd} -> multicastGroupsToAdd) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {multicastGroupsToAdd = a} :: UpdateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Multicast group resources to remove from the network analyzer
-- configuration. Provide the @MulticastGroupId@ of the resource to remove
-- in the input array.
updateNetworkAnalyzerConfiguration_multicastGroupsToRemove :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
updateNetworkAnalyzerConfiguration_multicastGroupsToRemove = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {multicastGroupsToRemove} -> multicastGroupsToRemove) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {multicastGroupsToRemove = a} :: UpdateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateNetworkAnalyzerConfiguration_traceContent :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe TraceContent)
updateNetworkAnalyzerConfiguration_traceContent = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {traceContent} -> traceContent) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {traceContent = a} :: UpdateNetworkAnalyzerConfiguration)

-- | Wireless device resources to add to the network analyzer configuration.
-- Provide the @WirelessDeviceId@ of the resource to add in the input
-- array.
updateNetworkAnalyzerConfiguration_wirelessDevicesToAdd :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
updateNetworkAnalyzerConfiguration_wirelessDevicesToAdd = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {wirelessDevicesToAdd} -> wirelessDevicesToAdd) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {wirelessDevicesToAdd = a} :: UpdateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Wireless device resources to remove from the network analyzer
-- configuration. Provide the @WirelessDeviceId@ of the resources to remove
-- in the input array.
updateNetworkAnalyzerConfiguration_wirelessDevicesToRemove :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
updateNetworkAnalyzerConfiguration_wirelessDevicesToRemove = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {wirelessDevicesToRemove} -> wirelessDevicesToRemove) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {wirelessDevicesToRemove = a} :: UpdateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Wireless gateway resources to add to the network analyzer configuration.
-- Provide the @WirelessGatewayId@ of the resource to add in the input
-- array.
updateNetworkAnalyzerConfiguration_wirelessGatewaysToAdd :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
updateNetworkAnalyzerConfiguration_wirelessGatewaysToAdd = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {wirelessGatewaysToAdd} -> wirelessGatewaysToAdd) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {wirelessGatewaysToAdd = a} :: UpdateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Wireless gateway resources to remove from the network analyzer
-- configuration. Provide the @WirelessGatewayId@ of the resources to
-- remove in the input array.
updateNetworkAnalyzerConfiguration_wirelessGatewaysToRemove :: Lens.Lens' UpdateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
updateNetworkAnalyzerConfiguration_wirelessGatewaysToRemove = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {wirelessGatewaysToRemove} -> wirelessGatewaysToRemove) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {wirelessGatewaysToRemove = a} :: UpdateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateNetworkAnalyzerConfiguration_configurationName :: Lens.Lens' UpdateNetworkAnalyzerConfiguration Prelude.Text
updateNetworkAnalyzerConfiguration_configurationName = Lens.lens (\UpdateNetworkAnalyzerConfiguration' {configurationName} -> configurationName) (\s@UpdateNetworkAnalyzerConfiguration' {} a -> s {configurationName = a} :: UpdateNetworkAnalyzerConfiguration)

instance
  Core.AWSRequest
    UpdateNetworkAnalyzerConfiguration
  where
  type
    AWSResponse UpdateNetworkAnalyzerConfiguration =
      UpdateNetworkAnalyzerConfigurationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNetworkAnalyzerConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateNetworkAnalyzerConfiguration
  where
  hashWithSalt
    _salt
    UpdateNetworkAnalyzerConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` multicastGroupsToAdd
        `Prelude.hashWithSalt` multicastGroupsToRemove
        `Prelude.hashWithSalt` traceContent
        `Prelude.hashWithSalt` wirelessDevicesToAdd
        `Prelude.hashWithSalt` wirelessDevicesToRemove
        `Prelude.hashWithSalt` wirelessGatewaysToAdd
        `Prelude.hashWithSalt` wirelessGatewaysToRemove
        `Prelude.hashWithSalt` configurationName

instance
  Prelude.NFData
    UpdateNetworkAnalyzerConfiguration
  where
  rnf UpdateNetworkAnalyzerConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf multicastGroupsToAdd
      `Prelude.seq` Prelude.rnf multicastGroupsToRemove
      `Prelude.seq` Prelude.rnf traceContent
      `Prelude.seq` Prelude.rnf wirelessDevicesToAdd
      `Prelude.seq` Prelude.rnf wirelessDevicesToRemove
      `Prelude.seq` Prelude.rnf wirelessGatewaysToAdd
      `Prelude.seq` Prelude.rnf wirelessGatewaysToRemove
      `Prelude.seq` Prelude.rnf configurationName

instance
  Data.ToHeaders
    UpdateNetworkAnalyzerConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    UpdateNetworkAnalyzerConfiguration
  where
  toJSON UpdateNetworkAnalyzerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("MulticastGroupsToAdd" Data..=)
              Prelude.<$> multicastGroupsToAdd,
            ("MulticastGroupsToRemove" Data..=)
              Prelude.<$> multicastGroupsToRemove,
            ("TraceContent" Data..=) Prelude.<$> traceContent,
            ("WirelessDevicesToAdd" Data..=)
              Prelude.<$> wirelessDevicesToAdd,
            ("WirelessDevicesToRemove" Data..=)
              Prelude.<$> wirelessDevicesToRemove,
            ("WirelessGatewaysToAdd" Data..=)
              Prelude.<$> wirelessGatewaysToAdd,
            ("WirelessGatewaysToRemove" Data..=)
              Prelude.<$> wirelessGatewaysToRemove
          ]
      )

instance
  Data.ToPath
    UpdateNetworkAnalyzerConfiguration
  where
  toPath UpdateNetworkAnalyzerConfiguration' {..} =
    Prelude.mconcat
      [ "/network-analyzer-configurations/",
        Data.toBS configurationName
      ]

instance
  Data.ToQuery
    UpdateNetworkAnalyzerConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNetworkAnalyzerConfigurationResponse' smart constructor.
data UpdateNetworkAnalyzerConfigurationResponse = UpdateNetworkAnalyzerConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkAnalyzerConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNetworkAnalyzerConfigurationResponse_httpStatus' - The response's http status code.
newUpdateNetworkAnalyzerConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNetworkAnalyzerConfigurationResponse
newUpdateNetworkAnalyzerConfigurationResponse
  pHttpStatus_ =
    UpdateNetworkAnalyzerConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateNetworkAnalyzerConfigurationResponse_httpStatus :: Lens.Lens' UpdateNetworkAnalyzerConfigurationResponse Prelude.Int
updateNetworkAnalyzerConfigurationResponse_httpStatus = Lens.lens (\UpdateNetworkAnalyzerConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateNetworkAnalyzerConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateNetworkAnalyzerConfigurationResponse)

instance
  Prelude.NFData
    UpdateNetworkAnalyzerConfigurationResponse
  where
  rnf UpdateNetworkAnalyzerConfigurationResponse' {..} =
    Prelude.rnf httpStatus

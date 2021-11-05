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
-- Module      : Network.AWS.AppFlow.Types.ConnectorConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.ConnectorConfiguration where

import Network.AWS.AppFlow.Types.ConnectorMetadata
import Network.AWS.AppFlow.Types.ConnectorType
import Network.AWS.AppFlow.Types.ScheduleFrequencyType
import Network.AWS.AppFlow.Types.TriggerType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration settings related to a given connector.
--
-- /See:/ 'newConnectorConfiguration' smart constructor.
data ConnectorConfiguration = ConnectorConfiguration'
  { -- | Specifies if PrivateLink is enabled for that connector.
    isPrivateLinkEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the supported trigger types for the flow.
    supportedTriggerTypes :: Prelude.Maybe [TriggerType],
    -- | Specifies whether the connector can be used as a source.
    canUseAsSource :: Prelude.Maybe Prelude.Bool,
    -- | Specifies connector-specific metadata such as @oAuthScopes@,
    -- @supportedRegions@, @privateLinkServiceUrl@, and so on.
    connectorMetadata :: Prelude.Maybe ConnectorMetadata,
    -- | Specifies whether the connector can be used as a destination.
    canUseAsDestination :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the supported flow frequency for that connector.
    supportedSchedulingFrequencies :: Prelude.Maybe [ScheduleFrequencyType],
    -- | Lists the connectors that are available for use as destinations.
    supportedDestinationConnectors :: Prelude.Maybe [ConnectorType],
    -- | Specifies if a PrivateLink endpoint URL is required.
    isPrivateLinkEndpointUrlRequired :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isPrivateLinkEnabled', 'connectorConfiguration_isPrivateLinkEnabled' - Specifies if PrivateLink is enabled for that connector.
--
-- 'supportedTriggerTypes', 'connectorConfiguration_supportedTriggerTypes' - Specifies the supported trigger types for the flow.
--
-- 'canUseAsSource', 'connectorConfiguration_canUseAsSource' - Specifies whether the connector can be used as a source.
--
-- 'connectorMetadata', 'connectorConfiguration_connectorMetadata' - Specifies connector-specific metadata such as @oAuthScopes@,
-- @supportedRegions@, @privateLinkServiceUrl@, and so on.
--
-- 'canUseAsDestination', 'connectorConfiguration_canUseAsDestination' - Specifies whether the connector can be used as a destination.
--
-- 'supportedSchedulingFrequencies', 'connectorConfiguration_supportedSchedulingFrequencies' - Specifies the supported flow frequency for that connector.
--
-- 'supportedDestinationConnectors', 'connectorConfiguration_supportedDestinationConnectors' - Lists the connectors that are available for use as destinations.
--
-- 'isPrivateLinkEndpointUrlRequired', 'connectorConfiguration_isPrivateLinkEndpointUrlRequired' - Specifies if a PrivateLink endpoint URL is required.
newConnectorConfiguration ::
  ConnectorConfiguration
newConnectorConfiguration =
  ConnectorConfiguration'
    { isPrivateLinkEnabled =
        Prelude.Nothing,
      supportedTriggerTypes = Prelude.Nothing,
      canUseAsSource = Prelude.Nothing,
      connectorMetadata = Prelude.Nothing,
      canUseAsDestination = Prelude.Nothing,
      supportedSchedulingFrequencies = Prelude.Nothing,
      supportedDestinationConnectors = Prelude.Nothing,
      isPrivateLinkEndpointUrlRequired = Prelude.Nothing
    }

-- | Specifies if PrivateLink is enabled for that connector.
connectorConfiguration_isPrivateLinkEnabled :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe Prelude.Bool)
connectorConfiguration_isPrivateLinkEnabled = Lens.lens (\ConnectorConfiguration' {isPrivateLinkEnabled} -> isPrivateLinkEnabled) (\s@ConnectorConfiguration' {} a -> s {isPrivateLinkEnabled = a} :: ConnectorConfiguration)

-- | Specifies the supported trigger types for the flow.
connectorConfiguration_supportedTriggerTypes :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe [TriggerType])
connectorConfiguration_supportedTriggerTypes = Lens.lens (\ConnectorConfiguration' {supportedTriggerTypes} -> supportedTriggerTypes) (\s@ConnectorConfiguration' {} a -> s {supportedTriggerTypes = a} :: ConnectorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the connector can be used as a source.
connectorConfiguration_canUseAsSource :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe Prelude.Bool)
connectorConfiguration_canUseAsSource = Lens.lens (\ConnectorConfiguration' {canUseAsSource} -> canUseAsSource) (\s@ConnectorConfiguration' {} a -> s {canUseAsSource = a} :: ConnectorConfiguration)

-- | Specifies connector-specific metadata such as @oAuthScopes@,
-- @supportedRegions@, @privateLinkServiceUrl@, and so on.
connectorConfiguration_connectorMetadata :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe ConnectorMetadata)
connectorConfiguration_connectorMetadata = Lens.lens (\ConnectorConfiguration' {connectorMetadata} -> connectorMetadata) (\s@ConnectorConfiguration' {} a -> s {connectorMetadata = a} :: ConnectorConfiguration)

-- | Specifies whether the connector can be used as a destination.
connectorConfiguration_canUseAsDestination :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe Prelude.Bool)
connectorConfiguration_canUseAsDestination = Lens.lens (\ConnectorConfiguration' {canUseAsDestination} -> canUseAsDestination) (\s@ConnectorConfiguration' {} a -> s {canUseAsDestination = a} :: ConnectorConfiguration)

-- | Specifies the supported flow frequency for that connector.
connectorConfiguration_supportedSchedulingFrequencies :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe [ScheduleFrequencyType])
connectorConfiguration_supportedSchedulingFrequencies = Lens.lens (\ConnectorConfiguration' {supportedSchedulingFrequencies} -> supportedSchedulingFrequencies) (\s@ConnectorConfiguration' {} a -> s {supportedSchedulingFrequencies = a} :: ConnectorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Lists the connectors that are available for use as destinations.
connectorConfiguration_supportedDestinationConnectors :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe [ConnectorType])
connectorConfiguration_supportedDestinationConnectors = Lens.lens (\ConnectorConfiguration' {supportedDestinationConnectors} -> supportedDestinationConnectors) (\s@ConnectorConfiguration' {} a -> s {supportedDestinationConnectors = a} :: ConnectorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies if a PrivateLink endpoint URL is required.
connectorConfiguration_isPrivateLinkEndpointUrlRequired :: Lens.Lens' ConnectorConfiguration (Prelude.Maybe Prelude.Bool)
connectorConfiguration_isPrivateLinkEndpointUrlRequired = Lens.lens (\ConnectorConfiguration' {isPrivateLinkEndpointUrlRequired} -> isPrivateLinkEndpointUrlRequired) (\s@ConnectorConfiguration' {} a -> s {isPrivateLinkEndpointUrlRequired = a} :: ConnectorConfiguration)

instance Core.FromJSON ConnectorConfiguration where
  parseJSON =
    Core.withObject
      "ConnectorConfiguration"
      ( \x ->
          ConnectorConfiguration'
            Prelude.<$> (x Core..:? "isPrivateLinkEnabled")
            Prelude.<*> ( x Core..:? "supportedTriggerTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "canUseAsSource")
            Prelude.<*> (x Core..:? "connectorMetadata")
            Prelude.<*> (x Core..:? "canUseAsDestination")
            Prelude.<*> ( x Core..:? "supportedSchedulingFrequencies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "supportedDestinationConnectors"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "isPrivateLinkEndpointUrlRequired")
      )

instance Prelude.Hashable ConnectorConfiguration

instance Prelude.NFData ConnectorConfiguration

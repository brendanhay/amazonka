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
-- Module      : Network.AWS.Greengrass.Types.Connector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Connector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a connector. Connectors run on the Greengrass core and
-- contain built-in integration with local infrastructure, device
-- protocols, AWS, and other cloud services.
--
-- /See:/ 'newConnector' smart constructor.
data Connector = Connector'
  { -- | The parameters or configuration that the connector uses.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The ARN of the connector.
    connectorArn :: Core.Text,
    -- | A descriptive or arbitrary ID for the connector. This value must be
    -- unique within the connector definition version. Max length is 128
    -- characters with pattern [a-zA-Z0-9:_-]+.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Connector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'connector_parameters' - The parameters or configuration that the connector uses.
--
-- 'connectorArn', 'connector_connectorArn' - The ARN of the connector.
--
-- 'id', 'connector_id' - A descriptive or arbitrary ID for the connector. This value must be
-- unique within the connector definition version. Max length is 128
-- characters with pattern [a-zA-Z0-9:_-]+.
newConnector ::
  -- | 'connectorArn'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  Connector
newConnector pConnectorArn_ pId_ =
  Connector'
    { parameters = Core.Nothing,
      connectorArn = pConnectorArn_,
      id = pId_
    }

-- | The parameters or configuration that the connector uses.
connector_parameters :: Lens.Lens' Connector (Core.Maybe (Core.HashMap Core.Text Core.Text))
connector_parameters = Lens.lens (\Connector' {parameters} -> parameters) (\s@Connector' {} a -> s {parameters = a} :: Connector) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the connector.
connector_connectorArn :: Lens.Lens' Connector Core.Text
connector_connectorArn = Lens.lens (\Connector' {connectorArn} -> connectorArn) (\s@Connector' {} a -> s {connectorArn = a} :: Connector)

-- | A descriptive or arbitrary ID for the connector. This value must be
-- unique within the connector definition version. Max length is 128
-- characters with pattern [a-zA-Z0-9:_-]+.
connector_id :: Lens.Lens' Connector Core.Text
connector_id = Lens.lens (\Connector' {id} -> id) (\s@Connector' {} a -> s {id = a} :: Connector)

instance Core.FromJSON Connector where
  parseJSON =
    Core.withObject
      "Connector"
      ( \x ->
          Connector'
            Core.<$> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..: "ConnectorArn")
            Core.<*> (x Core..: "Id")
      )

instance Core.Hashable Connector

instance Core.NFData Connector

instance Core.ToJSON Connector where
  toJSON Connector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("ConnectorArn" Core..= connectorArn),
            Core.Just ("Id" Core..= id)
          ]
      )

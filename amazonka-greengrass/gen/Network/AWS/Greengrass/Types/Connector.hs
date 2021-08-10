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
import qualified Network.AWS.Prelude as Prelude

-- | Information about a connector. Connectors run on the Greengrass core and
-- contain built-in integration with local infrastructure, device
-- protocols, AWS, and other cloud services.
--
-- /See:/ 'newConnector' smart constructor.
data Connector = Connector'
  { -- | The parameters or configuration that the connector uses.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the connector.
    connectorArn :: Prelude.Text,
    -- | A descriptive or arbitrary ID for the connector. This value must be
    -- unique within the connector definition version. Max length is 128
    -- characters with pattern [a-zA-Z0-9:_-]+.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  Connector
newConnector pConnectorArn_ pId_ =
  Connector'
    { parameters = Prelude.Nothing,
      connectorArn = pConnectorArn_,
      id = pId_
    }

-- | The parameters or configuration that the connector uses.
connector_parameters :: Lens.Lens' Connector (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
connector_parameters = Lens.lens (\Connector' {parameters} -> parameters) (\s@Connector' {} a -> s {parameters = a} :: Connector) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of the connector.
connector_connectorArn :: Lens.Lens' Connector Prelude.Text
connector_connectorArn = Lens.lens (\Connector' {connectorArn} -> connectorArn) (\s@Connector' {} a -> s {connectorArn = a} :: Connector)

-- | A descriptive or arbitrary ID for the connector. This value must be
-- unique within the connector definition version. Max length is 128
-- characters with pattern [a-zA-Z0-9:_-]+.
connector_id :: Lens.Lens' Connector Prelude.Text
connector_id = Lens.lens (\Connector' {id} -> id) (\s@Connector' {} a -> s {id = a} :: Connector)

instance Core.FromJSON Connector where
  parseJSON =
    Core.withObject
      "Connector"
      ( \x ->
          Connector'
            Prelude.<$> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "ConnectorArn")
            Prelude.<*> (x Core..: "Id")
      )

instance Prelude.Hashable Connector

instance Prelude.NFData Connector

instance Core.ToJSON Connector where
  toJSON Connector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("ConnectorArn" Core..= connectorArn),
            Prelude.Just ("Id" Core..= id)
          ]
      )

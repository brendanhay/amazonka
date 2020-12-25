{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Connector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Connector
  ( Connector (..),

    -- * Smart constructor
    mkConnector,

    -- * Lenses
    cfConnectorArn,
    cfId,
    cfParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a connector. Connectors run on the Greengrass core and contain built-in integration with local infrastructure, device protocols, AWS, and other cloud services.
--
-- /See:/ 'mkConnector' smart constructor.
data Connector = Connector'
  { -- | The ARN of the connector.
    connectorArn :: Core.Text,
    -- | A descriptive or arbitrary ID for the connector. This value must be unique within the connector definition version. Max length is 128 characters with pattern [a-zA-Z0-9:_-]+.
    id :: Core.Text,
    -- | The parameters or configuration that the connector uses.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Connector' value with any optional fields omitted.
mkConnector ::
  -- | 'connectorArn'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  Connector
mkConnector connectorArn id =
  Connector' {connectorArn, id, parameters = Core.Nothing}

-- | The ARN of the connector.
--
-- /Note:/ Consider using 'connectorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfConnectorArn :: Lens.Lens' Connector Core.Text
cfConnectorArn = Lens.field @"connectorArn"
{-# DEPRECATED cfConnectorArn "Use generic-lens or generic-optics with 'connectorArn' instead." #-}

-- | A descriptive or arbitrary ID for the connector. This value must be unique within the connector definition version. Max length is 128 characters with pattern [a-zA-Z0-9:_-]+.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' Connector Core.Text
cfId = Lens.field @"id"
{-# DEPRECATED cfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The parameters or configuration that the connector uses.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfParameters :: Lens.Lens' Connector (Core.Maybe (Core.HashMap Core.Text Core.Text))
cfParameters = Lens.field @"parameters"
{-# DEPRECATED cfParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON Connector where
  toJSON Connector {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConnectorArn" Core..= connectorArn),
            Core.Just ("Id" Core..= id),
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.FromJSON Connector where
  parseJSON =
    Core.withObject "Connector" Core.$
      \x ->
        Connector'
          Core.<$> (x Core..: "ConnectorArn")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..:? "Parameters")

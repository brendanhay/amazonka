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
    cfConnectorARN,
    cfParameters,
    cfId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a connector. Connectors run on the Greengrass core and contain built-in integration with local infrastructure, device protocols, AWS, and other cloud services.
--
-- /See:/ 'mkConnector' smart constructor.
data Connector = Connector'
  { -- | The ARN of the connector.
    connectorARN :: Lude.Text,
    -- | The parameters or configuration that the connector uses.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A descriptive or arbitrary ID for the connector. This value must be unique within the connector definition version. Max length is 128 characters with pattern [a-zA-Z0-9:_-]+.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Connector' with the minimum fields required to make a request.
--
-- * 'connectorARN' - The ARN of the connector.
-- * 'parameters' - The parameters or configuration that the connector uses.
-- * 'id' - A descriptive or arbitrary ID for the connector. This value must be unique within the connector definition version. Max length is 128 characters with pattern [a-zA-Z0-9:_-]+.
mkConnector ::
  -- | 'connectorARN'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  Connector
mkConnector pConnectorARN_ pId_ =
  Connector'
    { connectorARN = pConnectorARN_,
      parameters = Lude.Nothing,
      id = pId_
    }

-- | The ARN of the connector.
--
-- /Note:/ Consider using 'connectorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfConnectorARN :: Lens.Lens' Connector Lude.Text
cfConnectorARN = Lens.lens (connectorARN :: Connector -> Lude.Text) (\s a -> s {connectorARN = a} :: Connector)
{-# DEPRECATED cfConnectorARN "Use generic-lens or generic-optics with 'connectorARN' instead." #-}

-- | The parameters or configuration that the connector uses.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfParameters :: Lens.Lens' Connector (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfParameters = Lens.lens (parameters :: Connector -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: Connector)
{-# DEPRECATED cfParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A descriptive or arbitrary ID for the connector. This value must be unique within the connector definition version. Max length is 128 characters with pattern [a-zA-Z0-9:_-]+.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' Connector Lude.Text
cfId = Lens.lens (id :: Connector -> Lude.Text) (\s a -> s {id = a} :: Connector)
{-# DEPRECATED cfId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Connector where
  parseJSON =
    Lude.withObject
      "Connector"
      ( \x ->
          Connector'
            Lude.<$> (x Lude..: "ConnectorArn")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Id")
      )

instance Lude.ToJSON Connector where
  toJSON Connector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConnectorArn" Lude..= connectorARN),
            ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Id" Lude..= id)
          ]
      )

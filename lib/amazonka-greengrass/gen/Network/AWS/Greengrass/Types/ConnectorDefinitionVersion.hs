{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
  ( ConnectorDefinitionVersion (..),

    -- * Smart constructor
    mkConnectorDefinitionVersion,

    -- * Lenses
    cdvConnectors,
  )
where

import Network.AWS.Greengrass.Types.Connector
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the connector definition version, which is a container for connectors.
--
-- /See:/ 'mkConnectorDefinitionVersion' smart constructor.
newtype ConnectorDefinitionVersion = ConnectorDefinitionVersion'
  { connectors ::
      Lude.Maybe [Connector]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectorDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'connectors' - A list of references to connectors in this version, with their corresponding configuration settings.
mkConnectorDefinitionVersion ::
  ConnectorDefinitionVersion
mkConnectorDefinitionVersion =
  ConnectorDefinitionVersion' {connectors = Lude.Nothing}

-- | A list of references to connectors in this version, with their corresponding configuration settings.
--
-- /Note:/ Consider using 'connectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvConnectors :: Lens.Lens' ConnectorDefinitionVersion (Lude.Maybe [Connector])
cdvConnectors = Lens.lens (connectors :: ConnectorDefinitionVersion -> Lude.Maybe [Connector]) (\s a -> s {connectors = a} :: ConnectorDefinitionVersion)
{-# DEPRECATED cdvConnectors "Use generic-lens or generic-optics with 'connectors' instead." #-}

instance Lude.FromJSON ConnectorDefinitionVersion where
  parseJSON =
    Lude.withObject
      "ConnectorDefinitionVersion"
      ( \x ->
          ConnectorDefinitionVersion'
            Lude.<$> (x Lude..:? "Connectors" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ConnectorDefinitionVersion where
  toJSON ConnectorDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Connectors" Lude..=) Lude.<$> connectors])

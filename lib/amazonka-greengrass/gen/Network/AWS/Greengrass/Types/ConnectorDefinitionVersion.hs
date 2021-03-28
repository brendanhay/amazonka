{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
  ( ConnectorDefinitionVersion (..)
  -- * Smart constructor
  , mkConnectorDefinitionVersion
  -- * Lenses
  , cdvConnectors
  ) where

import qualified Network.AWS.Greengrass.Types.Connector as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the connector definition version, which is a container for connectors.
--
-- /See:/ 'mkConnectorDefinitionVersion' smart constructor.
newtype ConnectorDefinitionVersion = ConnectorDefinitionVersion'
  { connectors :: Core.Maybe [Types.Connector]
    -- ^ A list of references to connectors in this version, with their corresponding configuration settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectorDefinitionVersion' value with any optional fields omitted.
mkConnectorDefinitionVersion
    :: ConnectorDefinitionVersion
mkConnectorDefinitionVersion
  = ConnectorDefinitionVersion'{connectors = Core.Nothing}

-- | A list of references to connectors in this version, with their corresponding configuration settings.
--
-- /Note:/ Consider using 'connectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvConnectors :: Lens.Lens' ConnectorDefinitionVersion (Core.Maybe [Types.Connector])
cdvConnectors = Lens.field @"connectors"
{-# INLINEABLE cdvConnectors #-}
{-# DEPRECATED connectors "Use generic-lens or generic-optics with 'connectors' instead"  #-}

instance Core.FromJSON ConnectorDefinitionVersion where
        toJSON ConnectorDefinitionVersion{..}
          = Core.object
              (Core.catMaybes [("Connectors" Core..=) Core.<$> connectors])

instance Core.FromJSON ConnectorDefinitionVersion where
        parseJSON
          = Core.withObject "ConnectorDefinitionVersion" Core.$
              \ x ->
                ConnectorDefinitionVersion' Core.<$> (x Core..:? "Connectors")

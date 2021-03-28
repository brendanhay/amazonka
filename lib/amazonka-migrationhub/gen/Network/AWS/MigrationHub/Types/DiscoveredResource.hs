{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.DiscoveredResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types.DiscoveredResource
  ( DiscoveredResource (..)
  -- * Smart constructor
  , mkDiscoveredResource
  -- * Lenses
  , drConfigurationId
  , drDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.ConfigurationId as Types
import qualified Network.AWS.MigrationHub.Types.Description as Types
import qualified Network.AWS.Prelude as Core

-- | Object representing the on-premises resource being migrated.
--
-- /See:/ 'mkDiscoveredResource' smart constructor.
data DiscoveredResource = DiscoveredResource'
  { configurationId :: Types.ConfigurationId
    -- ^ The configurationId in Application Discovery Service that uniquely identifies the on-premise resource.
  , description :: Core.Maybe Types.Description
    -- ^ A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiscoveredResource' value with any optional fields omitted.
mkDiscoveredResource
    :: Types.ConfigurationId -- ^ 'configurationId'
    -> DiscoveredResource
mkDiscoveredResource configurationId
  = DiscoveredResource'{configurationId, description = Core.Nothing}

-- | The configurationId in Application Discovery Service that uniquely identifies the on-premise resource.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drConfigurationId :: Lens.Lens' DiscoveredResource Types.ConfigurationId
drConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE drConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDescription :: Lens.Lens' DiscoveredResource (Core.Maybe Types.Description)
drDescription = Lens.field @"description"
{-# INLINEABLE drDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromJSON DiscoveredResource where
        toJSON DiscoveredResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigurationId" Core..= configurationId),
                  ("Description" Core..=) Core.<$> description])

instance Core.FromJSON DiscoveredResource where
        parseJSON
          = Core.withObject "DiscoveredResource" Core.$
              \ x ->
                DiscoveredResource' Core.<$>
                  (x Core..: "ConfigurationId") Core.<*> x Core..:? "Description"

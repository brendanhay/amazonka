{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.NielsenConfiguration
  ( NielsenConfiguration (..)
  -- * Smart constructor
  , mkNielsenConfiguration
  -- * Lenses
  , ncDistributorId
  , ncNielsenPcmToId3Tagging
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState as Types
import qualified Network.AWS.Prelude as Core

-- | Nielsen Configuration
--
-- /See:/ 'mkNielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { distributorId :: Core.Maybe Core.Text
    -- ^ Enter the Distributor ID assigned to your organization by Nielsen.
  , nielsenPcmToId3Tagging :: Core.Maybe Types.NielsenPcmToId3TaggingState
    -- ^ Enables Nielsen PCM to ID3 tagging
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NielsenConfiguration' value with any optional fields omitted.
mkNielsenConfiguration
    :: NielsenConfiguration
mkNielsenConfiguration
  = NielsenConfiguration'{distributorId = Core.Nothing,
                          nielsenPcmToId3Tagging = Core.Nothing}

-- | Enter the Distributor ID assigned to your organization by Nielsen.
--
-- /Note:/ Consider using 'distributorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncDistributorId :: Lens.Lens' NielsenConfiguration (Core.Maybe Core.Text)
ncDistributorId = Lens.field @"distributorId"
{-# INLINEABLE ncDistributorId #-}
{-# DEPRECATED distributorId "Use generic-lens or generic-optics with 'distributorId' instead"  #-}

-- | Enables Nielsen PCM to ID3 tagging
--
-- /Note:/ Consider using 'nielsenPcmToId3Tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNielsenPcmToId3Tagging :: Lens.Lens' NielsenConfiguration (Core.Maybe Types.NielsenPcmToId3TaggingState)
ncNielsenPcmToId3Tagging = Lens.field @"nielsenPcmToId3Tagging"
{-# INLINEABLE ncNielsenPcmToId3Tagging #-}
{-# DEPRECATED nielsenPcmToId3Tagging "Use generic-lens or generic-optics with 'nielsenPcmToId3Tagging' instead"  #-}

instance Core.FromJSON NielsenConfiguration where
        toJSON NielsenConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("distributorId" Core..=) Core.<$> distributorId,
                  ("nielsenPcmToId3Tagging" Core..=) Core.<$>
                    nielsenPcmToId3Tagging])

instance Core.FromJSON NielsenConfiguration where
        parseJSON
          = Core.withObject "NielsenConfiguration" Core.$
              \ x ->
                NielsenConfiguration' Core.<$>
                  (x Core..:? "distributorId") Core.<*>
                    x Core..:? "nielsenPcmToId3Tagging"

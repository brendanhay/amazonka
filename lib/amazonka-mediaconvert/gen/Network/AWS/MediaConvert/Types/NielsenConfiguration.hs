{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenConfiguration
  ( NielsenConfiguration (..),

    -- * Smart constructor
    mkNielsenConfiguration,

    -- * Lenses
    ncBreakoutCode,
    ncDistributorId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- /See:/ 'mkNielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { -- | Nielsen has discontinued the use of breakout code functionality. If you must include this property, set the value to zero.
    breakoutCode :: Core.Maybe Core.Natural,
    -- | Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
    distributorId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NielsenConfiguration' value with any optional fields omitted.
mkNielsenConfiguration ::
  NielsenConfiguration
mkNielsenConfiguration =
  NielsenConfiguration'
    { breakoutCode = Core.Nothing,
      distributorId = Core.Nothing
    }

-- | Nielsen has discontinued the use of breakout code functionality. If you must include this property, set the value to zero.
--
-- /Note:/ Consider using 'breakoutCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncBreakoutCode :: Lens.Lens' NielsenConfiguration (Core.Maybe Core.Natural)
ncBreakoutCode = Lens.field @"breakoutCode"
{-# DEPRECATED ncBreakoutCode "Use generic-lens or generic-optics with 'breakoutCode' instead." #-}

-- | Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
--
-- /Note:/ Consider using 'distributorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncDistributorId :: Lens.Lens' NielsenConfiguration (Core.Maybe Core.Text)
ncDistributorId = Lens.field @"distributorId"
{-# DEPRECATED ncDistributorId "Use generic-lens or generic-optics with 'distributorId' instead." #-}

instance Core.FromJSON NielsenConfiguration where
  toJSON NielsenConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("breakoutCode" Core..=) Core.<$> breakoutCode,
            ("distributorId" Core..=) Core.<$> distributorId
          ]
      )

instance Core.FromJSON NielsenConfiguration where
  parseJSON =
    Core.withObject "NielsenConfiguration" Core.$
      \x ->
        NielsenConfiguration'
          Core.<$> (x Core..:? "breakoutCode") Core.<*> (x Core..:? "distributorId")

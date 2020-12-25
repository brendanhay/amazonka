{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.VersioningConfiguration
  ( VersioningConfiguration (..),

    -- * Smart constructor
    mkVersioningConfiguration,

    -- * Lenses
    vcMaxVersions,
    vcUnlimited,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the versioning of dataset contents.
--
-- /See:/ 'mkVersioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { -- | How many versions of dataset contents are kept. The @unlimited@ parameter must be @false@ .
    maxVersions :: Core.Maybe Core.Natural,
    -- | If true, unlimited versions of dataset contents are kept.
    unlimited :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VersioningConfiguration' value with any optional fields omitted.
mkVersioningConfiguration ::
  VersioningConfiguration
mkVersioningConfiguration =
  VersioningConfiguration'
    { maxVersions = Core.Nothing,
      unlimited = Core.Nothing
    }

-- | How many versions of dataset contents are kept. The @unlimited@ parameter must be @false@ .
--
-- /Note:/ Consider using 'maxVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcMaxVersions :: Lens.Lens' VersioningConfiguration (Core.Maybe Core.Natural)
vcMaxVersions = Lens.field @"maxVersions"
{-# DEPRECATED vcMaxVersions "Use generic-lens or generic-optics with 'maxVersions' instead." #-}

-- | If true, unlimited versions of dataset contents are kept.
--
-- /Note:/ Consider using 'unlimited' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcUnlimited :: Lens.Lens' VersioningConfiguration (Core.Maybe Core.Bool)
vcUnlimited = Lens.field @"unlimited"
{-# DEPRECATED vcUnlimited "Use generic-lens or generic-optics with 'unlimited' instead." #-}

instance Core.FromJSON VersioningConfiguration where
  toJSON VersioningConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxVersions" Core..=) Core.<$> maxVersions,
            ("unlimited" Core..=) Core.<$> unlimited
          ]
      )

instance Core.FromJSON VersioningConfiguration where
  parseJSON =
    Core.withObject "VersioningConfiguration" Core.$
      \x ->
        VersioningConfiguration'
          Core.<$> (x Core..:? "maxVersions") Core.<*> (x Core..:? "unlimited")

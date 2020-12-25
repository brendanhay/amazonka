{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.BundleDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.BundleDetails
  ( BundleDetails (..),

    -- * Smart constructor
    mkBundleDetails,

    -- * Lenses
    bdAvailablePlatforms,
    bdBundleId,
    bdDescription,
    bdIconUrl,
    bdTitle,
    bdVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types.BundleDescription as Types
import qualified Network.AWS.Mobile.Types.BundleId as Types
import qualified Network.AWS.Mobile.Types.BundleTitle as Types
import qualified Network.AWS.Mobile.Types.BundleVersion as Types
import qualified Network.AWS.Mobile.Types.IconUrl as Types
import qualified Network.AWS.Mobile.Types.Platform as Types
import qualified Network.AWS.Prelude as Core

-- | The details of the bundle.
--
-- /See:/ 'mkBundleDetails' smart constructor.
data BundleDetails = BundleDetails'
  { availablePlatforms :: Core.Maybe [Types.Platform],
    bundleId :: Core.Maybe Types.BundleId,
    description :: Core.Maybe Types.BundleDescription,
    iconUrl :: Core.Maybe Types.IconUrl,
    title :: Core.Maybe Types.BundleTitle,
    version :: Core.Maybe Types.BundleVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BundleDetails' value with any optional fields omitted.
mkBundleDetails ::
  BundleDetails
mkBundleDetails =
  BundleDetails'
    { availablePlatforms = Core.Nothing,
      bundleId = Core.Nothing,
      description = Core.Nothing,
      iconUrl = Core.Nothing,
      title = Core.Nothing,
      version = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'availablePlatforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdAvailablePlatforms :: Lens.Lens' BundleDetails (Core.Maybe [Types.Platform])
bdAvailablePlatforms = Lens.field @"availablePlatforms"
{-# DEPRECATED bdAvailablePlatforms "Use generic-lens or generic-optics with 'availablePlatforms' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBundleId :: Lens.Lens' BundleDetails (Core.Maybe Types.BundleId)
bdBundleId = Lens.field @"bundleId"
{-# DEPRECATED bdBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdDescription :: Lens.Lens' BundleDetails (Core.Maybe Types.BundleDescription)
bdDescription = Lens.field @"description"
{-# DEPRECATED bdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'iconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdIconUrl :: Lens.Lens' BundleDetails (Core.Maybe Types.IconUrl)
bdIconUrl = Lens.field @"iconUrl"
{-# DEPRECATED bdIconUrl "Use generic-lens or generic-optics with 'iconUrl' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdTitle :: Lens.Lens' BundleDetails (Core.Maybe Types.BundleTitle)
bdTitle = Lens.field @"title"
{-# DEPRECATED bdTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdVersion :: Lens.Lens' BundleDetails (Core.Maybe Types.BundleVersion)
bdVersion = Lens.field @"version"
{-# DEPRECATED bdVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON BundleDetails where
  parseJSON =
    Core.withObject "BundleDetails" Core.$
      \x ->
        BundleDetails'
          Core.<$> (x Core..:? "availablePlatforms")
          Core.<*> (x Core..:? "bundleId")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "iconUrl")
          Core.<*> (x Core..:? "title")
          Core.<*> (x Core..:? "version")

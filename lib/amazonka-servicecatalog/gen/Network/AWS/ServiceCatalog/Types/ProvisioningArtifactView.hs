{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
  ( ProvisioningArtifactView (..),

    -- * Smart constructor
    mkProvisioningArtifactView,

    -- * Lenses
    pavProductViewSummary,
    pavProvisioningArtifact,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ProductViewSummary as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifact as Types

-- | An object that contains summary information about a product view and a provisioning artifact.
--
-- /See:/ 'mkProvisioningArtifactView' smart constructor.
data ProvisioningArtifactView = ProvisioningArtifactView'
  { -- | Summary information about a product view.
    productViewSummary :: Core.Maybe Types.ProductViewSummary,
    -- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
    provisioningArtifact :: Core.Maybe Types.ProvisioningArtifact
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProvisioningArtifactView' value with any optional fields omitted.
mkProvisioningArtifactView ::
  ProvisioningArtifactView
mkProvisioningArtifactView =
  ProvisioningArtifactView'
    { productViewSummary = Core.Nothing,
      provisioningArtifact = Core.Nothing
    }

-- | Summary information about a product view.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavProductViewSummary :: Lens.Lens' ProvisioningArtifactView (Core.Maybe Types.ProductViewSummary)
pavProductViewSummary = Lens.field @"productViewSummary"
{-# DEPRECATED pavProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
--
-- /Note:/ Consider using 'provisioningArtifact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavProvisioningArtifact :: Lens.Lens' ProvisioningArtifactView (Core.Maybe Types.ProvisioningArtifact)
pavProvisioningArtifact = Lens.field @"provisioningArtifact"
{-# DEPRECATED pavProvisioningArtifact "Use generic-lens or generic-optics with 'provisioningArtifact' instead." #-}

instance Core.FromJSON ProvisioningArtifactView where
  parseJSON =
    Core.withObject "ProvisioningArtifactView" Core.$
      \x ->
        ProvisioningArtifactView'
          Core.<$> (x Core..:? "ProductViewSummary")
          Core.<*> (x Core..:? "ProvisioningArtifact")

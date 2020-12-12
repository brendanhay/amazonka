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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifact

-- | An object that contains summary information about a product view and a provisioning artifact.
--
-- /See:/ 'mkProvisioningArtifactView' smart constructor.
data ProvisioningArtifactView = ProvisioningArtifactView'
  { productViewSummary ::
      Lude.Maybe ProductViewSummary,
    provisioningArtifact ::
      Lude.Maybe ProvisioningArtifact
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactView' with the minimum fields required to make a request.
--
-- * 'productViewSummary' - Summary information about a product view.
-- * 'provisioningArtifact' - Information about a provisioning artifact. A provisioning artifact is also known as a product version.
mkProvisioningArtifactView ::
  ProvisioningArtifactView
mkProvisioningArtifactView =
  ProvisioningArtifactView'
    { productViewSummary = Lude.Nothing,
      provisioningArtifact = Lude.Nothing
    }

-- | Summary information about a product view.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavProductViewSummary :: Lens.Lens' ProvisioningArtifactView (Lude.Maybe ProductViewSummary)
pavProductViewSummary = Lens.lens (productViewSummary :: ProvisioningArtifactView -> Lude.Maybe ProductViewSummary) (\s a -> s {productViewSummary = a} :: ProvisioningArtifactView)
{-# DEPRECATED pavProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
--
-- /Note:/ Consider using 'provisioningArtifact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavProvisioningArtifact :: Lens.Lens' ProvisioningArtifactView (Lude.Maybe ProvisioningArtifact)
pavProvisioningArtifact = Lens.lens (provisioningArtifact :: ProvisioningArtifactView -> Lude.Maybe ProvisioningArtifact) (\s a -> s {provisioningArtifact = a} :: ProvisioningArtifactView)
{-# DEPRECATED pavProvisioningArtifact "Use generic-lens or generic-optics with 'provisioningArtifact' instead." #-}

instance Lude.FromJSON ProvisioningArtifactView where
  parseJSON =
    Lude.withObject
      "ProvisioningArtifactView"
      ( \x ->
          ProvisioningArtifactView'
            Lude.<$> (x Lude..:? "ProductViewSummary")
            Lude.<*> (x Lude..:? "ProvisioningArtifact")
      )

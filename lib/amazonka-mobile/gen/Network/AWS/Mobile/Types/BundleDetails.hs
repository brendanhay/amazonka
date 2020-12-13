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
    bdVersion,
    bdIconURL,
    bdTitle,
    bdDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.Platform
import qualified Network.AWS.Prelude as Lude

-- | The details of the bundle.
--
-- /See:/ 'mkBundleDetails' smart constructor.
data BundleDetails = BundleDetails'
  { availablePlatforms :: Lude.Maybe [Platform],
    bundleId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    iconURL :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BundleDetails' with the minimum fields required to make a request.
--
-- * 'availablePlatforms' -
-- * 'bundleId' -
-- * 'version' -
-- * 'iconURL' -
-- * 'title' -
-- * 'description' -
mkBundleDetails ::
  BundleDetails
mkBundleDetails =
  BundleDetails'
    { availablePlatforms = Lude.Nothing,
      bundleId = Lude.Nothing,
      version = Lude.Nothing,
      iconURL = Lude.Nothing,
      title = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'availablePlatforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdAvailablePlatforms :: Lens.Lens' BundleDetails (Lude.Maybe [Platform])
bdAvailablePlatforms = Lens.lens (availablePlatforms :: BundleDetails -> Lude.Maybe [Platform]) (\s a -> s {availablePlatforms = a} :: BundleDetails)
{-# DEPRECATED bdAvailablePlatforms "Use generic-lens or generic-optics with 'availablePlatforms' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBundleId :: Lens.Lens' BundleDetails (Lude.Maybe Lude.Text)
bdBundleId = Lens.lens (bundleId :: BundleDetails -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: BundleDetails)
{-# DEPRECATED bdBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdVersion :: Lens.Lens' BundleDetails (Lude.Maybe Lude.Text)
bdVersion = Lens.lens (version :: BundleDetails -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: BundleDetails)
{-# DEPRECATED bdVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'iconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdIconURL :: Lens.Lens' BundleDetails (Lude.Maybe Lude.Text)
bdIconURL = Lens.lens (iconURL :: BundleDetails -> Lude.Maybe Lude.Text) (\s a -> s {iconURL = a} :: BundleDetails)
{-# DEPRECATED bdIconURL "Use generic-lens or generic-optics with 'iconURL' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdTitle :: Lens.Lens' BundleDetails (Lude.Maybe Lude.Text)
bdTitle = Lens.lens (title :: BundleDetails -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: BundleDetails)
{-# DEPRECATED bdTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdDescription :: Lens.Lens' BundleDetails (Lude.Maybe Lude.Text)
bdDescription = Lens.lens (description :: BundleDetails -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: BundleDetails)
{-# DEPRECATED bdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON BundleDetails where
  parseJSON =
    Lude.withObject
      "BundleDetails"
      ( \x ->
          BundleDetails'
            Lude.<$> (x Lude..:? "availablePlatforms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "bundleId")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "iconUrl")
            Lude.<*> (x Lude..:? "title")
            Lude.<*> (x Lude..:? "description")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RegionsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RegionsInfo
  ( RegionsInfo (..),

    -- * Smart constructor
    mkRegionsInfo,

    -- * Lenses
    riPrimaryRegion,
    riAdditionalRegions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the Regions that are configured for multi-Region replication.
--
-- /See:/ 'mkRegionsInfo' smart constructor.
data RegionsInfo = RegionsInfo'
  { -- | The Region from where the AWS Managed Microsoft AD directory was originally created.
    primaryRegion :: Lude.Maybe Lude.Text,
    -- | Lists the Regions where the directory has been replicated, excluding the primary Region.
    additionalRegions :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegionsInfo' with the minimum fields required to make a request.
--
-- * 'primaryRegion' - The Region from where the AWS Managed Microsoft AD directory was originally created.
-- * 'additionalRegions' - Lists the Regions where the directory has been replicated, excluding the primary Region.
mkRegionsInfo ::
  RegionsInfo
mkRegionsInfo =
  RegionsInfo'
    { primaryRegion = Lude.Nothing,
      additionalRegions = Lude.Nothing
    }

-- | The Region from where the AWS Managed Microsoft AD directory was originally created.
--
-- /Note:/ Consider using 'primaryRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPrimaryRegion :: Lens.Lens' RegionsInfo (Lude.Maybe Lude.Text)
riPrimaryRegion = Lens.lens (primaryRegion :: RegionsInfo -> Lude.Maybe Lude.Text) (\s a -> s {primaryRegion = a} :: RegionsInfo)
{-# DEPRECATED riPrimaryRegion "Use generic-lens or generic-optics with 'primaryRegion' instead." #-}

-- | Lists the Regions where the directory has been replicated, excluding the primary Region.
--
-- /Note:/ Consider using 'additionalRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAdditionalRegions :: Lens.Lens' RegionsInfo (Lude.Maybe [Lude.Text])
riAdditionalRegions = Lens.lens (additionalRegions :: RegionsInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalRegions = a} :: RegionsInfo)
{-# DEPRECATED riAdditionalRegions "Use generic-lens or generic-optics with 'additionalRegions' instead." #-}

instance Lude.FromJSON RegionsInfo where
  parseJSON =
    Lude.withObject
      "RegionsInfo"
      ( \x ->
          RegionsInfo'
            Lude.<$> (x Lude..:? "PrimaryRegion")
            Lude.<*> (x Lude..:? "AdditionalRegions" Lude..!= Lude.mempty)
      )

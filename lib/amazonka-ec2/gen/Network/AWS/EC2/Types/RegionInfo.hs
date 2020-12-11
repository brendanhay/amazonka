-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RegionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RegionInfo
  ( RegionInfo (..),

    -- * Smart constructor
    mkRegionInfo,

    -- * Lenses
    riRegionName,
    riOptInStatus,
    riEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Region.
--
-- /See:/ 'mkRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { regionName :: Lude.Maybe Lude.Text,
    optInStatus :: Lude.Maybe Lude.Text,
    endpoint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegionInfo' with the minimum fields required to make a request.
--
-- * 'endpoint' - The Region service endpoint.
-- * 'optInStatus' - The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
-- * 'regionName' - The name of the Region.
mkRegionInfo ::
  RegionInfo
mkRegionInfo =
  RegionInfo'
    { regionName = Lude.Nothing,
      optInStatus = Lude.Nothing,
      endpoint = Lude.Nothing
    }

-- | The name of the Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRegionName :: Lens.Lens' RegionInfo (Lude.Maybe Lude.Text)
riRegionName = Lens.lens (regionName :: RegionInfo -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: RegionInfo)
{-# DEPRECATED riRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riOptInStatus :: Lens.Lens' RegionInfo (Lude.Maybe Lude.Text)
riOptInStatus = Lens.lens (optInStatus :: RegionInfo -> Lude.Maybe Lude.Text) (\s a -> s {optInStatus = a} :: RegionInfo)
{-# DEPRECATED riOptInStatus "Use generic-lens or generic-optics with 'optInStatus' instead." #-}

-- | The Region service endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEndpoint :: Lens.Lens' RegionInfo (Lude.Maybe Lude.Text)
riEndpoint = Lens.lens (endpoint :: RegionInfo -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: RegionInfo)
{-# DEPRECATED riEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.FromXML RegionInfo where
  parseXML x =
    RegionInfo'
      Lude.<$> (x Lude..@? "regionName")
      Lude.<*> (x Lude..@? "optInStatus")
      Lude.<*> (x Lude..@? "regionEndpoint")

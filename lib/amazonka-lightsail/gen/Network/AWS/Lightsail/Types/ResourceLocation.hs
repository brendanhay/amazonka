-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceLocation
  ( ResourceLocation (..),

    -- * Smart constructor
    mkResourceLocation,

    -- * Lenses
    rlRegionName,
    rlAvailabilityZone,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.RegionName
import qualified Network.AWS.Prelude as Lude

-- | Describes the resource location.
--
-- /See:/ 'mkResourceLocation' smart constructor.
data ResourceLocation = ResourceLocation'
  { regionName ::
      Lude.Maybe RegionName,
    availabilityZone :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceLocation' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
-- * 'regionName' - The AWS Region name.
mkResourceLocation ::
  ResourceLocation
mkResourceLocation =
  ResourceLocation'
    { regionName = Lude.Nothing,
      availabilityZone = Lude.Nothing
    }

-- | The AWS Region name.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlRegionName :: Lens.Lens' ResourceLocation (Lude.Maybe RegionName)
rlRegionName = Lens.lens (regionName :: ResourceLocation -> Lude.Maybe RegionName) (\s a -> s {regionName = a} :: ResourceLocation)
{-# DEPRECATED rlRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlAvailabilityZone :: Lens.Lens' ResourceLocation (Lude.Maybe Lude.Text)
rlAvailabilityZone = Lens.lens (availabilityZone :: ResourceLocation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ResourceLocation)
{-# DEPRECATED rlAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.FromJSON ResourceLocation where
  parseJSON =
    Lude.withObject
      "ResourceLocation"
      ( \x ->
          ResourceLocation'
            Lude.<$> (x Lude..:? "regionName") Lude.<*> (x Lude..:? "availabilityZone")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Location
  ( Location (..),

    -- * Smart constructor
    mkLocation,

    -- * Lenses
    lAvailablePortSpeeds,
    lLocationName,
    lLocationCode,
    lRegion,
    lAvailableProviders,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an AWS Direct Connect location.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { -- | The available port speeds for the location.
    availablePortSpeeds :: Lude.Maybe [Lude.Text],
    -- | The name of the location. This includes the name of the colocation partner and the physical site of the building.
    locationName :: Lude.Maybe Lude.Text,
    -- | The code for the location.
    locationCode :: Lude.Maybe Lude.Text,
    -- | The AWS Region for the location.
    region :: Lude.Maybe Lude.Text,
    -- | The name of the service provider for the location.
    availableProviders :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- * 'availablePortSpeeds' - The available port speeds for the location.
-- * 'locationName' - The name of the location. This includes the name of the colocation partner and the physical site of the building.
-- * 'locationCode' - The code for the location.
-- * 'region' - The AWS Region for the location.
-- * 'availableProviders' - The name of the service provider for the location.
mkLocation ::
  Location
mkLocation =
  Location'
    { availablePortSpeeds = Lude.Nothing,
      locationName = Lude.Nothing,
      locationCode = Lude.Nothing,
      region = Lude.Nothing,
      availableProviders = Lude.Nothing
    }

-- | The available port speeds for the location.
--
-- /Note:/ Consider using 'availablePortSpeeds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAvailablePortSpeeds :: Lens.Lens' Location (Lude.Maybe [Lude.Text])
lAvailablePortSpeeds = Lens.lens (availablePortSpeeds :: Location -> Lude.Maybe [Lude.Text]) (\s a -> s {availablePortSpeeds = a} :: Location)
{-# DEPRECATED lAvailablePortSpeeds "Use generic-lens or generic-optics with 'availablePortSpeeds' instead." #-}

-- | The name of the location. This includes the name of the colocation partner and the physical site of the building.
--
-- /Note:/ Consider using 'locationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLocationName :: Lens.Lens' Location (Lude.Maybe Lude.Text)
lLocationName = Lens.lens (locationName :: Location -> Lude.Maybe Lude.Text) (\s a -> s {locationName = a} :: Location)
{-# DEPRECATED lLocationName "Use generic-lens or generic-optics with 'locationName' instead." #-}

-- | The code for the location.
--
-- /Note:/ Consider using 'locationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLocationCode :: Lens.Lens' Location (Lude.Maybe Lude.Text)
lLocationCode = Lens.lens (locationCode :: Location -> Lude.Maybe Lude.Text) (\s a -> s {locationCode = a} :: Location)
{-# DEPRECATED lLocationCode "Use generic-lens or generic-optics with 'locationCode' instead." #-}

-- | The AWS Region for the location.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lRegion :: Lens.Lens' Location (Lude.Maybe Lude.Text)
lRegion = Lens.lens (region :: Location -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Location)
{-# DEPRECATED lRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The name of the service provider for the location.
--
-- /Note:/ Consider using 'availableProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAvailableProviders :: Lens.Lens' Location (Lude.Maybe [Lude.Text])
lAvailableProviders = Lens.lens (availableProviders :: Location -> Lude.Maybe [Lude.Text]) (\s a -> s {availableProviders = a} :: Location)
{-# DEPRECATED lAvailableProviders "Use generic-lens or generic-optics with 'availableProviders' instead." #-}

instance Lude.FromJSON Location where
  parseJSON =
    Lude.withObject
      "Location"
      ( \x ->
          Location'
            Lude.<$> (x Lude..:? "availablePortSpeeds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "locationName")
            Lude.<*> (x Lude..:? "locationCode")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "availableProviders" Lude..!= Lude.mempty)
      )

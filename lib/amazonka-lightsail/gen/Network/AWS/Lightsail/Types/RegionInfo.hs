{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RegionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RegionInfo
  ( RegionInfo (..),

    -- * Smart constructor
    mkRegionInfo,

    -- * Lenses
    riAvailabilityZones,
    riName,
    riRelationalDatabaseAvailabilityZones,
    riDisplayName,
    riContinentCode,
    riDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.RegionName
import qualified Network.AWS.Prelude as Lude

-- | Describes the AWS Region.
--
-- /See:/ 'mkRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | The Availability Zones. Follows the format @us-east-2a@ (case-sensitive).
    availabilityZones :: Lude.Maybe [AvailabilityZone],
    -- | The region name (e.g., @us-east-2@ ).
    name :: Lude.Maybe RegionName,
    -- | The Availability Zones for databases. Follows the format @us-east-2a@ (case-sensitive).
    relationalDatabaseAvailabilityZones :: Lude.Maybe [AvailabilityZone],
    -- | The display name (e.g., @Ohio@ ).
    displayName :: Lude.Maybe Lude.Text,
    -- | The continent code (e.g., @NA@ , meaning North America).
    continentCode :: Lude.Maybe Lude.Text,
    -- | The description of the AWS Region (e.g., @This region is recommended to serve users in the eastern United States and eastern Canada@ ).
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegionInfo' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The Availability Zones. Follows the format @us-east-2a@ (case-sensitive).
-- * 'name' - The region name (e.g., @us-east-2@ ).
-- * 'relationalDatabaseAvailabilityZones' - The Availability Zones for databases. Follows the format @us-east-2a@ (case-sensitive).
-- * 'displayName' - The display name (e.g., @Ohio@ ).
-- * 'continentCode' - The continent code (e.g., @NA@ , meaning North America).
-- * 'description' - The description of the AWS Region (e.g., @This region is recommended to serve users in the eastern United States and eastern Canada@ ).
mkRegionInfo ::
  RegionInfo
mkRegionInfo =
  RegionInfo'
    { availabilityZones = Lude.Nothing,
      name = Lude.Nothing,
      relationalDatabaseAvailabilityZones = Lude.Nothing,
      displayName = Lude.Nothing,
      continentCode = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Availability Zones. Follows the format @us-east-2a@ (case-sensitive).
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAvailabilityZones :: Lens.Lens' RegionInfo (Lude.Maybe [AvailabilityZone])
riAvailabilityZones = Lens.lens (availabilityZones :: RegionInfo -> Lude.Maybe [AvailabilityZone]) (\s a -> s {availabilityZones = a} :: RegionInfo)
{-# DEPRECATED riAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The region name (e.g., @us-east-2@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riName :: Lens.Lens' RegionInfo (Lude.Maybe RegionName)
riName = Lens.lens (name :: RegionInfo -> Lude.Maybe RegionName) (\s a -> s {name = a} :: RegionInfo)
{-# DEPRECATED riName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Availability Zones for databases. Follows the format @us-east-2a@ (case-sensitive).
--
-- /Note:/ Consider using 'relationalDatabaseAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRelationalDatabaseAvailabilityZones :: Lens.Lens' RegionInfo (Lude.Maybe [AvailabilityZone])
riRelationalDatabaseAvailabilityZones = Lens.lens (relationalDatabaseAvailabilityZones :: RegionInfo -> Lude.Maybe [AvailabilityZone]) (\s a -> s {relationalDatabaseAvailabilityZones = a} :: RegionInfo)
{-# DEPRECATED riRelationalDatabaseAvailabilityZones "Use generic-lens or generic-optics with 'relationalDatabaseAvailabilityZones' instead." #-}

-- | The display name (e.g., @Ohio@ ).
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDisplayName :: Lens.Lens' RegionInfo (Lude.Maybe Lude.Text)
riDisplayName = Lens.lens (displayName :: RegionInfo -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: RegionInfo)
{-# DEPRECATED riDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The continent code (e.g., @NA@ , meaning North America).
--
-- /Note:/ Consider using 'continentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riContinentCode :: Lens.Lens' RegionInfo (Lude.Maybe Lude.Text)
riContinentCode = Lens.lens (continentCode :: RegionInfo -> Lude.Maybe Lude.Text) (\s a -> s {continentCode = a} :: RegionInfo)
{-# DEPRECATED riContinentCode "Use generic-lens or generic-optics with 'continentCode' instead." #-}

-- | The description of the AWS Region (e.g., @This region is recommended to serve users in the eastern United States and eastern Canada@ ).
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDescription :: Lens.Lens' RegionInfo (Lude.Maybe Lude.Text)
riDescription = Lens.lens (description :: RegionInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RegionInfo)
{-# DEPRECATED riDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON RegionInfo where
  parseJSON =
    Lude.withObject
      "RegionInfo"
      ( \x ->
          RegionInfo'
            Lude.<$> (x Lude..:? "availabilityZones" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> ( x Lude..:? "relationalDatabaseAvailabilityZones"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "displayName")
            Lude.<*> (x Lude..:? "continentCode")
            Lude.<*> (x Lude..:? "description")
      )

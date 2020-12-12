{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RegionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RegionDescription
  ( RegionDescription (..),

    -- * Smart constructor
    mkRegionDescription,

    -- * Lenses
    rdStatus,
    rdDirectoryId,
    rdRegionName,
    rdDesiredNumberOfDomainControllers,
    rdRegionType,
    rdLaunchTime,
    rdLastUpdatedDateTime,
    rdStatusLastUpdatedDateTime,
    rdVPCSettings,
  )
where

import Network.AWS.DirectoryService.Types.DirectoryStage
import Network.AWS.DirectoryService.Types.DirectoryVPCSettings
import Network.AWS.DirectoryService.Types.RegionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The replicated regional information for a directory.
--
-- /See:/ 'mkRegionDescription' smart constructor.
data RegionDescription = RegionDescription'
  { status ::
      Lude.Maybe DirectoryStage,
    directoryId :: Lude.Maybe Lude.Text,
    regionName :: Lude.Maybe Lude.Text,
    desiredNumberOfDomainControllers ::
      Lude.Maybe Lude.Natural,
    regionType :: Lude.Maybe RegionType,
    launchTime :: Lude.Maybe Lude.Timestamp,
    lastUpdatedDateTime :: Lude.Maybe Lude.Timestamp,
    statusLastUpdatedDateTime :: Lude.Maybe Lude.Timestamp,
    vpcSettings :: Lude.Maybe DirectoryVPCSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegionDescription' with the minimum fields required to make a request.
--
-- * 'desiredNumberOfDomainControllers' - The desired number of domain controllers in the specified Region for the specified directory.
-- * 'directoryId' - The identifier of the directory.
-- * 'lastUpdatedDateTime' - The date and time that the Region description was last updated.
-- * 'launchTime' - Specifies when the Region replication began.
-- * 'regionName' - The name of the Region. For example, @us-east-1@ .
-- * 'regionType' - Specifies if the Region is the primary Region or an additional Region.
-- * 'status' - The status of the replication process for the specified Region.
-- * 'statusLastUpdatedDateTime' - The date and time that the Region status was last updated.
-- * 'vpcSettings' - Undocumented field.
mkRegionDescription ::
  RegionDescription
mkRegionDescription =
  RegionDescription'
    { status = Lude.Nothing,
      directoryId = Lude.Nothing,
      regionName = Lude.Nothing,
      desiredNumberOfDomainControllers = Lude.Nothing,
      regionType = Lude.Nothing,
      launchTime = Lude.Nothing,
      lastUpdatedDateTime = Lude.Nothing,
      statusLastUpdatedDateTime = Lude.Nothing,
      vpcSettings = Lude.Nothing
    }

-- | The status of the replication process for the specified Region.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdStatus :: Lens.Lens' RegionDescription (Lude.Maybe DirectoryStage)
rdStatus = Lens.lens (status :: RegionDescription -> Lude.Maybe DirectoryStage) (\s a -> s {status = a} :: RegionDescription)
{-# DEPRECATED rdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDirectoryId :: Lens.Lens' RegionDescription (Lude.Maybe Lude.Text)
rdDirectoryId = Lens.lens (directoryId :: RegionDescription -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: RegionDescription)
{-# DEPRECATED rdDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the Region. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRegionName :: Lens.Lens' RegionDescription (Lude.Maybe Lude.Text)
rdRegionName = Lens.lens (regionName :: RegionDescription -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: RegionDescription)
{-# DEPRECATED rdRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The desired number of domain controllers in the specified Region for the specified directory.
--
-- /Note:/ Consider using 'desiredNumberOfDomainControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDesiredNumberOfDomainControllers :: Lens.Lens' RegionDescription (Lude.Maybe Lude.Natural)
rdDesiredNumberOfDomainControllers = Lens.lens (desiredNumberOfDomainControllers :: RegionDescription -> Lude.Maybe Lude.Natural) (\s a -> s {desiredNumberOfDomainControllers = a} :: RegionDescription)
{-# DEPRECATED rdDesiredNumberOfDomainControllers "Use generic-lens or generic-optics with 'desiredNumberOfDomainControllers' instead." #-}

-- | Specifies if the Region is the primary Region or an additional Region.
--
-- /Note:/ Consider using 'regionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRegionType :: Lens.Lens' RegionDescription (Lude.Maybe RegionType)
rdRegionType = Lens.lens (regionType :: RegionDescription -> Lude.Maybe RegionType) (\s a -> s {regionType = a} :: RegionDescription)
{-# DEPRECATED rdRegionType "Use generic-lens or generic-optics with 'regionType' instead." #-}

-- | Specifies when the Region replication began.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLaunchTime :: Lens.Lens' RegionDescription (Lude.Maybe Lude.Timestamp)
rdLaunchTime = Lens.lens (launchTime :: RegionDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {launchTime = a} :: RegionDescription)
{-# DEPRECATED rdLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The date and time that the Region description was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLastUpdatedDateTime :: Lens.Lens' RegionDescription (Lude.Maybe Lude.Timestamp)
rdLastUpdatedDateTime = Lens.lens (lastUpdatedDateTime :: RegionDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDateTime = a} :: RegionDescription)
{-# DEPRECATED rdLastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead." #-}

-- | The date and time that the Region status was last updated.
--
-- /Note:/ Consider using 'statusLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdStatusLastUpdatedDateTime :: Lens.Lens' RegionDescription (Lude.Maybe Lude.Timestamp)
rdStatusLastUpdatedDateTime = Lens.lens (statusLastUpdatedDateTime :: RegionDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {statusLastUpdatedDateTime = a} :: RegionDescription)
{-# DEPRECATED rdStatusLastUpdatedDateTime "Use generic-lens or generic-optics with 'statusLastUpdatedDateTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdVPCSettings :: Lens.Lens' RegionDescription (Lude.Maybe DirectoryVPCSettings)
rdVPCSettings = Lens.lens (vpcSettings :: RegionDescription -> Lude.Maybe DirectoryVPCSettings) (\s a -> s {vpcSettings = a} :: RegionDescription)
{-# DEPRECATED rdVPCSettings "Use generic-lens or generic-optics with 'vpcSettings' instead." #-}

instance Lude.FromJSON RegionDescription where
  parseJSON =
    Lude.withObject
      "RegionDescription"
      ( \x ->
          RegionDescription'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "RegionName")
            Lude.<*> (x Lude..:? "DesiredNumberOfDomainControllers")
            Lude.<*> (x Lude..:? "RegionType")
            Lude.<*> (x Lude..:? "LaunchTime")
            Lude.<*> (x Lude..:? "LastUpdatedDateTime")
            Lude.<*> (x Lude..:? "StatusLastUpdatedDateTime")
            Lude.<*> (x Lude..:? "VpcSettings")
      )

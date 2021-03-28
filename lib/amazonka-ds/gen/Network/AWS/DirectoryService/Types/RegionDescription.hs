{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RegionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.RegionDescription
  ( RegionDescription (..)
  -- * Smart constructor
  , mkRegionDescription
  -- * Lenses
  , rdDesiredNumberOfDomainControllers
  , rdDirectoryId
  , rdLastUpdatedDateTime
  , rdLaunchTime
  , rdRegionName
  , rdRegionType
  , rdStatus
  , rdStatusLastUpdatedDateTime
  , rdVpcSettings
  ) where

import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryStage as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryVpcSettings as Types
import qualified Network.AWS.DirectoryService.Types.RegionName as Types
import qualified Network.AWS.DirectoryService.Types.RegionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The replicated regional information for a directory.
--
-- /See:/ 'mkRegionDescription' smart constructor.
data RegionDescription = RegionDescription'
  { desiredNumberOfDomainControllers :: Core.Maybe Core.Natural
    -- ^ The desired number of domain controllers in the specified Region for the specified directory.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the directory.
  , lastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the Region description was last updated.
  , launchTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Specifies when the Region replication began.
  , regionName :: Core.Maybe Types.RegionName
    -- ^ The name of the Region. For example, @us-east-1@ .
  , regionType :: Core.Maybe Types.RegionType
    -- ^ Specifies if the Region is the primary Region or an additional Region.
  , status :: Core.Maybe Types.DirectoryStage
    -- ^ The status of the replication process for the specified Region.
  , statusLastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the Region status was last updated.
  , vpcSettings :: Core.Maybe Types.DirectoryVpcSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RegionDescription' value with any optional fields omitted.
mkRegionDescription
    :: RegionDescription
mkRegionDescription
  = RegionDescription'{desiredNumberOfDomainControllers =
                         Core.Nothing,
                       directoryId = Core.Nothing, lastUpdatedDateTime = Core.Nothing,
                       launchTime = Core.Nothing, regionName = Core.Nothing,
                       regionType = Core.Nothing, status = Core.Nothing,
                       statusLastUpdatedDateTime = Core.Nothing,
                       vpcSettings = Core.Nothing}

-- | The desired number of domain controllers in the specified Region for the specified directory.
--
-- /Note:/ Consider using 'desiredNumberOfDomainControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDesiredNumberOfDomainControllers :: Lens.Lens' RegionDescription (Core.Maybe Core.Natural)
rdDesiredNumberOfDomainControllers = Lens.field @"desiredNumberOfDomainControllers"
{-# INLINEABLE rdDesiredNumberOfDomainControllers #-}
{-# DEPRECATED desiredNumberOfDomainControllers "Use generic-lens or generic-optics with 'desiredNumberOfDomainControllers' instead"  #-}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDirectoryId :: Lens.Lens' RegionDescription (Core.Maybe Types.DirectoryId)
rdDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE rdDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The date and time that the Region description was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLastUpdatedDateTime :: Lens.Lens' RegionDescription (Core.Maybe Core.NominalDiffTime)
rdLastUpdatedDateTime = Lens.field @"lastUpdatedDateTime"
{-# INLINEABLE rdLastUpdatedDateTime #-}
{-# DEPRECATED lastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead"  #-}

-- | Specifies when the Region replication began.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLaunchTime :: Lens.Lens' RegionDescription (Core.Maybe Core.NominalDiffTime)
rdLaunchTime = Lens.field @"launchTime"
{-# INLINEABLE rdLaunchTime #-}
{-# DEPRECATED launchTime "Use generic-lens or generic-optics with 'launchTime' instead"  #-}

-- | The name of the Region. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRegionName :: Lens.Lens' RegionDescription (Core.Maybe Types.RegionName)
rdRegionName = Lens.field @"regionName"
{-# INLINEABLE rdRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | Specifies if the Region is the primary Region or an additional Region.
--
-- /Note:/ Consider using 'regionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRegionType :: Lens.Lens' RegionDescription (Core.Maybe Types.RegionType)
rdRegionType = Lens.field @"regionType"
{-# INLINEABLE rdRegionType #-}
{-# DEPRECATED regionType "Use generic-lens or generic-optics with 'regionType' instead"  #-}

-- | The status of the replication process for the specified Region.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdStatus :: Lens.Lens' RegionDescription (Core.Maybe Types.DirectoryStage)
rdStatus = Lens.field @"status"
{-# INLINEABLE rdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date and time that the Region status was last updated.
--
-- /Note:/ Consider using 'statusLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdStatusLastUpdatedDateTime :: Lens.Lens' RegionDescription (Core.Maybe Core.NominalDiffTime)
rdStatusLastUpdatedDateTime = Lens.field @"statusLastUpdatedDateTime"
{-# INLINEABLE rdStatusLastUpdatedDateTime #-}
{-# DEPRECATED statusLastUpdatedDateTime "Use generic-lens or generic-optics with 'statusLastUpdatedDateTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdVpcSettings :: Lens.Lens' RegionDescription (Core.Maybe Types.DirectoryVpcSettings)
rdVpcSettings = Lens.field @"vpcSettings"
{-# INLINEABLE rdVpcSettings #-}
{-# DEPRECATED vpcSettings "Use generic-lens or generic-optics with 'vpcSettings' instead"  #-}

instance Core.FromJSON RegionDescription where
        parseJSON
          = Core.withObject "RegionDescription" Core.$
              \ x ->
                RegionDescription' Core.<$>
                  (x Core..:? "DesiredNumberOfDomainControllers") Core.<*>
                    x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "LastUpdatedDateTime"
                    Core.<*> x Core..:? "LaunchTime"
                    Core.<*> x Core..:? "RegionName"
                    Core.<*> x Core..:? "RegionType"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusLastUpdatedDateTime"
                    Core.<*> x Core..:? "VpcSettings"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceDataSyncSource
  ( ResourceDataSyncSource (..)
  -- * Smart constructor
  , mkResourceDataSyncSource
  -- * Lenses
  , rdssSourceType
  , rdssSourceRegions
  , rdssAwsOrganizationsSource
  , rdssIncludeFutureRegions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncSourceRegion as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncSourceType as Types

-- | Information about the source of the data included in the resource data sync.
--
-- /See:/ 'mkResourceDataSyncSource' smart constructor.
data ResourceDataSyncSource = ResourceDataSyncSource'
  { sourceType :: Types.ResourceDataSyncSourceType
    -- ^ The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
  , sourceRegions :: [Types.ResourceDataSyncSourceRegion]
    -- ^ The @SyncSource@ AWS Regions included in the resource data sync.
  , awsOrganizationsSource :: Core.Maybe Types.ResourceDataSyncAwsOrganizationsSource
    -- ^ Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations.
  , includeFutureRegions :: Core.Maybe Core.Bool
    -- ^ Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDataSyncSource' value with any optional fields omitted.
mkResourceDataSyncSource
    :: Types.ResourceDataSyncSourceType -- ^ 'sourceType'
    -> ResourceDataSyncSource
mkResourceDataSyncSource sourceType
  = ResourceDataSyncSource'{sourceType, sourceRegions = Core.mempty,
                            awsOrganizationsSource = Core.Nothing,
                            includeFutureRegions = Core.Nothing}

-- | The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssSourceType :: Lens.Lens' ResourceDataSyncSource Types.ResourceDataSyncSourceType
rdssSourceType = Lens.field @"sourceType"
{-# INLINEABLE rdssSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | The @SyncSource@ AWS Regions included in the resource data sync.
--
-- /Note:/ Consider using 'sourceRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssSourceRegions :: Lens.Lens' ResourceDataSyncSource [Types.ResourceDataSyncSourceRegion]
rdssSourceRegions = Lens.field @"sourceRegions"
{-# INLINEABLE rdssSourceRegions #-}
{-# DEPRECATED sourceRegions "Use generic-lens or generic-optics with 'sourceRegions' instead"  #-}

-- | Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations.
--
-- /Note:/ Consider using 'awsOrganizationsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssAwsOrganizationsSource :: Lens.Lens' ResourceDataSyncSource (Core.Maybe Types.ResourceDataSyncAwsOrganizationsSource)
rdssAwsOrganizationsSource = Lens.field @"awsOrganizationsSource"
{-# INLINEABLE rdssAwsOrganizationsSource #-}
{-# DEPRECATED awsOrganizationsSource "Use generic-lens or generic-optics with 'awsOrganizationsSource' instead"  #-}

-- | Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
--
-- /Note:/ Consider using 'includeFutureRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssIncludeFutureRegions :: Lens.Lens' ResourceDataSyncSource (Core.Maybe Core.Bool)
rdssIncludeFutureRegions = Lens.field @"includeFutureRegions"
{-# INLINEABLE rdssIncludeFutureRegions #-}
{-# DEPRECATED includeFutureRegions "Use generic-lens or generic-optics with 'includeFutureRegions' instead"  #-}

instance Core.FromJSON ResourceDataSyncSource where
        toJSON ResourceDataSyncSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SourceType" Core..= sourceType),
                  Core.Just ("SourceRegions" Core..= sourceRegions),
                  ("AwsOrganizationsSource" Core..=) Core.<$> awsOrganizationsSource,
                  ("IncludeFutureRegions" Core..=) Core.<$> includeFutureRegions])

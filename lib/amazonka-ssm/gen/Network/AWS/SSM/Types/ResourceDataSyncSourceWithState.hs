{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
  ( ResourceDataSyncSourceWithState (..)
  -- * Smart constructor
  , mkResourceDataSyncSourceWithState
  -- * Lenses
  , rdsswsAwsOrganizationsSource
  , rdsswsIncludeFutureRegions
  , rdsswsSourceRegions
  , rdsswsSourceType
  , rdsswsState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncSourceRegion as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncSourceType as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncState as Types

-- | The data type name for including resource data sync state. There are four sync states:
--
-- @OrganizationNotExists@ (Your organization doesn't exist)
-- @NoPermissions@ (The system can't locate the service-linked role. This role is automatically created when a user creates a resource data sync in Explorer.)
-- @InvalidOrganizationalUnit@ (You specified or selected an invalid unit in the resource data sync configuration.)
-- @TrustedAccessDisabled@ (You disabled Systems Manager access in the organization in AWS Organizations.)
--
-- /See:/ 'mkResourceDataSyncSourceWithState' smart constructor.
data ResourceDataSyncSourceWithState = ResourceDataSyncSourceWithState'
  { awsOrganizationsSource :: Core.Maybe Types.ResourceDataSyncAwsOrganizationsSource
    -- ^ The field name in @SyncSource@ for the @ResourceDataSyncAwsOrganizationsSource@ type.
  , includeFutureRegions :: Core.Maybe Core.Bool
    -- ^ Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
  , sourceRegions :: Core.Maybe [Types.ResourceDataSyncSourceRegion]
    -- ^ The @SyncSource@ AWS Regions included in the resource data sync.
  , sourceType :: Core.Maybe Types.ResourceDataSyncSourceType
    -- ^ The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
  , state :: Core.Maybe Types.ResourceDataSyncState
    -- ^ The data type name for including resource data sync state. There are four sync states:
--
-- @OrganizationNotExists@ : Your organization doesn't exist.
-- @NoPermissions@ : The system can't locate the service-linked role. This role is automatically created when a user creates a resource data sync in Explorer.
-- @InvalidOrganizationalUnit@ : You specified or selected an invalid unit in the resource data sync configuration.
-- @TrustedAccessDisabled@ : You disabled Systems Manager access in the organization in AWS Organizations.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDataSyncSourceWithState' value with any optional fields omitted.
mkResourceDataSyncSourceWithState
    :: ResourceDataSyncSourceWithState
mkResourceDataSyncSourceWithState
  = ResourceDataSyncSourceWithState'{awsOrganizationsSource =
                                       Core.Nothing,
                                     includeFutureRegions = Core.Nothing,
                                     sourceRegions = Core.Nothing, sourceType = Core.Nothing,
                                     state = Core.Nothing}

-- | The field name in @SyncSource@ for the @ResourceDataSyncAwsOrganizationsSource@ type.
--
-- /Note:/ Consider using 'awsOrganizationsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsAwsOrganizationsSource :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe Types.ResourceDataSyncAwsOrganizationsSource)
rdsswsAwsOrganizationsSource = Lens.field @"awsOrganizationsSource"
{-# INLINEABLE rdsswsAwsOrganizationsSource #-}
{-# DEPRECATED awsOrganizationsSource "Use generic-lens or generic-optics with 'awsOrganizationsSource' instead"  #-}

-- | Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
--
-- /Note:/ Consider using 'includeFutureRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsIncludeFutureRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe Core.Bool)
rdsswsIncludeFutureRegions = Lens.field @"includeFutureRegions"
{-# INLINEABLE rdsswsIncludeFutureRegions #-}
{-# DEPRECATED includeFutureRegions "Use generic-lens or generic-optics with 'includeFutureRegions' instead"  #-}

-- | The @SyncSource@ AWS Regions included in the resource data sync.
--
-- /Note:/ Consider using 'sourceRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsSourceRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe [Types.ResourceDataSyncSourceRegion])
rdsswsSourceRegions = Lens.field @"sourceRegions"
{-# INLINEABLE rdsswsSourceRegions #-}
{-# DEPRECATED sourceRegions "Use generic-lens or generic-optics with 'sourceRegions' instead"  #-}

-- | The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsSourceType :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe Types.ResourceDataSyncSourceType)
rdsswsSourceType = Lens.field @"sourceType"
{-# INLINEABLE rdsswsSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | The data type name for including resource data sync state. There are four sync states:
--
-- @OrganizationNotExists@ : Your organization doesn't exist.
-- @NoPermissions@ : The system can't locate the service-linked role. This role is automatically created when a user creates a resource data sync in Explorer.
-- @InvalidOrganizationalUnit@ : You specified or selected an invalid unit in the resource data sync configuration.
-- @TrustedAccessDisabled@ : You disabled Systems Manager access in the organization in AWS Organizations.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsState :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe Types.ResourceDataSyncState)
rdsswsState = Lens.field @"state"
{-# INLINEABLE rdsswsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON ResourceDataSyncSourceWithState where
        parseJSON
          = Core.withObject "ResourceDataSyncSourceWithState" Core.$
              \ x ->
                ResourceDataSyncSourceWithState' Core.<$>
                  (x Core..:? "AwsOrganizationsSource") Core.<*>
                    x Core..:? "IncludeFutureRegions"
                    Core.<*> x Core..:? "SourceRegions"
                    Core.<*> x Core..:? "SourceType"
                    Core.<*> x Core..:? "State"

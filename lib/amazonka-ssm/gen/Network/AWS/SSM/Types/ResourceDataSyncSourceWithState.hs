{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
  ( ResourceDataSyncSourceWithState (..),

    -- * Smart constructor
    mkResourceDataSyncSourceWithState,

    -- * Lenses
    rdsswsState,
    rdsswsIncludeFutureRegions,
    rdsswsSourceType,
    rdsswsAWSOrganizationsSource,
    rdsswsSourceRegions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource

-- | The data type name for including resource data sync state. There are four sync states:
--
-- @OrganizationNotExists@ (Your organization doesn't exist)
-- @NoPermissions@ (The system can't locate the service-linked role. This role is automatically created when a user creates a resource data sync in Explorer.)
-- @InvalidOrganizationalUnit@ (You specified or selected an invalid unit in the resource data sync configuration.)
-- @TrustedAccessDisabled@ (You disabled Systems Manager access in the organization in AWS Organizations.)
--
-- /See:/ 'mkResourceDataSyncSourceWithState' smart constructor.
data ResourceDataSyncSourceWithState = ResourceDataSyncSourceWithState'
  { -- | The data type name for including resource data sync state. There are four sync states:
    --
    -- @OrganizationNotExists@ : Your organization doesn't exist.
    -- @NoPermissions@ : The system can't locate the service-linked role. This role is automatically created when a user creates a resource data sync in Explorer.
    -- @InvalidOrganizationalUnit@ : You specified or selected an invalid unit in the resource data sync configuration.
    -- @TrustedAccessDisabled@ : You disabled Systems Manager access in the organization in AWS Organizations.
    state :: Lude.Maybe Lude.Text,
    -- | Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
    includeFutureRegions :: Lude.Maybe Lude.Bool,
    -- | The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
    sourceType :: Lude.Maybe Lude.Text,
    -- | The field name in @SyncSource@ for the @ResourceDataSyncAwsOrganizationsSource@ type.
    awsOrganizationsSource :: Lude.Maybe ResourceDataSyncAWSOrganizationsSource,
    -- | The @SyncSource@ AWS Regions included in the resource data sync.
    sourceRegions :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDataSyncSourceWithState' with the minimum fields required to make a request.
--
-- * 'state' - The data type name for including resource data sync state. There are four sync states:
--
-- @OrganizationNotExists@ : Your organization doesn't exist.
-- @NoPermissions@ : The system can't locate the service-linked role. This role is automatically created when a user creates a resource data sync in Explorer.
-- @InvalidOrganizationalUnit@ : You specified or selected an invalid unit in the resource data sync configuration.
-- @TrustedAccessDisabled@ : You disabled Systems Manager access in the organization in AWS Organizations.
-- * 'includeFutureRegions' - Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
-- * 'sourceType' - The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
-- * 'awsOrganizationsSource' - The field name in @SyncSource@ for the @ResourceDataSyncAwsOrganizationsSource@ type.
-- * 'sourceRegions' - The @SyncSource@ AWS Regions included in the resource data sync.
mkResourceDataSyncSourceWithState ::
  ResourceDataSyncSourceWithState
mkResourceDataSyncSourceWithState =
  ResourceDataSyncSourceWithState'
    { state = Lude.Nothing,
      includeFutureRegions = Lude.Nothing,
      sourceType = Lude.Nothing,
      awsOrganizationsSource = Lude.Nothing,
      sourceRegions = Lude.Nothing
    }

-- | The data type name for including resource data sync state. There are four sync states:
--
-- @OrganizationNotExists@ : Your organization doesn't exist.
-- @NoPermissions@ : The system can't locate the service-linked role. This role is automatically created when a user creates a resource data sync in Explorer.
-- @InvalidOrganizationalUnit@ : You specified or selected an invalid unit in the resource data sync configuration.
-- @TrustedAccessDisabled@ : You disabled Systems Manager access in the organization in AWS Organizations.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsState :: Lens.Lens' ResourceDataSyncSourceWithState (Lude.Maybe Lude.Text)
rdsswsState = Lens.lens (state :: ResourceDataSyncSourceWithState -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ResourceDataSyncSourceWithState)
{-# DEPRECATED rdsswsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
--
-- /Note:/ Consider using 'includeFutureRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsIncludeFutureRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Lude.Maybe Lude.Bool)
rdsswsIncludeFutureRegions = Lens.lens (includeFutureRegions :: ResourceDataSyncSourceWithState -> Lude.Maybe Lude.Bool) (\s a -> s {includeFutureRegions = a} :: ResourceDataSyncSourceWithState)
{-# DEPRECATED rdsswsIncludeFutureRegions "Use generic-lens or generic-optics with 'includeFutureRegions' instead." #-}

-- | The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsSourceType :: Lens.Lens' ResourceDataSyncSourceWithState (Lude.Maybe Lude.Text)
rdsswsSourceType = Lens.lens (sourceType :: ResourceDataSyncSourceWithState -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: ResourceDataSyncSourceWithState)
{-# DEPRECATED rdsswsSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The field name in @SyncSource@ for the @ResourceDataSyncAwsOrganizationsSource@ type.
--
-- /Note:/ Consider using 'awsOrganizationsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsAWSOrganizationsSource :: Lens.Lens' ResourceDataSyncSourceWithState (Lude.Maybe ResourceDataSyncAWSOrganizationsSource)
rdsswsAWSOrganizationsSource = Lens.lens (awsOrganizationsSource :: ResourceDataSyncSourceWithState -> Lude.Maybe ResourceDataSyncAWSOrganizationsSource) (\s a -> s {awsOrganizationsSource = a} :: ResourceDataSyncSourceWithState)
{-# DEPRECATED rdsswsAWSOrganizationsSource "Use generic-lens or generic-optics with 'awsOrganizationsSource' instead." #-}

-- | The @SyncSource@ AWS Regions included in the resource data sync.
--
-- /Note:/ Consider using 'sourceRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsswsSourceRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Lude.Maybe [Lude.Text])
rdsswsSourceRegions = Lens.lens (sourceRegions :: ResourceDataSyncSourceWithState -> Lude.Maybe [Lude.Text]) (\s a -> s {sourceRegions = a} :: ResourceDataSyncSourceWithState)
{-# DEPRECATED rdsswsSourceRegions "Use generic-lens or generic-optics with 'sourceRegions' instead." #-}

instance Lude.FromJSON ResourceDataSyncSourceWithState where
  parseJSON =
    Lude.withObject
      "ResourceDataSyncSourceWithState"
      ( \x ->
          ResourceDataSyncSourceWithState'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "IncludeFutureRegions")
            Lude.<*> (x Lude..:? "SourceType")
            Lude.<*> (x Lude..:? "AwsOrganizationsSource")
            Lude.<*> (x Lude..:? "SourceRegions" Lude..!= Lude.mempty)
      )

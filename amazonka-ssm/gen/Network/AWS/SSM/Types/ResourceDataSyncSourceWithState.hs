{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncSourceWithState where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource

-- | The data type name for including resource data sync state. There are
-- four sync states:
--
-- @OrganizationNotExists@ (Your organization doesn\'t exist)
--
-- @NoPermissions@ (The system can\'t locate the service-linked role. This
-- role is automatically created when a user creates a resource data sync
-- in Explorer.)
--
-- @InvalidOrganizationalUnit@ (You specified or selected an invalid unit
-- in the resource data sync configuration.)
--
-- @TrustedAccessDisabled@ (You disabled Systems Manager access in the
-- organization in AWS Organizations.)
--
-- /See:/ 'newResourceDataSyncSourceWithState' smart constructor.
data ResourceDataSyncSourceWithState = ResourceDataSyncSourceWithState'
  { -- | Whether to automatically synchronize and aggregate data from new AWS
    -- Regions when those Regions come online.
    includeFutureRegions :: Core.Maybe Core.Bool,
    -- | The data type name for including resource data sync state. There are
    -- four sync states:
    --
    -- @OrganizationNotExists@: Your organization doesn\'t exist.
    --
    -- @NoPermissions@: The system can\'t locate the service-linked role. This
    -- role is automatically created when a user creates a resource data sync
    -- in Explorer.
    --
    -- @InvalidOrganizationalUnit@: You specified or selected an invalid unit
    -- in the resource data sync configuration.
    --
    -- @TrustedAccessDisabled@: You disabled Systems Manager access in the
    -- organization in AWS Organizations.
    state :: Core.Maybe Core.Text,
    -- | The @SyncSource@ AWS Regions included in the resource data sync.
    sourceRegions :: Core.Maybe [Core.Text],
    -- | The field name in @SyncSource@ for the
    -- @ResourceDataSyncAwsOrganizationsSource@ type.
    awsOrganizationsSource :: Core.Maybe ResourceDataSyncAwsOrganizationsSource,
    -- | The type of data source for the resource data sync. @SourceType@ is
    -- either @AwsOrganizations@ (if an organization is present in AWS
    -- Organizations) or @singleAccountMultiRegions@.
    sourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceDataSyncSourceWithState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeFutureRegions', 'resourceDataSyncSourceWithState_includeFutureRegions' - Whether to automatically synchronize and aggregate data from new AWS
-- Regions when those Regions come online.
--
-- 'state', 'resourceDataSyncSourceWithState_state' - The data type name for including resource data sync state. There are
-- four sync states:
--
-- @OrganizationNotExists@: Your organization doesn\'t exist.
--
-- @NoPermissions@: The system can\'t locate the service-linked role. This
-- role is automatically created when a user creates a resource data sync
-- in Explorer.
--
-- @InvalidOrganizationalUnit@: You specified or selected an invalid unit
-- in the resource data sync configuration.
--
-- @TrustedAccessDisabled@: You disabled Systems Manager access in the
-- organization in AWS Organizations.
--
-- 'sourceRegions', 'resourceDataSyncSourceWithState_sourceRegions' - The @SyncSource@ AWS Regions included in the resource data sync.
--
-- 'awsOrganizationsSource', 'resourceDataSyncSourceWithState_awsOrganizationsSource' - The field name in @SyncSource@ for the
-- @ResourceDataSyncAwsOrganizationsSource@ type.
--
-- 'sourceType', 'resourceDataSyncSourceWithState_sourceType' - The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in AWS
-- Organizations) or @singleAccountMultiRegions@.
newResourceDataSyncSourceWithState ::
  ResourceDataSyncSourceWithState
newResourceDataSyncSourceWithState =
  ResourceDataSyncSourceWithState'
    { includeFutureRegions =
        Core.Nothing,
      state = Core.Nothing,
      sourceRegions = Core.Nothing,
      awsOrganizationsSource = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | Whether to automatically synchronize and aggregate data from new AWS
-- Regions when those Regions come online.
resourceDataSyncSourceWithState_includeFutureRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe Core.Bool)
resourceDataSyncSourceWithState_includeFutureRegions = Lens.lens (\ResourceDataSyncSourceWithState' {includeFutureRegions} -> includeFutureRegions) (\s@ResourceDataSyncSourceWithState' {} a -> s {includeFutureRegions = a} :: ResourceDataSyncSourceWithState)

-- | The data type name for including resource data sync state. There are
-- four sync states:
--
-- @OrganizationNotExists@: Your organization doesn\'t exist.
--
-- @NoPermissions@: The system can\'t locate the service-linked role. This
-- role is automatically created when a user creates a resource data sync
-- in Explorer.
--
-- @InvalidOrganizationalUnit@: You specified or selected an invalid unit
-- in the resource data sync configuration.
--
-- @TrustedAccessDisabled@: You disabled Systems Manager access in the
-- organization in AWS Organizations.
resourceDataSyncSourceWithState_state :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe Core.Text)
resourceDataSyncSourceWithState_state = Lens.lens (\ResourceDataSyncSourceWithState' {state} -> state) (\s@ResourceDataSyncSourceWithState' {} a -> s {state = a} :: ResourceDataSyncSourceWithState)

-- | The @SyncSource@ AWS Regions included in the resource data sync.
resourceDataSyncSourceWithState_sourceRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe [Core.Text])
resourceDataSyncSourceWithState_sourceRegions = Lens.lens (\ResourceDataSyncSourceWithState' {sourceRegions} -> sourceRegions) (\s@ResourceDataSyncSourceWithState' {} a -> s {sourceRegions = a} :: ResourceDataSyncSourceWithState) Core.. Lens.mapping Lens._Coerce

-- | The field name in @SyncSource@ for the
-- @ResourceDataSyncAwsOrganizationsSource@ type.
resourceDataSyncSourceWithState_awsOrganizationsSource :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe ResourceDataSyncAwsOrganizationsSource)
resourceDataSyncSourceWithState_awsOrganizationsSource = Lens.lens (\ResourceDataSyncSourceWithState' {awsOrganizationsSource} -> awsOrganizationsSource) (\s@ResourceDataSyncSourceWithState' {} a -> s {awsOrganizationsSource = a} :: ResourceDataSyncSourceWithState)

-- | The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in AWS
-- Organizations) or @singleAccountMultiRegions@.
resourceDataSyncSourceWithState_sourceType :: Lens.Lens' ResourceDataSyncSourceWithState (Core.Maybe Core.Text)
resourceDataSyncSourceWithState_sourceType = Lens.lens (\ResourceDataSyncSourceWithState' {sourceType} -> sourceType) (\s@ResourceDataSyncSourceWithState' {} a -> s {sourceType = a} :: ResourceDataSyncSourceWithState)

instance
  Core.FromJSON
    ResourceDataSyncSourceWithState
  where
  parseJSON =
    Core.withObject
      "ResourceDataSyncSourceWithState"
      ( \x ->
          ResourceDataSyncSourceWithState'
            Core.<$> (x Core..:? "IncludeFutureRegions")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "SourceRegions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AwsOrganizationsSource")
            Core.<*> (x Core..:? "SourceType")
      )

instance
  Core.Hashable
    ResourceDataSyncSourceWithState

instance Core.NFData ResourceDataSyncSourceWithState

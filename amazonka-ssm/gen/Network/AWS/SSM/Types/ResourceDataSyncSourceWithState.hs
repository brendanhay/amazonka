{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    includeFutureRegions :: Prelude.Maybe Prelude.Bool,
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
    state :: Prelude.Maybe Prelude.Text,
    -- | The @SyncSource@ AWS Regions included in the resource data sync.
    sourceRegions :: Prelude.Maybe [Prelude.Text],
    -- | The field name in @SyncSource@ for the
    -- @ResourceDataSyncAwsOrganizationsSource@ type.
    awsOrganizationsSource :: Prelude.Maybe ResourceDataSyncAwsOrganizationsSource,
    -- | The type of data source for the resource data sync. @SourceType@ is
    -- either @AwsOrganizations@ (if an organization is present in AWS
    -- Organizations) or @singleAccountMultiRegions@.
    sourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      state = Prelude.Nothing,
      sourceRegions = Prelude.Nothing,
      awsOrganizationsSource = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | Whether to automatically synchronize and aggregate data from new AWS
-- Regions when those Regions come online.
resourceDataSyncSourceWithState_includeFutureRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe Prelude.Bool)
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
resourceDataSyncSourceWithState_state :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe Prelude.Text)
resourceDataSyncSourceWithState_state = Lens.lens (\ResourceDataSyncSourceWithState' {state} -> state) (\s@ResourceDataSyncSourceWithState' {} a -> s {state = a} :: ResourceDataSyncSourceWithState)

-- | The @SyncSource@ AWS Regions included in the resource data sync.
resourceDataSyncSourceWithState_sourceRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe [Prelude.Text])
resourceDataSyncSourceWithState_sourceRegions = Lens.lens (\ResourceDataSyncSourceWithState' {sourceRegions} -> sourceRegions) (\s@ResourceDataSyncSourceWithState' {} a -> s {sourceRegions = a} :: ResourceDataSyncSourceWithState) Prelude.. Lens.mapping Prelude._Coerce

-- | The field name in @SyncSource@ for the
-- @ResourceDataSyncAwsOrganizationsSource@ type.
resourceDataSyncSourceWithState_awsOrganizationsSource :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe ResourceDataSyncAwsOrganizationsSource)
resourceDataSyncSourceWithState_awsOrganizationsSource = Lens.lens (\ResourceDataSyncSourceWithState' {awsOrganizationsSource} -> awsOrganizationsSource) (\s@ResourceDataSyncSourceWithState' {} a -> s {awsOrganizationsSource = a} :: ResourceDataSyncSourceWithState)

-- | The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in AWS
-- Organizations) or @singleAccountMultiRegions@.
resourceDataSyncSourceWithState_sourceType :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe Prelude.Text)
resourceDataSyncSourceWithState_sourceType = Lens.lens (\ResourceDataSyncSourceWithState' {sourceType} -> sourceType) (\s@ResourceDataSyncSourceWithState' {} a -> s {sourceType = a} :: ResourceDataSyncSourceWithState)

instance
  Prelude.FromJSON
    ResourceDataSyncSourceWithState
  where
  parseJSON =
    Prelude.withObject
      "ResourceDataSyncSourceWithState"
      ( \x ->
          ResourceDataSyncSourceWithState'
            Prelude.<$> (x Prelude..:? "IncludeFutureRegions")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> ( x Prelude..:? "SourceRegions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AwsOrganizationsSource")
            Prelude.<*> (x Prelude..:? "SourceType")
      )

instance
  Prelude.Hashable
    ResourceDataSyncSourceWithState

instance
  Prelude.NFData
    ResourceDataSyncSourceWithState

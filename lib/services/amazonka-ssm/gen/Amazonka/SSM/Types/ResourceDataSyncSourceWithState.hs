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
-- Module      : Amazonka.SSM.Types.ResourceDataSyncSourceWithState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceDataSyncSourceWithState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ResourceDataSyncAwsOrganizationsSource

-- | The data type name for including resource data sync state. There are
-- four sync states:
--
-- @OrganizationNotExists@ (Your organization doesn\'t exist)
--
-- @NoPermissions@ (The system can\'t locate the service-linked role. This
-- role is automatically created when a user creates a resource data sync
-- in Amazon Web Services Systems Manager Explorer.)
--
-- @InvalidOrganizationalUnit@ (You specified or selected an invalid unit
-- in the resource data sync configuration.)
--
-- @TrustedAccessDisabled@ (You disabled Systems Manager access in the
-- organization in Organizations.)
--
-- /See:/ 'newResourceDataSyncSourceWithState' smart constructor.
data ResourceDataSyncSourceWithState = ResourceDataSyncSourceWithState'
  { -- | The field name in @SyncSource@ for the
    -- @ResourceDataSyncAwsOrganizationsSource@ type.
    awsOrganizationsSource :: Prelude.Maybe ResourceDataSyncAwsOrganizationsSource,
    -- | When you create a resource data sync, if you choose one of the
    -- Organizations options, then Systems Manager automatically enables all
    -- OpsData sources in the selected Amazon Web Services Regions for all
    -- Amazon Web Services accounts in your organization (or in the selected
    -- organization units). For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resouce-data-sync-multiple-accounts-and-regions.html About multiple account and Region resource data syncs>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    enableAllOpsDataSources :: Prelude.Maybe Prelude.Bool,
    -- | Whether to automatically synchronize and aggregate data from new Amazon
    -- Web Services Regions when those Regions come online.
    includeFutureRegions :: Prelude.Maybe Prelude.Bool,
    -- | The @SyncSource@ Amazon Web Services Regions included in the resource
    -- data sync.
    sourceRegions :: Prelude.Maybe [Prelude.Text],
    -- | The type of data source for the resource data sync. @SourceType@ is
    -- either @AwsOrganizations@ (if an organization is present in
    -- Organizations) or @singleAccountMultiRegions@.
    sourceType :: Prelude.Maybe Prelude.Text,
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
    -- organization in Organizations.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncSourceWithState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsOrganizationsSource', 'resourceDataSyncSourceWithState_awsOrganizationsSource' - The field name in @SyncSource@ for the
-- @ResourceDataSyncAwsOrganizationsSource@ type.
--
-- 'enableAllOpsDataSources', 'resourceDataSyncSourceWithState_enableAllOpsDataSources' - When you create a resource data sync, if you choose one of the
-- Organizations options, then Systems Manager automatically enables all
-- OpsData sources in the selected Amazon Web Services Regions for all
-- Amazon Web Services accounts in your organization (or in the selected
-- organization units). For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resouce-data-sync-multiple-accounts-and-regions.html About multiple account and Region resource data syncs>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'includeFutureRegions', 'resourceDataSyncSourceWithState_includeFutureRegions' - Whether to automatically synchronize and aggregate data from new Amazon
-- Web Services Regions when those Regions come online.
--
-- 'sourceRegions', 'resourceDataSyncSourceWithState_sourceRegions' - The @SyncSource@ Amazon Web Services Regions included in the resource
-- data sync.
--
-- 'sourceType', 'resourceDataSyncSourceWithState_sourceType' - The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in
-- Organizations) or @singleAccountMultiRegions@.
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
-- organization in Organizations.
newResourceDataSyncSourceWithState ::
  ResourceDataSyncSourceWithState
newResourceDataSyncSourceWithState =
  ResourceDataSyncSourceWithState'
    { awsOrganizationsSource =
        Prelude.Nothing,
      enableAllOpsDataSources = Prelude.Nothing,
      includeFutureRegions = Prelude.Nothing,
      sourceRegions = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The field name in @SyncSource@ for the
-- @ResourceDataSyncAwsOrganizationsSource@ type.
resourceDataSyncSourceWithState_awsOrganizationsSource :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe ResourceDataSyncAwsOrganizationsSource)
resourceDataSyncSourceWithState_awsOrganizationsSource = Lens.lens (\ResourceDataSyncSourceWithState' {awsOrganizationsSource} -> awsOrganizationsSource) (\s@ResourceDataSyncSourceWithState' {} a -> s {awsOrganizationsSource = a} :: ResourceDataSyncSourceWithState)

-- | When you create a resource data sync, if you choose one of the
-- Organizations options, then Systems Manager automatically enables all
-- OpsData sources in the selected Amazon Web Services Regions for all
-- Amazon Web Services accounts in your organization (or in the selected
-- organization units). For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resouce-data-sync-multiple-accounts-and-regions.html About multiple account and Region resource data syncs>
-- in the /Amazon Web Services Systems Manager User Guide/.
resourceDataSyncSourceWithState_enableAllOpsDataSources :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe Prelude.Bool)
resourceDataSyncSourceWithState_enableAllOpsDataSources = Lens.lens (\ResourceDataSyncSourceWithState' {enableAllOpsDataSources} -> enableAllOpsDataSources) (\s@ResourceDataSyncSourceWithState' {} a -> s {enableAllOpsDataSources = a} :: ResourceDataSyncSourceWithState)

-- | Whether to automatically synchronize and aggregate data from new Amazon
-- Web Services Regions when those Regions come online.
resourceDataSyncSourceWithState_includeFutureRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe Prelude.Bool)
resourceDataSyncSourceWithState_includeFutureRegions = Lens.lens (\ResourceDataSyncSourceWithState' {includeFutureRegions} -> includeFutureRegions) (\s@ResourceDataSyncSourceWithState' {} a -> s {includeFutureRegions = a} :: ResourceDataSyncSourceWithState)

-- | The @SyncSource@ Amazon Web Services Regions included in the resource
-- data sync.
resourceDataSyncSourceWithState_sourceRegions :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe [Prelude.Text])
resourceDataSyncSourceWithState_sourceRegions = Lens.lens (\ResourceDataSyncSourceWithState' {sourceRegions} -> sourceRegions) (\s@ResourceDataSyncSourceWithState' {} a -> s {sourceRegions = a} :: ResourceDataSyncSourceWithState) Prelude.. Lens.mapping Lens.coerced

-- | The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in
-- Organizations) or @singleAccountMultiRegions@.
resourceDataSyncSourceWithState_sourceType :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe Prelude.Text)
resourceDataSyncSourceWithState_sourceType = Lens.lens (\ResourceDataSyncSourceWithState' {sourceType} -> sourceType) (\s@ResourceDataSyncSourceWithState' {} a -> s {sourceType = a} :: ResourceDataSyncSourceWithState)

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
-- organization in Organizations.
resourceDataSyncSourceWithState_state :: Lens.Lens' ResourceDataSyncSourceWithState (Prelude.Maybe Prelude.Text)
resourceDataSyncSourceWithState_state = Lens.lens (\ResourceDataSyncSourceWithState' {state} -> state) (\s@ResourceDataSyncSourceWithState' {} a -> s {state = a} :: ResourceDataSyncSourceWithState)

instance
  Data.FromJSON
    ResourceDataSyncSourceWithState
  where
  parseJSON =
    Data.withObject
      "ResourceDataSyncSourceWithState"
      ( \x ->
          ResourceDataSyncSourceWithState'
            Prelude.<$> (x Data..:? "AwsOrganizationsSource")
            Prelude.<*> (x Data..:? "EnableAllOpsDataSources")
            Prelude.<*> (x Data..:? "IncludeFutureRegions")
            Prelude.<*> (x Data..:? "SourceRegions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceType")
            Prelude.<*> (x Data..:? "State")
      )

instance
  Prelude.Hashable
    ResourceDataSyncSourceWithState
  where
  hashWithSalt
    _salt
    ResourceDataSyncSourceWithState' {..} =
      _salt `Prelude.hashWithSalt` awsOrganizationsSource
        `Prelude.hashWithSalt` enableAllOpsDataSources
        `Prelude.hashWithSalt` includeFutureRegions
        `Prelude.hashWithSalt` sourceRegions
        `Prelude.hashWithSalt` sourceType
        `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    ResourceDataSyncSourceWithState
  where
  rnf ResourceDataSyncSourceWithState' {..} =
    Prelude.rnf awsOrganizationsSource
      `Prelude.seq` Prelude.rnf enableAllOpsDataSources
      `Prelude.seq` Prelude.rnf includeFutureRegions
      `Prelude.seq` Prelude.rnf sourceRegions
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf state

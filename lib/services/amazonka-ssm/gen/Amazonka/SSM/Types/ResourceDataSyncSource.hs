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
-- Module      : Amazonka.SSM.Types.ResourceDataSyncSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceDataSyncSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ResourceDataSyncAwsOrganizationsSource

-- | Information about the source of the data included in the resource data
-- sync.
--
-- /See:/ 'newResourceDataSyncSource' smart constructor.
data ResourceDataSyncSource = ResourceDataSyncSource'
  { -- | Information about the @AwsOrganizationsSource@ resource data sync
    -- source. A sync source of this type can synchronize data from
    -- Organizations.
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
    -- | The type of data source for the resource data sync. @SourceType@ is
    -- either @AwsOrganizations@ (if an organization is present in
    -- Organizations) or @SingleAccountMultiRegions@.
    sourceType :: Prelude.Text,
    -- | The @SyncSource@ Amazon Web Services Regions included in the resource
    -- data sync.
    sourceRegions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsOrganizationsSource', 'resourceDataSyncSource_awsOrganizationsSource' - Information about the @AwsOrganizationsSource@ resource data sync
-- source. A sync source of this type can synchronize data from
-- Organizations.
--
-- 'enableAllOpsDataSources', 'resourceDataSyncSource_enableAllOpsDataSources' - When you create a resource data sync, if you choose one of the
-- Organizations options, then Systems Manager automatically enables all
-- OpsData sources in the selected Amazon Web Services Regions for all
-- Amazon Web Services accounts in your organization (or in the selected
-- organization units). For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resouce-data-sync-multiple-accounts-and-regions.html About multiple account and Region resource data syncs>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'includeFutureRegions', 'resourceDataSyncSource_includeFutureRegions' - Whether to automatically synchronize and aggregate data from new Amazon
-- Web Services Regions when those Regions come online.
--
-- 'sourceType', 'resourceDataSyncSource_sourceType' - The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in
-- Organizations) or @SingleAccountMultiRegions@.
--
-- 'sourceRegions', 'resourceDataSyncSource_sourceRegions' - The @SyncSource@ Amazon Web Services Regions included in the resource
-- data sync.
newResourceDataSyncSource ::
  -- | 'sourceType'
  Prelude.Text ->
  ResourceDataSyncSource
newResourceDataSyncSource pSourceType_ =
  ResourceDataSyncSource'
    { awsOrganizationsSource =
        Prelude.Nothing,
      enableAllOpsDataSources = Prelude.Nothing,
      includeFutureRegions = Prelude.Nothing,
      sourceType = pSourceType_,
      sourceRegions = Prelude.mempty
    }

-- | Information about the @AwsOrganizationsSource@ resource data sync
-- source. A sync source of this type can synchronize data from
-- Organizations.
resourceDataSyncSource_awsOrganizationsSource :: Lens.Lens' ResourceDataSyncSource (Prelude.Maybe ResourceDataSyncAwsOrganizationsSource)
resourceDataSyncSource_awsOrganizationsSource = Lens.lens (\ResourceDataSyncSource' {awsOrganizationsSource} -> awsOrganizationsSource) (\s@ResourceDataSyncSource' {} a -> s {awsOrganizationsSource = a} :: ResourceDataSyncSource)

-- | When you create a resource data sync, if you choose one of the
-- Organizations options, then Systems Manager automatically enables all
-- OpsData sources in the selected Amazon Web Services Regions for all
-- Amazon Web Services accounts in your organization (or in the selected
-- organization units). For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resouce-data-sync-multiple-accounts-and-regions.html About multiple account and Region resource data syncs>
-- in the /Amazon Web Services Systems Manager User Guide/.
resourceDataSyncSource_enableAllOpsDataSources :: Lens.Lens' ResourceDataSyncSource (Prelude.Maybe Prelude.Bool)
resourceDataSyncSource_enableAllOpsDataSources = Lens.lens (\ResourceDataSyncSource' {enableAllOpsDataSources} -> enableAllOpsDataSources) (\s@ResourceDataSyncSource' {} a -> s {enableAllOpsDataSources = a} :: ResourceDataSyncSource)

-- | Whether to automatically synchronize and aggregate data from new Amazon
-- Web Services Regions when those Regions come online.
resourceDataSyncSource_includeFutureRegions :: Lens.Lens' ResourceDataSyncSource (Prelude.Maybe Prelude.Bool)
resourceDataSyncSource_includeFutureRegions = Lens.lens (\ResourceDataSyncSource' {includeFutureRegions} -> includeFutureRegions) (\s@ResourceDataSyncSource' {} a -> s {includeFutureRegions = a} :: ResourceDataSyncSource)

-- | The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in
-- Organizations) or @SingleAccountMultiRegions@.
resourceDataSyncSource_sourceType :: Lens.Lens' ResourceDataSyncSource Prelude.Text
resourceDataSyncSource_sourceType = Lens.lens (\ResourceDataSyncSource' {sourceType} -> sourceType) (\s@ResourceDataSyncSource' {} a -> s {sourceType = a} :: ResourceDataSyncSource)

-- | The @SyncSource@ Amazon Web Services Regions included in the resource
-- data sync.
resourceDataSyncSource_sourceRegions :: Lens.Lens' ResourceDataSyncSource [Prelude.Text]
resourceDataSyncSource_sourceRegions = Lens.lens (\ResourceDataSyncSource' {sourceRegions} -> sourceRegions) (\s@ResourceDataSyncSource' {} a -> s {sourceRegions = a} :: ResourceDataSyncSource) Prelude.. Lens.coerced

instance Prelude.Hashable ResourceDataSyncSource where
  hashWithSalt _salt ResourceDataSyncSource' {..} =
    _salt `Prelude.hashWithSalt` awsOrganizationsSource
      `Prelude.hashWithSalt` enableAllOpsDataSources
      `Prelude.hashWithSalt` includeFutureRegions
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceRegions

instance Prelude.NFData ResourceDataSyncSource where
  rnf ResourceDataSyncSource' {..} =
    Prelude.rnf awsOrganizationsSource
      `Prelude.seq` Prelude.rnf enableAllOpsDataSources
      `Prelude.seq` Prelude.rnf includeFutureRegions
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceRegions

instance Data.ToJSON ResourceDataSyncSource where
  toJSON ResourceDataSyncSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AwsOrganizationsSource" Data..=)
              Prelude.<$> awsOrganizationsSource,
            ("EnableAllOpsDataSources" Data..=)
              Prelude.<$> enableAllOpsDataSources,
            ("IncludeFutureRegions" Data..=)
              Prelude.<$> includeFutureRegions,
            Prelude.Just ("SourceType" Data..= sourceType),
            Prelude.Just
              ("SourceRegions" Data..= sourceRegions)
          ]
      )

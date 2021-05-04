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
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource

-- | Information about the source of the data included in the resource data
-- sync.
--
-- /See:/ 'newResourceDataSyncSource' smart constructor.
data ResourceDataSyncSource = ResourceDataSyncSource'
  { -- | Whether to automatically synchronize and aggregate data from new AWS
    -- Regions when those Regions come online.
    includeFutureRegions :: Prelude.Maybe Prelude.Bool,
    -- | Information about the AwsOrganizationsSource resource data sync source.
    -- A sync source of this type can synchronize data from AWS Organizations.
    awsOrganizationsSource :: Prelude.Maybe ResourceDataSyncAwsOrganizationsSource,
    -- | The type of data source for the resource data sync. @SourceType@ is
    -- either @AwsOrganizations@ (if an organization is present in AWS
    -- Organizations) or @singleAccountMultiRegions@.
    sourceType :: Prelude.Text,
    -- | The @SyncSource@ AWS Regions included in the resource data sync.
    sourceRegions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeFutureRegions', 'resourceDataSyncSource_includeFutureRegions' - Whether to automatically synchronize and aggregate data from new AWS
-- Regions when those Regions come online.
--
-- 'awsOrganizationsSource', 'resourceDataSyncSource_awsOrganizationsSource' - Information about the AwsOrganizationsSource resource data sync source.
-- A sync source of this type can synchronize data from AWS Organizations.
--
-- 'sourceType', 'resourceDataSyncSource_sourceType' - The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in AWS
-- Organizations) or @singleAccountMultiRegions@.
--
-- 'sourceRegions', 'resourceDataSyncSource_sourceRegions' - The @SyncSource@ AWS Regions included in the resource data sync.
newResourceDataSyncSource ::
  -- | 'sourceType'
  Prelude.Text ->
  ResourceDataSyncSource
newResourceDataSyncSource pSourceType_ =
  ResourceDataSyncSource'
    { includeFutureRegions =
        Prelude.Nothing,
      awsOrganizationsSource = Prelude.Nothing,
      sourceType = pSourceType_,
      sourceRegions = Prelude.mempty
    }

-- | Whether to automatically synchronize and aggregate data from new AWS
-- Regions when those Regions come online.
resourceDataSyncSource_includeFutureRegions :: Lens.Lens' ResourceDataSyncSource (Prelude.Maybe Prelude.Bool)
resourceDataSyncSource_includeFutureRegions = Lens.lens (\ResourceDataSyncSource' {includeFutureRegions} -> includeFutureRegions) (\s@ResourceDataSyncSource' {} a -> s {includeFutureRegions = a} :: ResourceDataSyncSource)

-- | Information about the AwsOrganizationsSource resource data sync source.
-- A sync source of this type can synchronize data from AWS Organizations.
resourceDataSyncSource_awsOrganizationsSource :: Lens.Lens' ResourceDataSyncSource (Prelude.Maybe ResourceDataSyncAwsOrganizationsSource)
resourceDataSyncSource_awsOrganizationsSource = Lens.lens (\ResourceDataSyncSource' {awsOrganizationsSource} -> awsOrganizationsSource) (\s@ResourceDataSyncSource' {} a -> s {awsOrganizationsSource = a} :: ResourceDataSyncSource)

-- | The type of data source for the resource data sync. @SourceType@ is
-- either @AwsOrganizations@ (if an organization is present in AWS
-- Organizations) or @singleAccountMultiRegions@.
resourceDataSyncSource_sourceType :: Lens.Lens' ResourceDataSyncSource Prelude.Text
resourceDataSyncSource_sourceType = Lens.lens (\ResourceDataSyncSource' {sourceType} -> sourceType) (\s@ResourceDataSyncSource' {} a -> s {sourceType = a} :: ResourceDataSyncSource)

-- | The @SyncSource@ AWS Regions included in the resource data sync.
resourceDataSyncSource_sourceRegions :: Lens.Lens' ResourceDataSyncSource [Prelude.Text]
resourceDataSyncSource_sourceRegions = Lens.lens (\ResourceDataSyncSource' {sourceRegions} -> sourceRegions) (\s@ResourceDataSyncSource' {} a -> s {sourceRegions = a} :: ResourceDataSyncSource) Prelude.. Prelude._Coerce

instance Prelude.Hashable ResourceDataSyncSource

instance Prelude.NFData ResourceDataSyncSource

instance Prelude.ToJSON ResourceDataSyncSource where
  toJSON ResourceDataSyncSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IncludeFutureRegions" Prelude..=)
              Prelude.<$> includeFutureRegions,
            ("AwsOrganizationsSource" Prelude..=)
              Prelude.<$> awsOrganizationsSource,
            Prelude.Just ("SourceType" Prelude..= sourceType),
            Prelude.Just
              ("SourceRegions" Prelude..= sourceRegions)
          ]
      )

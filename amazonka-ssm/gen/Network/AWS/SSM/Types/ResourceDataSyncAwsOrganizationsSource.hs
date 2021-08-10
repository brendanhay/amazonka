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
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit

-- | Information about the AwsOrganizationsSource resource data sync source.
-- A sync source of this type can synchronize data from AWS Organizations
-- or, if an AWS Organization is not present, from multiple AWS Regions.
--
-- /See:/ 'newResourceDataSyncAwsOrganizationsSource' smart constructor.
data ResourceDataSyncAwsOrganizationsSource = ResourceDataSyncAwsOrganizationsSource'
  { -- | The AWS Organizations organization units included in the sync.
    organizationalUnits :: Prelude.Maybe (Prelude.NonEmpty ResourceDataSyncOrganizationalUnit),
    -- | If an AWS Organization is present, this is either @OrganizationalUnits@
    -- or @EntireOrganization@. For @OrganizationalUnits@, the data is
    -- aggregated from a set of organization units. For @EntireOrganization@,
    -- the data is aggregated from the entire AWS Organization.
    organizationSourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncAwsOrganizationsSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnits', 'resourceDataSyncAwsOrganizationsSource_organizationalUnits' - The AWS Organizations organization units included in the sync.
--
-- 'organizationSourceType', 'resourceDataSyncAwsOrganizationsSource_organizationSourceType' - If an AWS Organization is present, this is either @OrganizationalUnits@
-- or @EntireOrganization@. For @OrganizationalUnits@, the data is
-- aggregated from a set of organization units. For @EntireOrganization@,
-- the data is aggregated from the entire AWS Organization.
newResourceDataSyncAwsOrganizationsSource ::
  -- | 'organizationSourceType'
  Prelude.Text ->
  ResourceDataSyncAwsOrganizationsSource
newResourceDataSyncAwsOrganizationsSource
  pOrganizationSourceType_ =
    ResourceDataSyncAwsOrganizationsSource'
      { organizationalUnits =
          Prelude.Nothing,
        organizationSourceType =
          pOrganizationSourceType_
      }

-- | The AWS Organizations organization units included in the sync.
resourceDataSyncAwsOrganizationsSource_organizationalUnits :: Lens.Lens' ResourceDataSyncAwsOrganizationsSource (Prelude.Maybe (Prelude.NonEmpty ResourceDataSyncOrganizationalUnit))
resourceDataSyncAwsOrganizationsSource_organizationalUnits = Lens.lens (\ResourceDataSyncAwsOrganizationsSource' {organizationalUnits} -> organizationalUnits) (\s@ResourceDataSyncAwsOrganizationsSource' {} a -> s {organizationalUnits = a} :: ResourceDataSyncAwsOrganizationsSource) Prelude.. Lens.mapping Lens._Coerce

-- | If an AWS Organization is present, this is either @OrganizationalUnits@
-- or @EntireOrganization@. For @OrganizationalUnits@, the data is
-- aggregated from a set of organization units. For @EntireOrganization@,
-- the data is aggregated from the entire AWS Organization.
resourceDataSyncAwsOrganizationsSource_organizationSourceType :: Lens.Lens' ResourceDataSyncAwsOrganizationsSource Prelude.Text
resourceDataSyncAwsOrganizationsSource_organizationSourceType = Lens.lens (\ResourceDataSyncAwsOrganizationsSource' {organizationSourceType} -> organizationSourceType) (\s@ResourceDataSyncAwsOrganizationsSource' {} a -> s {organizationSourceType = a} :: ResourceDataSyncAwsOrganizationsSource)

instance
  Core.FromJSON
    ResourceDataSyncAwsOrganizationsSource
  where
  parseJSON =
    Core.withObject
      "ResourceDataSyncAwsOrganizationsSource"
      ( \x ->
          ResourceDataSyncAwsOrganizationsSource'
            Prelude.<$> (x Core..:? "OrganizationalUnits")
            Prelude.<*> (x Core..: "OrganizationSourceType")
      )

instance
  Prelude.Hashable
    ResourceDataSyncAwsOrganizationsSource

instance
  Prelude.NFData
    ResourceDataSyncAwsOrganizationsSource

instance
  Core.ToJSON
    ResourceDataSyncAwsOrganizationsSource
  where
  toJSON ResourceDataSyncAwsOrganizationsSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OrganizationalUnits" Core..=)
              Prelude.<$> organizationalUnits,
            Prelude.Just
              ( "OrganizationSourceType"
                  Core..= organizationSourceType
              )
          ]
      )

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
-- Module      : Amazonka.SSM.Types.ResourceDataSyncAwsOrganizationsSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceDataSyncAwsOrganizationsSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ResourceDataSyncOrganizationalUnit

-- | Information about the @AwsOrganizationsSource@ resource data sync
-- source. A sync source of this type can synchronize data from
-- Organizations or, if an Amazon Web Services organization isn\'t present,
-- from multiple Amazon Web Services Regions.
--
-- /See:/ 'newResourceDataSyncAwsOrganizationsSource' smart constructor.
data ResourceDataSyncAwsOrganizationsSource = ResourceDataSyncAwsOrganizationsSource'
  { -- | The Organizations organization units included in the sync.
    organizationalUnits :: Prelude.Maybe (Prelude.NonEmpty ResourceDataSyncOrganizationalUnit),
    -- | If an Amazon Web Services organization is present, this is either
    -- @OrganizationalUnits@ or @EntireOrganization@. For
    -- @OrganizationalUnits@, the data is aggregated from a set of organization
    -- units. For @EntireOrganization@, the data is aggregated from the entire
    -- Amazon Web Services organization.
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
-- 'organizationalUnits', 'resourceDataSyncAwsOrganizationsSource_organizationalUnits' - The Organizations organization units included in the sync.
--
-- 'organizationSourceType', 'resourceDataSyncAwsOrganizationsSource_organizationSourceType' - If an Amazon Web Services organization is present, this is either
-- @OrganizationalUnits@ or @EntireOrganization@. For
-- @OrganizationalUnits@, the data is aggregated from a set of organization
-- units. For @EntireOrganization@, the data is aggregated from the entire
-- Amazon Web Services organization.
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

-- | The Organizations organization units included in the sync.
resourceDataSyncAwsOrganizationsSource_organizationalUnits :: Lens.Lens' ResourceDataSyncAwsOrganizationsSource (Prelude.Maybe (Prelude.NonEmpty ResourceDataSyncOrganizationalUnit))
resourceDataSyncAwsOrganizationsSource_organizationalUnits = Lens.lens (\ResourceDataSyncAwsOrganizationsSource' {organizationalUnits} -> organizationalUnits) (\s@ResourceDataSyncAwsOrganizationsSource' {} a -> s {organizationalUnits = a} :: ResourceDataSyncAwsOrganizationsSource) Prelude.. Lens.mapping Lens.coerced

-- | If an Amazon Web Services organization is present, this is either
-- @OrganizationalUnits@ or @EntireOrganization@. For
-- @OrganizationalUnits@, the data is aggregated from a set of organization
-- units. For @EntireOrganization@, the data is aggregated from the entire
-- Amazon Web Services organization.
resourceDataSyncAwsOrganizationsSource_organizationSourceType :: Lens.Lens' ResourceDataSyncAwsOrganizationsSource Prelude.Text
resourceDataSyncAwsOrganizationsSource_organizationSourceType = Lens.lens (\ResourceDataSyncAwsOrganizationsSource' {organizationSourceType} -> organizationSourceType) (\s@ResourceDataSyncAwsOrganizationsSource' {} a -> s {organizationSourceType = a} :: ResourceDataSyncAwsOrganizationsSource)

instance
  Data.FromJSON
    ResourceDataSyncAwsOrganizationsSource
  where
  parseJSON =
    Data.withObject
      "ResourceDataSyncAwsOrganizationsSource"
      ( \x ->
          ResourceDataSyncAwsOrganizationsSource'
            Prelude.<$> (x Data..:? "OrganizationalUnits")
            Prelude.<*> (x Data..: "OrganizationSourceType")
      )

instance
  Prelude.Hashable
    ResourceDataSyncAwsOrganizationsSource
  where
  hashWithSalt
    _salt
    ResourceDataSyncAwsOrganizationsSource' {..} =
      _salt `Prelude.hashWithSalt` organizationalUnits
        `Prelude.hashWithSalt` organizationSourceType

instance
  Prelude.NFData
    ResourceDataSyncAwsOrganizationsSource
  where
  rnf ResourceDataSyncAwsOrganizationsSource' {..} =
    Prelude.rnf organizationalUnits
      `Prelude.seq` Prelude.rnf organizationSourceType

instance
  Data.ToJSON
    ResourceDataSyncAwsOrganizationsSource
  where
  toJSON ResourceDataSyncAwsOrganizationsSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OrganizationalUnits" Data..=)
              Prelude.<$> organizationalUnits,
            Prelude.Just
              ( "OrganizationSourceType"
                  Data..= organizationSourceType
              )
          ]
      )

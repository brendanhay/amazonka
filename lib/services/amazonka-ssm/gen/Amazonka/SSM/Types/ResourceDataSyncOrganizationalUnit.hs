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
-- Module      : Amazonka.SSM.Types.ResourceDataSyncOrganizationalUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceDataSyncOrganizationalUnit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Organizations organizational unit data source for the sync.
--
-- /See:/ 'newResourceDataSyncOrganizationalUnit' smart constructor.
data ResourceDataSyncOrganizationalUnit = ResourceDataSyncOrganizationalUnit'
  { -- | The Organizations unit ID data source for the sync.
    organizationalUnitId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncOrganizationalUnit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnitId', 'resourceDataSyncOrganizationalUnit_organizationalUnitId' - The Organizations unit ID data source for the sync.
newResourceDataSyncOrganizationalUnit ::
  ResourceDataSyncOrganizationalUnit
newResourceDataSyncOrganizationalUnit =
  ResourceDataSyncOrganizationalUnit'
    { organizationalUnitId =
        Prelude.Nothing
    }

-- | The Organizations unit ID data source for the sync.
resourceDataSyncOrganizationalUnit_organizationalUnitId :: Lens.Lens' ResourceDataSyncOrganizationalUnit (Prelude.Maybe Prelude.Text)
resourceDataSyncOrganizationalUnit_organizationalUnitId = Lens.lens (\ResourceDataSyncOrganizationalUnit' {organizationalUnitId} -> organizationalUnitId) (\s@ResourceDataSyncOrganizationalUnit' {} a -> s {organizationalUnitId = a} :: ResourceDataSyncOrganizationalUnit)

instance
  Data.FromJSON
    ResourceDataSyncOrganizationalUnit
  where
  parseJSON =
    Data.withObject
      "ResourceDataSyncOrganizationalUnit"
      ( \x ->
          ResourceDataSyncOrganizationalUnit'
            Prelude.<$> (x Data..:? "OrganizationalUnitId")
      )

instance
  Prelude.Hashable
    ResourceDataSyncOrganizationalUnit
  where
  hashWithSalt
    _salt
    ResourceDataSyncOrganizationalUnit' {..} =
      _salt `Prelude.hashWithSalt` organizationalUnitId

instance
  Prelude.NFData
    ResourceDataSyncOrganizationalUnit
  where
  rnf ResourceDataSyncOrganizationalUnit' {..} =
    Prelude.rnf organizationalUnitId

instance
  Data.ToJSON
    ResourceDataSyncOrganizationalUnit
  where
  toJSON ResourceDataSyncOrganizationalUnit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OrganizationalUnitId" Data..=)
              Prelude.<$> organizationalUnitId
          ]
      )

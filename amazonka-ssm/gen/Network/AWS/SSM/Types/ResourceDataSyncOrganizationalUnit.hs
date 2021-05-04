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
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The AWS Organizations organizational unit data source for the sync.
--
-- /See:/ 'newResourceDataSyncOrganizationalUnit' smart constructor.
data ResourceDataSyncOrganizationalUnit = ResourceDataSyncOrganizationalUnit'
  { -- | The AWS Organization unit ID data source for the sync.
    organizationalUnitId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncOrganizationalUnit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnitId', 'resourceDataSyncOrganizationalUnit_organizationalUnitId' - The AWS Organization unit ID data source for the sync.
newResourceDataSyncOrganizationalUnit ::
  ResourceDataSyncOrganizationalUnit
newResourceDataSyncOrganizationalUnit =
  ResourceDataSyncOrganizationalUnit'
    { organizationalUnitId =
        Prelude.Nothing
    }

-- | The AWS Organization unit ID data source for the sync.
resourceDataSyncOrganizationalUnit_organizationalUnitId :: Lens.Lens' ResourceDataSyncOrganizationalUnit (Prelude.Maybe Prelude.Text)
resourceDataSyncOrganizationalUnit_organizationalUnitId = Lens.lens (\ResourceDataSyncOrganizationalUnit' {organizationalUnitId} -> organizationalUnitId) (\s@ResourceDataSyncOrganizationalUnit' {} a -> s {organizationalUnitId = a} :: ResourceDataSyncOrganizationalUnit)

instance
  Prelude.FromJSON
    ResourceDataSyncOrganizationalUnit
  where
  parseJSON =
    Prelude.withObject
      "ResourceDataSyncOrganizationalUnit"
      ( \x ->
          ResourceDataSyncOrganizationalUnit'
            Prelude.<$> (x Prelude..:? "OrganizationalUnitId")
      )

instance
  Prelude.Hashable
    ResourceDataSyncOrganizationalUnit

instance
  Prelude.NFData
    ResourceDataSyncOrganizationalUnit

instance
  Prelude.ToJSON
    ResourceDataSyncOrganizationalUnit
  where
  toJSON ResourceDataSyncOrganizationalUnit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OrganizationalUnitId" Prelude..=)
              Prelude.<$> organizationalUnitId
          ]
      )

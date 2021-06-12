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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The AWS Organizations organizational unit data source for the sync.
--
-- /See:/ 'newResourceDataSyncOrganizationalUnit' smart constructor.
data ResourceDataSyncOrganizationalUnit = ResourceDataSyncOrganizationalUnit'
  { -- | The AWS Organization unit ID data source for the sync.
    organizationalUnitId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The AWS Organization unit ID data source for the sync.
resourceDataSyncOrganizationalUnit_organizationalUnitId :: Lens.Lens' ResourceDataSyncOrganizationalUnit (Core.Maybe Core.Text)
resourceDataSyncOrganizationalUnit_organizationalUnitId = Lens.lens (\ResourceDataSyncOrganizationalUnit' {organizationalUnitId} -> organizationalUnitId) (\s@ResourceDataSyncOrganizationalUnit' {} a -> s {organizationalUnitId = a} :: ResourceDataSyncOrganizationalUnit)

instance
  Core.FromJSON
    ResourceDataSyncOrganizationalUnit
  where
  parseJSON =
    Core.withObject
      "ResourceDataSyncOrganizationalUnit"
      ( \x ->
          ResourceDataSyncOrganizationalUnit'
            Core.<$> (x Core..:? "OrganizationalUnitId")
      )

instance
  Core.Hashable
    ResourceDataSyncOrganizationalUnit

instance
  Core.NFData
    ResourceDataSyncOrganizationalUnit

instance
  Core.ToJSON
    ResourceDataSyncOrganizationalUnit
  where
  toJSON ResourceDataSyncOrganizationalUnit' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OrganizationalUnitId" Core..=)
              Core.<$> organizationalUnitId
          ]
      )

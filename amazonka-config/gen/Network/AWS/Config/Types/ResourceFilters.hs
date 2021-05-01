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
-- Module      : Network.AWS.Config.Types.ResourceFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceFilters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters the results by resource account ID, region, resource ID, and
-- resource name.
--
-- /See:/ 'newResourceFilters' smart constructor.
data ResourceFilters = ResourceFilters'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit source account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'resourceFilters_resourceId' - The ID of the resource.
--
-- 'accountId', 'resourceFilters_accountId' - The 12-digit source account ID.
--
-- 'region', 'resourceFilters_region' - The source region.
--
-- 'resourceName', 'resourceFilters_resourceName' - The name of the resource.
newResourceFilters ::
  ResourceFilters
newResourceFilters =
  ResourceFilters'
    { resourceId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      region = Prelude.Nothing,
      resourceName = Prelude.Nothing
    }

-- | The ID of the resource.
resourceFilters_resourceId :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_resourceId = Lens.lens (\ResourceFilters' {resourceId} -> resourceId) (\s@ResourceFilters' {} a -> s {resourceId = a} :: ResourceFilters)

-- | The 12-digit source account ID.
resourceFilters_accountId :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_accountId = Lens.lens (\ResourceFilters' {accountId} -> accountId) (\s@ResourceFilters' {} a -> s {accountId = a} :: ResourceFilters)

-- | The source region.
resourceFilters_region :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_region = Lens.lens (\ResourceFilters' {region} -> region) (\s@ResourceFilters' {} a -> s {region = a} :: ResourceFilters)

-- | The name of the resource.
resourceFilters_resourceName :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_resourceName = Lens.lens (\ResourceFilters' {resourceName} -> resourceName) (\s@ResourceFilters' {} a -> s {resourceName = a} :: ResourceFilters)

instance Prelude.Hashable ResourceFilters

instance Prelude.NFData ResourceFilters

instance Prelude.ToJSON ResourceFilters where
  toJSON ResourceFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceId" Prelude..=) Prelude.<$> resourceId,
            ("AccountId" Prelude..=) Prelude.<$> accountId,
            ("Region" Prelude..=) Prelude.<$> region,
            ("ResourceName" Prelude..=)
              Prelude.<$> resourceName
          ]
      )

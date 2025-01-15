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
-- Module      : Amazonka.Config.Types.ResourceFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters the results by resource account ID, region, resource ID, and
-- resource name.
--
-- /See:/ 'newResourceFilters' smart constructor.
data ResourceFilters = ResourceFilters'
  { -- | The 12-digit source account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'resourceFilters_accountId' - The 12-digit source account ID.
--
-- 'region', 'resourceFilters_region' - The source region.
--
-- 'resourceId', 'resourceFilters_resourceId' - The ID of the resource.
--
-- 'resourceName', 'resourceFilters_resourceName' - The name of the resource.
newResourceFilters ::
  ResourceFilters
newResourceFilters =
  ResourceFilters'
    { accountId = Prelude.Nothing,
      region = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing
    }

-- | The 12-digit source account ID.
resourceFilters_accountId :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_accountId = Lens.lens (\ResourceFilters' {accountId} -> accountId) (\s@ResourceFilters' {} a -> s {accountId = a} :: ResourceFilters)

-- | The source region.
resourceFilters_region :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_region = Lens.lens (\ResourceFilters' {region} -> region) (\s@ResourceFilters' {} a -> s {region = a} :: ResourceFilters)

-- | The ID of the resource.
resourceFilters_resourceId :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_resourceId = Lens.lens (\ResourceFilters' {resourceId} -> resourceId) (\s@ResourceFilters' {} a -> s {resourceId = a} :: ResourceFilters)

-- | The name of the resource.
resourceFilters_resourceName :: Lens.Lens' ResourceFilters (Prelude.Maybe Prelude.Text)
resourceFilters_resourceName = Lens.lens (\ResourceFilters' {resourceName} -> resourceName) (\s@ResourceFilters' {} a -> s {resourceName = a} :: ResourceFilters)

instance Prelude.Hashable ResourceFilters where
  hashWithSalt _salt ResourceFilters' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData ResourceFilters where
  rnf ResourceFilters' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf region `Prelude.seq`
        Prelude.rnf resourceId `Prelude.seq`
          Prelude.rnf resourceName

instance Data.ToJSON ResourceFilters where
  toJSON ResourceFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("Region" Data..=) Prelude.<$> region,
            ("ResourceId" Data..=) Prelude.<$> resourceId,
            ("ResourceName" Data..=) Prelude.<$> resourceName
          ]
      )

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
-- Module      : Amazonka.Config.Types.ResourceCountFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceCountFilters where

import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters the resource count based on account ID, region, and resource
-- type.
--
-- /See:/ 'newResourceCountFilters' smart constructor.
data ResourceCountFilters = ResourceCountFilters'
  { -- | The 12-digit ID of the account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The region where the account is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The type of the Amazon Web Services resource.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCountFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'resourceCountFilters_accountId' - The 12-digit ID of the account.
--
-- 'region', 'resourceCountFilters_region' - The region where the account is located.
--
-- 'resourceType', 'resourceCountFilters_resourceType' - The type of the Amazon Web Services resource.
newResourceCountFilters ::
  ResourceCountFilters
newResourceCountFilters =
  ResourceCountFilters'
    { accountId = Prelude.Nothing,
      region = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The 12-digit ID of the account.
resourceCountFilters_accountId :: Lens.Lens' ResourceCountFilters (Prelude.Maybe Prelude.Text)
resourceCountFilters_accountId = Lens.lens (\ResourceCountFilters' {accountId} -> accountId) (\s@ResourceCountFilters' {} a -> s {accountId = a} :: ResourceCountFilters)

-- | The region where the account is located.
resourceCountFilters_region :: Lens.Lens' ResourceCountFilters (Prelude.Maybe Prelude.Text)
resourceCountFilters_region = Lens.lens (\ResourceCountFilters' {region} -> region) (\s@ResourceCountFilters' {} a -> s {region = a} :: ResourceCountFilters)

-- | The type of the Amazon Web Services resource.
resourceCountFilters_resourceType :: Lens.Lens' ResourceCountFilters (Prelude.Maybe ResourceType)
resourceCountFilters_resourceType = Lens.lens (\ResourceCountFilters' {resourceType} -> resourceType) (\s@ResourceCountFilters' {} a -> s {resourceType = a} :: ResourceCountFilters)

instance Prelude.Hashable ResourceCountFilters where
  hashWithSalt _salt ResourceCountFilters' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ResourceCountFilters where
  rnf ResourceCountFilters' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToJSON ResourceCountFilters where
  toJSON ResourceCountFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("Region" Data..=) Prelude.<$> region,
            ("ResourceType" Data..=) Prelude.<$> resourceType
          ]
      )

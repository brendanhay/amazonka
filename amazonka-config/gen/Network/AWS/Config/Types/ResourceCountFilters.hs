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
-- Module      : Network.AWS.Config.Types.ResourceCountFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCountFilters where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Filters the resource count based on account ID, region, and resource
-- type.
--
-- /See:/ 'newResourceCountFilters' smart constructor.
data ResourceCountFilters = ResourceCountFilters'
  { -- | The 12-digit ID of the account.
    accountId :: Core.Maybe Core.Text,
    -- | The type of the AWS resource.
    resourceType :: Core.Maybe ResourceType,
    -- | The region where the account is located.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'resourceType', 'resourceCountFilters_resourceType' - The type of the AWS resource.
--
-- 'region', 'resourceCountFilters_region' - The region where the account is located.
newResourceCountFilters ::
  ResourceCountFilters
newResourceCountFilters =
  ResourceCountFilters'
    { accountId = Core.Nothing,
      resourceType = Core.Nothing,
      region = Core.Nothing
    }

-- | The 12-digit ID of the account.
resourceCountFilters_accountId :: Lens.Lens' ResourceCountFilters (Core.Maybe Core.Text)
resourceCountFilters_accountId = Lens.lens (\ResourceCountFilters' {accountId} -> accountId) (\s@ResourceCountFilters' {} a -> s {accountId = a} :: ResourceCountFilters)

-- | The type of the AWS resource.
resourceCountFilters_resourceType :: Lens.Lens' ResourceCountFilters (Core.Maybe ResourceType)
resourceCountFilters_resourceType = Lens.lens (\ResourceCountFilters' {resourceType} -> resourceType) (\s@ResourceCountFilters' {} a -> s {resourceType = a} :: ResourceCountFilters)

-- | The region where the account is located.
resourceCountFilters_region :: Lens.Lens' ResourceCountFilters (Core.Maybe Core.Text)
resourceCountFilters_region = Lens.lens (\ResourceCountFilters' {region} -> region) (\s@ResourceCountFilters' {} a -> s {region = a} :: ResourceCountFilters)

instance Core.Hashable ResourceCountFilters

instance Core.NFData ResourceCountFilters

instance Core.ToJSON ResourceCountFilters where
  toJSON ResourceCountFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountId" Core..=) Core.<$> accountId,
            ("ResourceType" Core..=) Core.<$> resourceType,
            ("Region" Core..=) Core.<$> region
          ]
      )

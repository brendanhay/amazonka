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
-- Module      : Network.AWS.Config.Types.ResourceCountFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCountFilters where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters the resource count based on account ID, region, and resource
-- type.
--
-- /See:/ 'newResourceCountFilters' smart constructor.
data ResourceCountFilters = ResourceCountFilters'
  { -- | The 12-digit ID of the account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The type of the AWS resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The region where the account is located.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { accountId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The 12-digit ID of the account.
resourceCountFilters_accountId :: Lens.Lens' ResourceCountFilters (Prelude.Maybe Prelude.Text)
resourceCountFilters_accountId = Lens.lens (\ResourceCountFilters' {accountId} -> accountId) (\s@ResourceCountFilters' {} a -> s {accountId = a} :: ResourceCountFilters)

-- | The type of the AWS resource.
resourceCountFilters_resourceType :: Lens.Lens' ResourceCountFilters (Prelude.Maybe ResourceType)
resourceCountFilters_resourceType = Lens.lens (\ResourceCountFilters' {resourceType} -> resourceType) (\s@ResourceCountFilters' {} a -> s {resourceType = a} :: ResourceCountFilters)

-- | The region where the account is located.
resourceCountFilters_region :: Lens.Lens' ResourceCountFilters (Prelude.Maybe Prelude.Text)
resourceCountFilters_region = Lens.lens (\ResourceCountFilters' {region} -> region) (\s@ResourceCountFilters' {} a -> s {region = a} :: ResourceCountFilters)

instance Prelude.Hashable ResourceCountFilters

instance Prelude.NFData ResourceCountFilters

instance Prelude.ToJSON ResourceCountFilters where
  toJSON ResourceCountFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccountId" Prelude..=) Prelude.<$> accountId,
            ("ResourceType" Prelude..=) Prelude.<$> resourceType,
            ("Region" Prelude..=) Prelude.<$> region
          ]
      )

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
-- Module      : Amazonka.Shield.Types.InclusionProtectionGroupFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.InclusionProtectionGroupFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ProtectedResourceType
import Amazonka.Shield.Types.ProtectionGroupAggregation
import Amazonka.Shield.Types.ProtectionGroupPattern

-- | Narrows the set of protection groups that the call retrieves. You can
-- retrieve a single protection group by its name and you can retrieve all
-- protection groups that are configured with a specific pattern,
-- aggregation, or resource type. You can provide up to one criteria per
-- filter type. Shield Advanced returns the protection groups that exactly
-- match all of the search criteria that you provide.
--
-- /See:/ 'newInclusionProtectionGroupFilters' smart constructor.
data InclusionProtectionGroupFilters = InclusionProtectionGroupFilters'
  { -- | The aggregation setting of the protection groups that you want to
    -- retrieve.
    aggregations :: Prelude.Maybe (Prelude.NonEmpty ProtectionGroupAggregation),
    -- | The pattern specification of the protection groups that you want to
    -- retrieve.
    patterns :: Prelude.Maybe (Prelude.NonEmpty ProtectionGroupPattern),
    -- | The ID of the protection group that you want to retrieve.
    protectionGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The resource type configuration of the protection groups that you want
    -- to retrieve. In the protection group configuration, you specify the
    -- resource type when you set the group\'s @Pattern@ to @BY_RESOURCE_TYPE@.
    resourceTypes :: Prelude.Maybe (Prelude.NonEmpty ProtectedResourceType)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InclusionProtectionGroupFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregations', 'inclusionProtectionGroupFilters_aggregations' - The aggregation setting of the protection groups that you want to
-- retrieve.
--
-- 'patterns', 'inclusionProtectionGroupFilters_patterns' - The pattern specification of the protection groups that you want to
-- retrieve.
--
-- 'protectionGroupIds', 'inclusionProtectionGroupFilters_protectionGroupIds' - The ID of the protection group that you want to retrieve.
--
-- 'resourceTypes', 'inclusionProtectionGroupFilters_resourceTypes' - The resource type configuration of the protection groups that you want
-- to retrieve. In the protection group configuration, you specify the
-- resource type when you set the group\'s @Pattern@ to @BY_RESOURCE_TYPE@.
newInclusionProtectionGroupFilters ::
  InclusionProtectionGroupFilters
newInclusionProtectionGroupFilters =
  InclusionProtectionGroupFilters'
    { aggregations =
        Prelude.Nothing,
      patterns = Prelude.Nothing,
      protectionGroupIds = Prelude.Nothing,
      resourceTypes = Prelude.Nothing
    }

-- | The aggregation setting of the protection groups that you want to
-- retrieve.
inclusionProtectionGroupFilters_aggregations :: Lens.Lens' InclusionProtectionGroupFilters (Prelude.Maybe (Prelude.NonEmpty ProtectionGroupAggregation))
inclusionProtectionGroupFilters_aggregations = Lens.lens (\InclusionProtectionGroupFilters' {aggregations} -> aggregations) (\s@InclusionProtectionGroupFilters' {} a -> s {aggregations = a} :: InclusionProtectionGroupFilters) Prelude.. Lens.mapping Lens.coerced

-- | The pattern specification of the protection groups that you want to
-- retrieve.
inclusionProtectionGroupFilters_patterns :: Lens.Lens' InclusionProtectionGroupFilters (Prelude.Maybe (Prelude.NonEmpty ProtectionGroupPattern))
inclusionProtectionGroupFilters_patterns = Lens.lens (\InclusionProtectionGroupFilters' {patterns} -> patterns) (\s@InclusionProtectionGroupFilters' {} a -> s {patterns = a} :: InclusionProtectionGroupFilters) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the protection group that you want to retrieve.
inclusionProtectionGroupFilters_protectionGroupIds :: Lens.Lens' InclusionProtectionGroupFilters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
inclusionProtectionGroupFilters_protectionGroupIds = Lens.lens (\InclusionProtectionGroupFilters' {protectionGroupIds} -> protectionGroupIds) (\s@InclusionProtectionGroupFilters' {} a -> s {protectionGroupIds = a} :: InclusionProtectionGroupFilters) Prelude.. Lens.mapping Lens.coerced

-- | The resource type configuration of the protection groups that you want
-- to retrieve. In the protection group configuration, you specify the
-- resource type when you set the group\'s @Pattern@ to @BY_RESOURCE_TYPE@.
inclusionProtectionGroupFilters_resourceTypes :: Lens.Lens' InclusionProtectionGroupFilters (Prelude.Maybe (Prelude.NonEmpty ProtectedResourceType))
inclusionProtectionGroupFilters_resourceTypes = Lens.lens (\InclusionProtectionGroupFilters' {resourceTypes} -> resourceTypes) (\s@InclusionProtectionGroupFilters' {} a -> s {resourceTypes = a} :: InclusionProtectionGroupFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    InclusionProtectionGroupFilters
  where
  hashWithSalt
    _salt
    InclusionProtectionGroupFilters' {..} =
      _salt `Prelude.hashWithSalt` aggregations
        `Prelude.hashWithSalt` patterns
        `Prelude.hashWithSalt` protectionGroupIds
        `Prelude.hashWithSalt` resourceTypes

instance
  Prelude.NFData
    InclusionProtectionGroupFilters
  where
  rnf InclusionProtectionGroupFilters' {..} =
    Prelude.rnf aggregations
      `Prelude.seq` Prelude.rnf patterns
      `Prelude.seq` Prelude.rnf protectionGroupIds
      `Prelude.seq` Prelude.rnf resourceTypes

instance Data.ToJSON InclusionProtectionGroupFilters where
  toJSON InclusionProtectionGroupFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregations" Data..=) Prelude.<$> aggregations,
            ("Patterns" Data..=) Prelude.<$> patterns,
            ("ProtectionGroupIds" Data..=)
              Prelude.<$> protectionGroupIds,
            ("ResourceTypes" Data..=) Prelude.<$> resourceTypes
          ]
      )

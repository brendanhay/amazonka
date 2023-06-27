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
-- Module      : Amazonka.Connect.Types.FilterV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.FilterV2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the filter to apply when retrieving metrics with the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_GetMetricDataV2.html GetMetricDataV2>
-- API.
--
-- /See:/ 'newFilterV2' smart constructor.
data FilterV2 = FilterV2'
  { -- | The key to use for filtering data. For example, @QUEUE@,
    -- @ROUTING_PROFILE, AGENT@, @CHANNEL@, @AGENT_HIERARCHY_LEVEL_ONE@,
    -- @AGENT_HIERARCHY_LEVEL_TWO@, @AGENT_HIERARCHY_LEVEL_THREE@,
    -- @AGENT_HIERARCHY_LEVEL_FOUR@, @AGENT_HIERARCHY_LEVEL_FIVE@. There must
    -- be at least 1 key and a maximum 5 keys.
    filterKey :: Prelude.Maybe Prelude.Text,
    -- | The identifiers to use for filtering data. For example, if you have a
    -- filter key of @QUEUE@, you would add queue IDs or ARNs in
    -- @FilterValues@.
    filterValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterKey', 'filterV2_filterKey' - The key to use for filtering data. For example, @QUEUE@,
-- @ROUTING_PROFILE, AGENT@, @CHANNEL@, @AGENT_HIERARCHY_LEVEL_ONE@,
-- @AGENT_HIERARCHY_LEVEL_TWO@, @AGENT_HIERARCHY_LEVEL_THREE@,
-- @AGENT_HIERARCHY_LEVEL_FOUR@, @AGENT_HIERARCHY_LEVEL_FIVE@. There must
-- be at least 1 key and a maximum 5 keys.
--
-- 'filterValues', 'filterV2_filterValues' - The identifiers to use for filtering data. For example, if you have a
-- filter key of @QUEUE@, you would add queue IDs or ARNs in
-- @FilterValues@.
newFilterV2 ::
  FilterV2
newFilterV2 =
  FilterV2'
    { filterKey = Prelude.Nothing,
      filterValues = Prelude.Nothing
    }

-- | The key to use for filtering data. For example, @QUEUE@,
-- @ROUTING_PROFILE, AGENT@, @CHANNEL@, @AGENT_HIERARCHY_LEVEL_ONE@,
-- @AGENT_HIERARCHY_LEVEL_TWO@, @AGENT_HIERARCHY_LEVEL_THREE@,
-- @AGENT_HIERARCHY_LEVEL_FOUR@, @AGENT_HIERARCHY_LEVEL_FIVE@. There must
-- be at least 1 key and a maximum 5 keys.
filterV2_filterKey :: Lens.Lens' FilterV2 (Prelude.Maybe Prelude.Text)
filterV2_filterKey = Lens.lens (\FilterV2' {filterKey} -> filterKey) (\s@FilterV2' {} a -> s {filterKey = a} :: FilterV2)

-- | The identifiers to use for filtering data. For example, if you have a
-- filter key of @QUEUE@, you would add queue IDs or ARNs in
-- @FilterValues@.
filterV2_filterValues :: Lens.Lens' FilterV2 (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filterV2_filterValues = Lens.lens (\FilterV2' {filterValues} -> filterValues) (\s@FilterV2' {} a -> s {filterValues = a} :: FilterV2) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable FilterV2 where
  hashWithSalt _salt FilterV2' {..} =
    _salt
      `Prelude.hashWithSalt` filterKey
      `Prelude.hashWithSalt` filterValues

instance Prelude.NFData FilterV2 where
  rnf FilterV2' {..} =
    Prelude.rnf filterKey
      `Prelude.seq` Prelude.rnf filterValues

instance Data.ToJSON FilterV2 where
  toJSON FilterV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterKey" Data..=) Prelude.<$> filterKey,
            ("FilterValues" Data..=) Prelude.<$> filterValues
          ]
      )

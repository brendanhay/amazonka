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
-- Module      : Amazonka.MarketplaceCatalog.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter object, used to optionally filter results from calls to the
-- @ListEntities@ and @ListChangeSets@ actions.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | For @ListEntities@, the supported value for this is an @EntityId@.
    --
    -- For @ListChangeSets@, the supported values are as follows:
    name :: Prelude.Maybe Prelude.Text,
    -- | @ListEntities@ - This is a list of unique @EntityId@s.
    --
    -- @ListChangeSets@ - The supported filter names and associated
    -- @ValueList@s is as follows:
    --
    -- -   @ChangeSetName@ - The supported @ValueList@ is a list of non-unique
    --     @ChangeSetName@s. These are defined when you call the
    --     @StartChangeSet@ action.
    --
    -- -   @Status@ - The supported @ValueList@ is a list of statuses for all
    --     change set requests.
    --
    -- -   @EntityId@ - The supported @ValueList@ is a list of unique
    --     @EntityId@s.
    --
    -- -   @BeforeStartTime@ - The supported @ValueList@ is a list of all
    --     change sets that started before the filter value.
    --
    -- -   @AfterStartTime@ - The supported @ValueList@ is a list of all change
    --     sets that started after the filter value.
    --
    -- -   @BeforeEndTime@ - The supported @ValueList@ is a list of all change
    --     sets that ended before the filter value.
    --
    -- -   @AfterEndTime@ - The supported @ValueList@ is a list of all change
    --     sets that ended after the filter value.
    valueList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filter_name' - For @ListEntities@, the supported value for this is an @EntityId@.
--
-- For @ListChangeSets@, the supported values are as follows:
--
-- 'valueList', 'filter_valueList' - @ListEntities@ - This is a list of unique @EntityId@s.
--
-- @ListChangeSets@ - The supported filter names and associated
-- @ValueList@s is as follows:
--
-- -   @ChangeSetName@ - The supported @ValueList@ is a list of non-unique
--     @ChangeSetName@s. These are defined when you call the
--     @StartChangeSet@ action.
--
-- -   @Status@ - The supported @ValueList@ is a list of statuses for all
--     change set requests.
--
-- -   @EntityId@ - The supported @ValueList@ is a list of unique
--     @EntityId@s.
--
-- -   @BeforeStartTime@ - The supported @ValueList@ is a list of all
--     change sets that started before the filter value.
--
-- -   @AfterStartTime@ - The supported @ValueList@ is a list of all change
--     sets that started after the filter value.
--
-- -   @BeforeEndTime@ - The supported @ValueList@ is a list of all change
--     sets that ended before the filter value.
--
-- -   @AfterEndTime@ - The supported @ValueList@ is a list of all change
--     sets that ended after the filter value.
newFilter ::
  Filter
newFilter =
  Filter'
    { name = Prelude.Nothing,
      valueList = Prelude.Nothing
    }

-- | For @ListEntities@, the supported value for this is an @EntityId@.
--
-- For @ListChangeSets@, the supported values are as follows:
filter_name :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | @ListEntities@ - This is a list of unique @EntityId@s.
--
-- @ListChangeSets@ - The supported filter names and associated
-- @ValueList@s is as follows:
--
-- -   @ChangeSetName@ - The supported @ValueList@ is a list of non-unique
--     @ChangeSetName@s. These are defined when you call the
--     @StartChangeSet@ action.
--
-- -   @Status@ - The supported @ValueList@ is a list of statuses for all
--     change set requests.
--
-- -   @EntityId@ - The supported @ValueList@ is a list of unique
--     @EntityId@s.
--
-- -   @BeforeStartTime@ - The supported @ValueList@ is a list of all
--     change sets that started before the filter value.
--
-- -   @AfterStartTime@ - The supported @ValueList@ is a list of all change
--     sets that started after the filter value.
--
-- -   @BeforeEndTime@ - The supported @ValueList@ is a list of all change
--     sets that ended before the filter value.
--
-- -   @AfterEndTime@ - The supported @ValueList@ is a list of all change
--     sets that ended after the filter value.
filter_valueList :: Lens.Lens' Filter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filter_valueList = Lens.lens (\Filter' {valueList} -> valueList) (\s@Filter' {} a -> s {valueList = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` valueList

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf valueList

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("ValueList" Data..=) Prelude.<$> valueList
          ]
      )

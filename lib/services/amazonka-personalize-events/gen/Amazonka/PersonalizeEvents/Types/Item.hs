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
-- Module      : Amazonka.PersonalizeEvents.Types.Item
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeEvents.Types.Item where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents item metadata added to an Items dataset using the @PutItems@
-- API. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/importing-items.html Importing Items Incrementally>.
--
-- /See:/ 'newItem' smart constructor.
data Item = Item'
  { -- | A string map of item-specific metadata. Each element in the map consists
    -- of a key-value pair. For example, @{\"numberOfRatings\": \"12\"}@.
    --
    -- The keys use camel case names that match the fields in the schema for
    -- the Items dataset. In the previous example, the @numberOfRatings@
    -- matches the \'NUMBER_OF_RATINGS\' field defined in the Items schema. For
    -- categorical string data, to include multiple categories for a single
    -- item, separate each category with a pipe separator (@|@). For example,
    -- @\\\"Horror|Action\\\"@.
    properties :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID associated with the item.
    itemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Item' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'properties', 'item_properties' - A string map of item-specific metadata. Each element in the map consists
-- of a key-value pair. For example, @{\"numberOfRatings\": \"12\"}@.
--
-- The keys use camel case names that match the fields in the schema for
-- the Items dataset. In the previous example, the @numberOfRatings@
-- matches the \'NUMBER_OF_RATINGS\' field defined in the Items schema. For
-- categorical string data, to include multiple categories for a single
-- item, separate each category with a pipe separator (@|@). For example,
-- @\\\"Horror|Action\\\"@.
--
-- 'itemId', 'item_itemId' - The ID associated with the item.
newItem ::
  -- | 'itemId'
  Prelude.Text ->
  Item
newItem pItemId_ =
  Item'
    { properties = Prelude.Nothing,
      itemId = pItemId_
    }

-- | A string map of item-specific metadata. Each element in the map consists
-- of a key-value pair. For example, @{\"numberOfRatings\": \"12\"}@.
--
-- The keys use camel case names that match the fields in the schema for
-- the Items dataset. In the previous example, the @numberOfRatings@
-- matches the \'NUMBER_OF_RATINGS\' field defined in the Items schema. For
-- categorical string data, to include multiple categories for a single
-- item, separate each category with a pipe separator (@|@). For example,
-- @\\\"Horror|Action\\\"@.
item_properties :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_properties = Lens.lens (\Item' {properties} -> properties) (\s@Item' {} a -> s {properties = a} :: Item) Prelude.. Lens.mapping Data._Sensitive

-- | The ID associated with the item.
item_itemId :: Lens.Lens' Item Prelude.Text
item_itemId = Lens.lens (\Item' {itemId} -> itemId) (\s@Item' {} a -> s {itemId = a} :: Item)

instance Prelude.Hashable Item where
  hashWithSalt _salt Item' {..} =
    _salt `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` itemId

instance Prelude.NFData Item where
  rnf Item' {..} =
    Prelude.rnf properties
      `Prelude.seq` Prelude.rnf itemId

instance Data.ToJSON Item where
  toJSON Item' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("properties" Data..=) Prelude.<$> properties,
            Prelude.Just ("itemId" Data..= itemId)
          ]
      )

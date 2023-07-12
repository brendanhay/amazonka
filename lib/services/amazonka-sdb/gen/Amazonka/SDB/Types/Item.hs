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
-- Module      : Amazonka.SDB.Types.Item
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SDB.Types.Item where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SDB.Types.Attribute

-- |
--
-- /See:/ 'newItem' smart constructor.
data Item = Item'
  { alternateNameEncoding :: Prelude.Maybe Prelude.Text,
    -- | The name of the item.
    name :: Prelude.Text,
    -- | A list of attributes.
    attributes :: [Attribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Item' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alternateNameEncoding', 'item_alternateNameEncoding' -
--
-- 'name', 'item_name' - The name of the item.
--
-- 'attributes', 'item_attributes' - A list of attributes.
newItem ::
  -- | 'name'
  Prelude.Text ->
  Item
newItem pName_ =
  Item'
    { alternateNameEncoding = Prelude.Nothing,
      name = pName_,
      attributes = Prelude.mempty
    }

item_alternateNameEncoding :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_alternateNameEncoding = Lens.lens (\Item' {alternateNameEncoding} -> alternateNameEncoding) (\s@Item' {} a -> s {alternateNameEncoding = a} :: Item)

-- | The name of the item.
item_name :: Lens.Lens' Item Prelude.Text
item_name = Lens.lens (\Item' {name} -> name) (\s@Item' {} a -> s {name = a} :: Item)

-- | A list of attributes.
item_attributes :: Lens.Lens' Item [Attribute]
item_attributes = Lens.lens (\Item' {attributes} -> attributes) (\s@Item' {} a -> s {attributes = a} :: Item) Prelude.. Lens.coerced

instance Data.FromXML Item where
  parseXML x =
    Item'
      Prelude.<$> (x Data..@? "AlternateNameEncoding")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (Data.parseXMLList "Attribute" x)

instance Prelude.Hashable Item where
  hashWithSalt _salt Item' {..} =
    _salt
      `Prelude.hashWithSalt` alternateNameEncoding
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData Item where
  rnf Item' {..} =
    Prelude.rnf alternateNameEncoding
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf attributes

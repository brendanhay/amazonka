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
-- Module      : Network.AWS.SDB.Types.Item
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Item where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SDB.Types.Attribute

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- |
item_alternateNameEncoding :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_alternateNameEncoding = Lens.lens (\Item' {alternateNameEncoding} -> alternateNameEncoding) (\s@Item' {} a -> s {alternateNameEncoding = a} :: Item)

-- | The name of the item.
item_name :: Lens.Lens' Item Prelude.Text
item_name = Lens.lens (\Item' {name} -> name) (\s@Item' {} a -> s {name = a} :: Item)

-- | A list of attributes.
item_attributes :: Lens.Lens' Item [Attribute]
item_attributes = Lens.lens (\Item' {attributes} -> attributes) (\s@Item' {} a -> s {attributes = a} :: Item) Prelude.. Prelude._Coerce

instance Prelude.FromXML Item where
  parseXML x =
    Item'
      Prelude.<$> (x Prelude..@? "AlternateNameEncoding")
      Prelude.<*> (x Prelude..@ "Name")
      Prelude.<*> (Prelude.parseXMLList "Attribute" x)

instance Prelude.Hashable Item

instance Prelude.NFData Item

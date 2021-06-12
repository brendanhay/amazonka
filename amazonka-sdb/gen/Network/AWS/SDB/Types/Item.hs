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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SDB.Types.Attribute

-- |
--
-- /See:/ 'newItem' smart constructor.
data Item = Item'
  { alternateNameEncoding :: Core.Maybe Core.Text,
    -- | The name of the item.
    name :: Core.Text,
    -- | A list of attributes.
    attributes :: [Attribute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  Item
newItem pName_ =
  Item'
    { alternateNameEncoding = Core.Nothing,
      name = pName_,
      attributes = Core.mempty
    }

-- |
item_alternateNameEncoding :: Lens.Lens' Item (Core.Maybe Core.Text)
item_alternateNameEncoding = Lens.lens (\Item' {alternateNameEncoding} -> alternateNameEncoding) (\s@Item' {} a -> s {alternateNameEncoding = a} :: Item)

-- | The name of the item.
item_name :: Lens.Lens' Item Core.Text
item_name = Lens.lens (\Item' {name} -> name) (\s@Item' {} a -> s {name = a} :: Item)

-- | A list of attributes.
item_attributes :: Lens.Lens' Item [Attribute]
item_attributes = Lens.lens (\Item' {attributes} -> attributes) (\s@Item' {} a -> s {attributes = a} :: Item) Core.. Lens._Coerce

instance Core.FromXML Item where
  parseXML x =
    Item'
      Core.<$> (x Core..@? "AlternateNameEncoding")
      Core.<*> (x Core..@ "Name")
      Core.<*> (Core.parseXMLList "Attribute" x)

instance Core.Hashable Item

instance Core.NFData Item

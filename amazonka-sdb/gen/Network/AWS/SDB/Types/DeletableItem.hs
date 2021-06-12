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
-- Module      : Network.AWS.SDB.Types.DeletableItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.DeletableItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SDB.Types.Attribute

-- | /See:/ 'newDeletableItem' smart constructor.
data DeletableItem = DeletableItem'
  { attributes :: Core.Maybe [Attribute],
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletableItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'deletableItem_attributes' - Undocumented member.
--
-- 'name', 'deletableItem_name' - Undocumented member.
newDeletableItem ::
  -- | 'name'
  Core.Text ->
  DeletableItem
newDeletableItem pName_ =
  DeletableItem'
    { attributes = Core.Nothing,
      name = pName_
    }

-- | Undocumented member.
deletableItem_attributes :: Lens.Lens' DeletableItem (Core.Maybe [Attribute])
deletableItem_attributes = Lens.lens (\DeletableItem' {attributes} -> attributes) (\s@DeletableItem' {} a -> s {attributes = a} :: DeletableItem) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
deletableItem_name :: Lens.Lens' DeletableItem Core.Text
deletableItem_name = Lens.lens (\DeletableItem' {name} -> name) (\s@DeletableItem' {} a -> s {name = a} :: DeletableItem)

instance Core.Hashable DeletableItem

instance Core.NFData DeletableItem

instance Core.ToQuery DeletableItem where
  toQuery DeletableItem' {..} =
    Core.mconcat
      [ Core.toQuery
          (Core.toQueryList "Attribute" Core.<$> attributes),
        "ItemName" Core.=: name
      ]

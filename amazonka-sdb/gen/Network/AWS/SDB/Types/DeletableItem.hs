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
-- Module      : Network.AWS.SDB.Types.DeletableItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.DeletableItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SDB.Types.Attribute

-- | /See:/ 'newDeletableItem' smart constructor.
data DeletableItem = DeletableItem'
  { attributes :: Prelude.Maybe [Attribute],
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeletableItem
newDeletableItem pName_ =
  DeletableItem'
    { attributes = Prelude.Nothing,
      name = pName_
    }

-- | Undocumented member.
deletableItem_attributes :: Lens.Lens' DeletableItem (Prelude.Maybe [Attribute])
deletableItem_attributes = Lens.lens (\DeletableItem' {attributes} -> attributes) (\s@DeletableItem' {} a -> s {attributes = a} :: DeletableItem) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
deletableItem_name :: Lens.Lens' DeletableItem Prelude.Text
deletableItem_name = Lens.lens (\DeletableItem' {name} -> name) (\s@DeletableItem' {} a -> s {name = a} :: DeletableItem)

instance Prelude.Hashable DeletableItem

instance Prelude.NFData DeletableItem

instance Prelude.ToQuery DeletableItem where
  toQuery DeletableItem' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          ( Prelude.toQueryList "Attribute"
              Prelude.<$> attributes
          ),
        "ItemName" Prelude.=: name
      ]

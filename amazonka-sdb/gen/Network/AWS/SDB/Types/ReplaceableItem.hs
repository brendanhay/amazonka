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
-- Module      : Network.AWS.SDB.Types.ReplaceableItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SDB.Types.ReplaceableAttribute

-- |
--
-- /See:/ 'newReplaceableItem' smart constructor.
data ReplaceableItem = ReplaceableItem'
  { -- | The name of the replaceable item.
    name :: Prelude.Text,
    -- | The list of attributes for a replaceable item.
    attributes :: [ReplaceableAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplaceableItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'replaceableItem_name' - The name of the replaceable item.
--
-- 'attributes', 'replaceableItem_attributes' - The list of attributes for a replaceable item.
newReplaceableItem ::
  -- | 'name'
  Prelude.Text ->
  ReplaceableItem
newReplaceableItem pName_ =
  ReplaceableItem'
    { name = pName_,
      attributes = Prelude.mempty
    }

-- | The name of the replaceable item.
replaceableItem_name :: Lens.Lens' ReplaceableItem Prelude.Text
replaceableItem_name = Lens.lens (\ReplaceableItem' {name} -> name) (\s@ReplaceableItem' {} a -> s {name = a} :: ReplaceableItem)

-- | The list of attributes for a replaceable item.
replaceableItem_attributes :: Lens.Lens' ReplaceableItem [ReplaceableAttribute]
replaceableItem_attributes = Lens.lens (\ReplaceableItem' {attributes} -> attributes) (\s@ReplaceableItem' {} a -> s {attributes = a} :: ReplaceableItem) Prelude.. Prelude._Coerce

instance Prelude.Hashable ReplaceableItem

instance Prelude.NFData ReplaceableItem

instance Prelude.ToQuery ReplaceableItem where
  toQuery ReplaceableItem' {..} =
    Prelude.mconcat
      [ "ItemName" Prelude.=: name,
        Prelude.toQueryList "Attribute" attributes
      ]

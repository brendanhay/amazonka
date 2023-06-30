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
-- Module      : Amazonka.SDB.Types.ReplaceableItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SDB.Types.ReplaceableItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SDB.Types.ReplaceableAttribute

-- |
--
-- /See:/ 'newReplaceableItem' smart constructor.
data ReplaceableItem = ReplaceableItem'
  { -- | The name of the replaceable item.
    name :: Prelude.Text,
    -- | The list of attributes for a replaceable item.
    attributes :: [ReplaceableAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
replaceableItem_attributes = Lens.lens (\ReplaceableItem' {attributes} -> attributes) (\s@ReplaceableItem' {} a -> s {attributes = a} :: ReplaceableItem) Prelude.. Lens.coerced

instance Prelude.Hashable ReplaceableItem where
  hashWithSalt _salt ReplaceableItem' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData ReplaceableItem where
  rnf ReplaceableItem' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToQuery ReplaceableItem where
  toQuery ReplaceableItem' {..} =
    Prelude.mconcat
      [ "ItemName" Data.=: name,
        Data.toQueryList "Attribute" attributes
      ]

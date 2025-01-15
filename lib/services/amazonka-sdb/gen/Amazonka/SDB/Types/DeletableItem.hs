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
-- Module      : Amazonka.SDB.Types.DeletableItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SDB.Types.DeletableItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SDB.Types.Attribute

-- | /See:/ 'newDeletableItem' smart constructor.
data DeletableItem = DeletableItem'
  { attributes :: Prelude.Maybe [Attribute],
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
deletableItem_attributes = Lens.lens (\DeletableItem' {attributes} -> attributes) (\s@DeletableItem' {} a -> s {attributes = a} :: DeletableItem) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
deletableItem_name :: Lens.Lens' DeletableItem Prelude.Text
deletableItem_name = Lens.lens (\DeletableItem' {name} -> name) (\s@DeletableItem' {} a -> s {name = a} :: DeletableItem)

instance Prelude.Hashable DeletableItem where
  hashWithSalt _salt DeletableItem' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeletableItem where
  rnf DeletableItem' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf name

instance Data.ToQuery DeletableItem where
  toQuery DeletableItem' {..} =
    Prelude.mconcat
      [ Data.toQuery
          ( Data.toQueryList "Attribute"
              Prelude.<$> attributes
          ),
        "ItemName" Data.=: name
      ]

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
-- Module      : Amazonka.Comprehend.Types.RelationshipsListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.RelationshipsListItem where

import Amazonka.Comprehend.Types.RelationshipType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of child blocks for the current block.
--
-- /See:/ 'newRelationshipsListItem' smart constructor.
data RelationshipsListItem = RelationshipsListItem'
  { -- | Identifers of the child blocks.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | Only supported relationship is a child relationship.
    type' :: Prelude.Maybe RelationshipType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationshipsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'relationshipsListItem_ids' - Identifers of the child blocks.
--
-- 'type'', 'relationshipsListItem_type' - Only supported relationship is a child relationship.
newRelationshipsListItem ::
  RelationshipsListItem
newRelationshipsListItem =
  RelationshipsListItem'
    { ids = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Identifers of the child blocks.
relationshipsListItem_ids :: Lens.Lens' RelationshipsListItem (Prelude.Maybe [Prelude.Text])
relationshipsListItem_ids = Lens.lens (\RelationshipsListItem' {ids} -> ids) (\s@RelationshipsListItem' {} a -> s {ids = a} :: RelationshipsListItem) Prelude.. Lens.mapping Lens.coerced

-- | Only supported relationship is a child relationship.
relationshipsListItem_type :: Lens.Lens' RelationshipsListItem (Prelude.Maybe RelationshipType)
relationshipsListItem_type = Lens.lens (\RelationshipsListItem' {type'} -> type') (\s@RelationshipsListItem' {} a -> s {type' = a} :: RelationshipsListItem)

instance Data.FromJSON RelationshipsListItem where
  parseJSON =
    Data.withObject
      "RelationshipsListItem"
      ( \x ->
          RelationshipsListItem'
            Prelude.<$> (x Data..:? "Ids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable RelationshipsListItem where
  hashWithSalt _salt RelationshipsListItem' {..} =
    _salt `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RelationshipsListItem where
  rnf RelationshipsListItem' {..} =
    Prelude.rnf ids `Prelude.seq` Prelude.rnf type'

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
-- Module      : Amazonka.Textract.Types.Relationship
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Relationship where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.RelationshipType

-- | Information about how blocks are related to each other. A @Block@ object
-- contains 0 or more @Relation@ objects in a list, @Relationships@. For
-- more information, see Block.
--
-- The @Type@ element provides the type of the relationship for all blocks
-- in the @IDs@ array.
--
-- /See:/ 'newRelationship' smart constructor.
data Relationship = Relationship'
  { -- | An array of IDs for related blocks. You can get the type of the
    -- relationship from the @Type@ element.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | The type of relationship between the blocks in the IDs array and the
    -- current block. The following list describes the relationship types that
    -- can be returned.
    --
    -- -   /VALUE/ - A list that contains the ID of the VALUE block that\'s
    --     associated with the KEY of a key-value pair.
    --
    -- -   /CHILD/ - A list of IDs that identify blocks found within the
    --     current block object. For example, WORD blocks have a CHILD
    --     relationship to the LINE block type.
    --
    -- -   /MERGED_CELL/ - A list of IDs that identify each of the MERGED_CELL
    --     block types in a table.
    --
    -- -   /ANSWER/ - A list that contains the ID of the QUERY_RESULT block
    --     that’s associated with the corresponding QUERY block.
    --
    -- -   /TABLE/ - A list of IDs that identify associated TABLE block types.
    --
    -- -   /TABLE_TITLE/ - A list that contains the ID for the TABLE_TITLE
    --     block type in a table.
    --
    -- -   /TABLE_FOOTER/ - A list of IDs that identify the TABLE_FOOTER block
    --     types in a table.
    type' :: Prelude.Maybe RelationshipType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Relationship' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'relationship_ids' - An array of IDs for related blocks. You can get the type of the
-- relationship from the @Type@ element.
--
-- 'type'', 'relationship_type' - The type of relationship between the blocks in the IDs array and the
-- current block. The following list describes the relationship types that
-- can be returned.
--
-- -   /VALUE/ - A list that contains the ID of the VALUE block that\'s
--     associated with the KEY of a key-value pair.
--
-- -   /CHILD/ - A list of IDs that identify blocks found within the
--     current block object. For example, WORD blocks have a CHILD
--     relationship to the LINE block type.
--
-- -   /MERGED_CELL/ - A list of IDs that identify each of the MERGED_CELL
--     block types in a table.
--
-- -   /ANSWER/ - A list that contains the ID of the QUERY_RESULT block
--     that’s associated with the corresponding QUERY block.
--
-- -   /TABLE/ - A list of IDs that identify associated TABLE block types.
--
-- -   /TABLE_TITLE/ - A list that contains the ID for the TABLE_TITLE
--     block type in a table.
--
-- -   /TABLE_FOOTER/ - A list of IDs that identify the TABLE_FOOTER block
--     types in a table.
newRelationship ::
  Relationship
newRelationship =
  Relationship'
    { ids = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An array of IDs for related blocks. You can get the type of the
-- relationship from the @Type@ element.
relationship_ids :: Lens.Lens' Relationship (Prelude.Maybe [Prelude.Text])
relationship_ids = Lens.lens (\Relationship' {ids} -> ids) (\s@Relationship' {} a -> s {ids = a} :: Relationship) Prelude.. Lens.mapping Lens.coerced

-- | The type of relationship between the blocks in the IDs array and the
-- current block. The following list describes the relationship types that
-- can be returned.
--
-- -   /VALUE/ - A list that contains the ID of the VALUE block that\'s
--     associated with the KEY of a key-value pair.
--
-- -   /CHILD/ - A list of IDs that identify blocks found within the
--     current block object. For example, WORD blocks have a CHILD
--     relationship to the LINE block type.
--
-- -   /MERGED_CELL/ - A list of IDs that identify each of the MERGED_CELL
--     block types in a table.
--
-- -   /ANSWER/ - A list that contains the ID of the QUERY_RESULT block
--     that’s associated with the corresponding QUERY block.
--
-- -   /TABLE/ - A list of IDs that identify associated TABLE block types.
--
-- -   /TABLE_TITLE/ - A list that contains the ID for the TABLE_TITLE
--     block type in a table.
--
-- -   /TABLE_FOOTER/ - A list of IDs that identify the TABLE_FOOTER block
--     types in a table.
relationship_type :: Lens.Lens' Relationship (Prelude.Maybe RelationshipType)
relationship_type = Lens.lens (\Relationship' {type'} -> type') (\s@Relationship' {} a -> s {type' = a} :: Relationship)

instance Data.FromJSON Relationship where
  parseJSON =
    Data.withObject
      "Relationship"
      ( \x ->
          Relationship'
            Prelude.<$> (x Data..:? "Ids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Relationship where
  hashWithSalt _salt Relationship' {..} =
    _salt
      `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Relationship where
  rnf Relationship' {..} =
    Prelude.rnf ids `Prelude.seq` Prelude.rnf type'

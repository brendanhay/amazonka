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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Relationship where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The type of relationship that the blocks in the IDs array have with the
    -- current block. The relationship can be @VALUE@ or @CHILD@. A
    -- relationship of type VALUE is a list that contains the ID of the VALUE
    -- block that\'s associated with the KEY of a key-value pair. A
    -- relationship of type CHILD is a list of IDs that identify WORD blocks in
    -- the case of lines Cell blocks in the case of Tables, and WORD blocks in
    -- the case of Selection Elements.
    type' :: Prelude.Maybe RelationshipType,
    -- | An array of IDs for related blocks. You can get the type of the
    -- relationship from the @Type@ element.
    ids :: Prelude.Maybe [Prelude.Text]
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
-- 'type'', 'relationship_type' - The type of relationship that the blocks in the IDs array have with the
-- current block. The relationship can be @VALUE@ or @CHILD@. A
-- relationship of type VALUE is a list that contains the ID of the VALUE
-- block that\'s associated with the KEY of a key-value pair. A
-- relationship of type CHILD is a list of IDs that identify WORD blocks in
-- the case of lines Cell blocks in the case of Tables, and WORD blocks in
-- the case of Selection Elements.
--
-- 'ids', 'relationship_ids' - An array of IDs for related blocks. You can get the type of the
-- relationship from the @Type@ element.
newRelationship ::
  Relationship
newRelationship =
  Relationship'
    { type' = Prelude.Nothing,
      ids = Prelude.Nothing
    }

-- | The type of relationship that the blocks in the IDs array have with the
-- current block. The relationship can be @VALUE@ or @CHILD@. A
-- relationship of type VALUE is a list that contains the ID of the VALUE
-- block that\'s associated with the KEY of a key-value pair. A
-- relationship of type CHILD is a list of IDs that identify WORD blocks in
-- the case of lines Cell blocks in the case of Tables, and WORD blocks in
-- the case of Selection Elements.
relationship_type :: Lens.Lens' Relationship (Prelude.Maybe RelationshipType)
relationship_type = Lens.lens (\Relationship' {type'} -> type') (\s@Relationship' {} a -> s {type' = a} :: Relationship)

-- | An array of IDs for related blocks. You can get the type of the
-- relationship from the @Type@ element.
relationship_ids :: Lens.Lens' Relationship (Prelude.Maybe [Prelude.Text])
relationship_ids = Lens.lens (\Relationship' {ids} -> ids) (\s@Relationship' {} a -> s {ids = a} :: Relationship) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Relationship where
  parseJSON =
    Core.withObject
      "Relationship"
      ( \x ->
          Relationship'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Ids" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Relationship where
  hashWithSalt _salt Relationship' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` ids

instance Prelude.NFData Relationship where
  rnf Relationship' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf ids

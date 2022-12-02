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
-- Module      : Amazonka.QuickSight.Types.TransformOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TransformOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CastColumnTypeOperation
import Amazonka.QuickSight.Types.CreateColumnsOperation
import Amazonka.QuickSight.Types.FilterOperation
import Amazonka.QuickSight.Types.ProjectOperation
import Amazonka.QuickSight.Types.RenameColumnOperation
import Amazonka.QuickSight.Types.TagColumnOperation
import Amazonka.QuickSight.Types.UntagColumnOperation

-- | A data transformation on a logical table. This is a variant type
-- structure. For this structure to be valid, only one of the attributes
-- can be non-null.
--
-- /See:/ 'newTransformOperation' smart constructor.
data TransformOperation = TransformOperation'
  { untagColumnOperation :: Prelude.Maybe UntagColumnOperation,
    -- | An operation that creates calculated columns. Columns created in one
    -- such operation form a lexical closure.
    createColumnsOperation :: Prelude.Maybe CreateColumnsOperation,
    -- | An operation that renames a column.
    renameColumnOperation :: Prelude.Maybe RenameColumnOperation,
    -- | An operation that projects columns. Operations that come after a
    -- projection can only refer to projected columns.
    projectOperation :: Prelude.Maybe ProjectOperation,
    -- | An operation that tags a column with additional information.
    tagColumnOperation :: Prelude.Maybe TagColumnOperation,
    -- | An operation that filters rows based on some condition.
    filterOperation :: Prelude.Maybe FilterOperation,
    -- | A transform operation that casts a column to a different type.
    castColumnTypeOperation :: Prelude.Maybe CastColumnTypeOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'untagColumnOperation', 'transformOperation_untagColumnOperation' - Undocumented member.
--
-- 'createColumnsOperation', 'transformOperation_createColumnsOperation' - An operation that creates calculated columns. Columns created in one
-- such operation form a lexical closure.
--
-- 'renameColumnOperation', 'transformOperation_renameColumnOperation' - An operation that renames a column.
--
-- 'projectOperation', 'transformOperation_projectOperation' - An operation that projects columns. Operations that come after a
-- projection can only refer to projected columns.
--
-- 'tagColumnOperation', 'transformOperation_tagColumnOperation' - An operation that tags a column with additional information.
--
-- 'filterOperation', 'transformOperation_filterOperation' - An operation that filters rows based on some condition.
--
-- 'castColumnTypeOperation', 'transformOperation_castColumnTypeOperation' - A transform operation that casts a column to a different type.
newTransformOperation ::
  TransformOperation
newTransformOperation =
  TransformOperation'
    { untagColumnOperation =
        Prelude.Nothing,
      createColumnsOperation = Prelude.Nothing,
      renameColumnOperation = Prelude.Nothing,
      projectOperation = Prelude.Nothing,
      tagColumnOperation = Prelude.Nothing,
      filterOperation = Prelude.Nothing,
      castColumnTypeOperation = Prelude.Nothing
    }

-- | Undocumented member.
transformOperation_untagColumnOperation :: Lens.Lens' TransformOperation (Prelude.Maybe UntagColumnOperation)
transformOperation_untagColumnOperation = Lens.lens (\TransformOperation' {untagColumnOperation} -> untagColumnOperation) (\s@TransformOperation' {} a -> s {untagColumnOperation = a} :: TransformOperation)

-- | An operation that creates calculated columns. Columns created in one
-- such operation form a lexical closure.
transformOperation_createColumnsOperation :: Lens.Lens' TransformOperation (Prelude.Maybe CreateColumnsOperation)
transformOperation_createColumnsOperation = Lens.lens (\TransformOperation' {createColumnsOperation} -> createColumnsOperation) (\s@TransformOperation' {} a -> s {createColumnsOperation = a} :: TransformOperation)

-- | An operation that renames a column.
transformOperation_renameColumnOperation :: Lens.Lens' TransformOperation (Prelude.Maybe RenameColumnOperation)
transformOperation_renameColumnOperation = Lens.lens (\TransformOperation' {renameColumnOperation} -> renameColumnOperation) (\s@TransformOperation' {} a -> s {renameColumnOperation = a} :: TransformOperation)

-- | An operation that projects columns. Operations that come after a
-- projection can only refer to projected columns.
transformOperation_projectOperation :: Lens.Lens' TransformOperation (Prelude.Maybe ProjectOperation)
transformOperation_projectOperation = Lens.lens (\TransformOperation' {projectOperation} -> projectOperation) (\s@TransformOperation' {} a -> s {projectOperation = a} :: TransformOperation)

-- | An operation that tags a column with additional information.
transformOperation_tagColumnOperation :: Lens.Lens' TransformOperation (Prelude.Maybe TagColumnOperation)
transformOperation_tagColumnOperation = Lens.lens (\TransformOperation' {tagColumnOperation} -> tagColumnOperation) (\s@TransformOperation' {} a -> s {tagColumnOperation = a} :: TransformOperation)

-- | An operation that filters rows based on some condition.
transformOperation_filterOperation :: Lens.Lens' TransformOperation (Prelude.Maybe FilterOperation)
transformOperation_filterOperation = Lens.lens (\TransformOperation' {filterOperation} -> filterOperation) (\s@TransformOperation' {} a -> s {filterOperation = a} :: TransformOperation)

-- | A transform operation that casts a column to a different type.
transformOperation_castColumnTypeOperation :: Lens.Lens' TransformOperation (Prelude.Maybe CastColumnTypeOperation)
transformOperation_castColumnTypeOperation = Lens.lens (\TransformOperation' {castColumnTypeOperation} -> castColumnTypeOperation) (\s@TransformOperation' {} a -> s {castColumnTypeOperation = a} :: TransformOperation)

instance Data.FromJSON TransformOperation where
  parseJSON =
    Data.withObject
      "TransformOperation"
      ( \x ->
          TransformOperation'
            Prelude.<$> (x Data..:? "UntagColumnOperation")
            Prelude.<*> (x Data..:? "CreateColumnsOperation")
            Prelude.<*> (x Data..:? "RenameColumnOperation")
            Prelude.<*> (x Data..:? "ProjectOperation")
            Prelude.<*> (x Data..:? "TagColumnOperation")
            Prelude.<*> (x Data..:? "FilterOperation")
            Prelude.<*> (x Data..:? "CastColumnTypeOperation")
      )

instance Prelude.Hashable TransformOperation where
  hashWithSalt _salt TransformOperation' {..} =
    _salt `Prelude.hashWithSalt` untagColumnOperation
      `Prelude.hashWithSalt` createColumnsOperation
      `Prelude.hashWithSalt` renameColumnOperation
      `Prelude.hashWithSalt` projectOperation
      `Prelude.hashWithSalt` tagColumnOperation
      `Prelude.hashWithSalt` filterOperation
      `Prelude.hashWithSalt` castColumnTypeOperation

instance Prelude.NFData TransformOperation where
  rnf TransformOperation' {..} =
    Prelude.rnf untagColumnOperation
      `Prelude.seq` Prelude.rnf createColumnsOperation
      `Prelude.seq` Prelude.rnf renameColumnOperation
      `Prelude.seq` Prelude.rnf projectOperation
      `Prelude.seq` Prelude.rnf tagColumnOperation
      `Prelude.seq` Prelude.rnf filterOperation
      `Prelude.seq` Prelude.rnf castColumnTypeOperation

instance Data.ToJSON TransformOperation where
  toJSON TransformOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UntagColumnOperation" Data..=)
              Prelude.<$> untagColumnOperation,
            ("CreateColumnsOperation" Data..=)
              Prelude.<$> createColumnsOperation,
            ("RenameColumnOperation" Data..=)
              Prelude.<$> renameColumnOperation,
            ("ProjectOperation" Data..=)
              Prelude.<$> projectOperation,
            ("TagColumnOperation" Data..=)
              Prelude.<$> tagColumnOperation,
            ("FilterOperation" Data..=)
              Prelude.<$> filterOperation,
            ("CastColumnTypeOperation" Data..=)
              Prelude.<$> castColumnTypeOperation
          ]
      )

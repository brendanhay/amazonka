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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | A transform operation that casts a column to a different type.
    castColumnTypeOperation :: Prelude.Maybe CastColumnTypeOperation,
    -- | An operation that creates calculated columns. Columns created in one
    -- such operation form a lexical closure.
    createColumnsOperation :: Prelude.Maybe CreateColumnsOperation,
    -- | An operation that filters rows based on some condition.
    filterOperation :: Prelude.Maybe FilterOperation,
    -- | An operation that projects columns. Operations that come after a
    -- projection can only refer to projected columns.
    projectOperation :: Prelude.Maybe ProjectOperation,
    -- | An operation that renames a column.
    renameColumnOperation :: Prelude.Maybe RenameColumnOperation,
    -- | An operation that tags a column with additional information.
    tagColumnOperation :: Prelude.Maybe TagColumnOperation,
    untagColumnOperation :: Prelude.Maybe UntagColumnOperation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'castColumnTypeOperation', 'transformOperation_castColumnTypeOperation' - A transform operation that casts a column to a different type.
--
-- 'createColumnsOperation', 'transformOperation_createColumnsOperation' - An operation that creates calculated columns. Columns created in one
-- such operation form a lexical closure.
--
-- 'filterOperation', 'transformOperation_filterOperation' - An operation that filters rows based on some condition.
--
-- 'projectOperation', 'transformOperation_projectOperation' - An operation that projects columns. Operations that come after a
-- projection can only refer to projected columns.
--
-- 'renameColumnOperation', 'transformOperation_renameColumnOperation' - An operation that renames a column.
--
-- 'tagColumnOperation', 'transformOperation_tagColumnOperation' - An operation that tags a column with additional information.
--
-- 'untagColumnOperation', 'transformOperation_untagColumnOperation' - Undocumented member.
newTransformOperation ::
  TransformOperation
newTransformOperation =
  TransformOperation'
    { castColumnTypeOperation =
        Prelude.Nothing,
      createColumnsOperation = Prelude.Nothing,
      filterOperation = Prelude.Nothing,
      projectOperation = Prelude.Nothing,
      renameColumnOperation = Prelude.Nothing,
      tagColumnOperation = Prelude.Nothing,
      untagColumnOperation = Prelude.Nothing
    }

-- | A transform operation that casts a column to a different type.
transformOperation_castColumnTypeOperation :: Lens.Lens' TransformOperation (Prelude.Maybe CastColumnTypeOperation)
transformOperation_castColumnTypeOperation = Lens.lens (\TransformOperation' {castColumnTypeOperation} -> castColumnTypeOperation) (\s@TransformOperation' {} a -> s {castColumnTypeOperation = a} :: TransformOperation)

-- | An operation that creates calculated columns. Columns created in one
-- such operation form a lexical closure.
transformOperation_createColumnsOperation :: Lens.Lens' TransformOperation (Prelude.Maybe CreateColumnsOperation)
transformOperation_createColumnsOperation = Lens.lens (\TransformOperation' {createColumnsOperation} -> createColumnsOperation) (\s@TransformOperation' {} a -> s {createColumnsOperation = a} :: TransformOperation)

-- | An operation that filters rows based on some condition.
transformOperation_filterOperation :: Lens.Lens' TransformOperation (Prelude.Maybe FilterOperation)
transformOperation_filterOperation = Lens.lens (\TransformOperation' {filterOperation} -> filterOperation) (\s@TransformOperation' {} a -> s {filterOperation = a} :: TransformOperation)

-- | An operation that projects columns. Operations that come after a
-- projection can only refer to projected columns.
transformOperation_projectOperation :: Lens.Lens' TransformOperation (Prelude.Maybe ProjectOperation)
transformOperation_projectOperation = Lens.lens (\TransformOperation' {projectOperation} -> projectOperation) (\s@TransformOperation' {} a -> s {projectOperation = a} :: TransformOperation)

-- | An operation that renames a column.
transformOperation_renameColumnOperation :: Lens.Lens' TransformOperation (Prelude.Maybe RenameColumnOperation)
transformOperation_renameColumnOperation = Lens.lens (\TransformOperation' {renameColumnOperation} -> renameColumnOperation) (\s@TransformOperation' {} a -> s {renameColumnOperation = a} :: TransformOperation)

-- | An operation that tags a column with additional information.
transformOperation_tagColumnOperation :: Lens.Lens' TransformOperation (Prelude.Maybe TagColumnOperation)
transformOperation_tagColumnOperation = Lens.lens (\TransformOperation' {tagColumnOperation} -> tagColumnOperation) (\s@TransformOperation' {} a -> s {tagColumnOperation = a} :: TransformOperation)

-- | Undocumented member.
transformOperation_untagColumnOperation :: Lens.Lens' TransformOperation (Prelude.Maybe UntagColumnOperation)
transformOperation_untagColumnOperation = Lens.lens (\TransformOperation' {untagColumnOperation} -> untagColumnOperation) (\s@TransformOperation' {} a -> s {untagColumnOperation = a} :: TransformOperation)

instance Data.FromJSON TransformOperation where
  parseJSON =
    Data.withObject
      "TransformOperation"
      ( \x ->
          TransformOperation'
            Prelude.<$> (x Data..:? "CastColumnTypeOperation")
            Prelude.<*> (x Data..:? "CreateColumnsOperation")
            Prelude.<*> (x Data..:? "FilterOperation")
            Prelude.<*> (x Data..:? "ProjectOperation")
            Prelude.<*> (x Data..:? "RenameColumnOperation")
            Prelude.<*> (x Data..:? "TagColumnOperation")
            Prelude.<*> (x Data..:? "UntagColumnOperation")
      )

instance Prelude.Hashable TransformOperation where
  hashWithSalt _salt TransformOperation' {..} =
    _salt
      `Prelude.hashWithSalt` castColumnTypeOperation
      `Prelude.hashWithSalt` createColumnsOperation
      `Prelude.hashWithSalt` filterOperation
      `Prelude.hashWithSalt` projectOperation
      `Prelude.hashWithSalt` renameColumnOperation
      `Prelude.hashWithSalt` tagColumnOperation
      `Prelude.hashWithSalt` untagColumnOperation

instance Prelude.NFData TransformOperation where
  rnf TransformOperation' {..} =
    Prelude.rnf castColumnTypeOperation
      `Prelude.seq` Prelude.rnf createColumnsOperation
      `Prelude.seq` Prelude.rnf filterOperation
      `Prelude.seq` Prelude.rnf projectOperation
      `Prelude.seq` Prelude.rnf renameColumnOperation
      `Prelude.seq` Prelude.rnf tagColumnOperation
      `Prelude.seq` Prelude.rnf untagColumnOperation

instance Data.ToJSON TransformOperation where
  toJSON TransformOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CastColumnTypeOperation" Data..=)
              Prelude.<$> castColumnTypeOperation,
            ("CreateColumnsOperation" Data..=)
              Prelude.<$> createColumnsOperation,
            ("FilterOperation" Data..=)
              Prelude.<$> filterOperation,
            ("ProjectOperation" Data..=)
              Prelude.<$> projectOperation,
            ("RenameColumnOperation" Data..=)
              Prelude.<$> renameColumnOperation,
            ("TagColumnOperation" Data..=)
              Prelude.<$> tagColumnOperation,
            ("UntagColumnOperation" Data..=)
              Prelude.<$> untagColumnOperation
          ]
      )

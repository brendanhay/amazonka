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
-- Module      : Amazonka.Glue.Types.RenameField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RenameField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that renames a single data property key.
--
-- /See:/ 'newRenameField' smart constructor.
data RenameField = RenameField'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | A JSON path to a variable in the data structure for the source data.
    sourcePath :: [Prelude.Text],
    -- | A JSON path to a variable in the data structure for the target data.
    targetPath :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenameField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'renameField_name' - The name of the transform node.
--
-- 'inputs', 'renameField_inputs' - The data inputs identified by their node names.
--
-- 'sourcePath', 'renameField_sourcePath' - A JSON path to a variable in the data structure for the source data.
--
-- 'targetPath', 'renameField_targetPath' - A JSON path to a variable in the data structure for the target data.
newRenameField ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  RenameField
newRenameField pName_ pInputs_ =
  RenameField'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      sourcePath = Prelude.mempty,
      targetPath = Prelude.mempty
    }

-- | The name of the transform node.
renameField_name :: Lens.Lens' RenameField Prelude.Text
renameField_name = Lens.lens (\RenameField' {name} -> name) (\s@RenameField' {} a -> s {name = a} :: RenameField)

-- | The data inputs identified by their node names.
renameField_inputs :: Lens.Lens' RenameField (Prelude.NonEmpty Prelude.Text)
renameField_inputs = Lens.lens (\RenameField' {inputs} -> inputs) (\s@RenameField' {} a -> s {inputs = a} :: RenameField) Prelude.. Lens.coerced

-- | A JSON path to a variable in the data structure for the source data.
renameField_sourcePath :: Lens.Lens' RenameField [Prelude.Text]
renameField_sourcePath = Lens.lens (\RenameField' {sourcePath} -> sourcePath) (\s@RenameField' {} a -> s {sourcePath = a} :: RenameField) Prelude.. Lens.coerced

-- | A JSON path to a variable in the data structure for the target data.
renameField_targetPath :: Lens.Lens' RenameField [Prelude.Text]
renameField_targetPath = Lens.lens (\RenameField' {targetPath} -> targetPath) (\s@RenameField' {} a -> s {targetPath = a} :: RenameField) Prelude.. Lens.coerced

instance Core.FromJSON RenameField where
  parseJSON =
    Core.withObject
      "RenameField"
      ( \x ->
          RenameField'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "Inputs")
            Prelude.<*> (x Core..:? "SourcePath" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TargetPath" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable RenameField where
  hashWithSalt _salt RenameField' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` sourcePath
      `Prelude.hashWithSalt` targetPath

instance Prelude.NFData RenameField where
  rnf RenameField' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf sourcePath
      `Prelude.seq` Prelude.rnf targetPath

instance Core.ToJSON RenameField where
  toJSON RenameField' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Inputs" Core..= inputs),
            Prelude.Just ("SourcePath" Core..= sourcePath),
            Prelude.Just ("TargetPath" Core..= targetPath)
          ]
      )

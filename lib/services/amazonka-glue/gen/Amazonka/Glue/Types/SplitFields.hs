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
-- Module      : Amazonka.Glue.Types.SplitFields
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SplitFields where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that splits data property keys into two
-- @DynamicFrames@. The output is a collection of @DynamicFrames@: one with
-- selected data property keys, and one with the remaining data property
-- keys.
--
-- /See:/ 'newSplitFields' smart constructor.
data SplitFields = SplitFields'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | A JSON path to a variable in the data structure.
    paths :: [[Prelude.Text]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SplitFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'splitFields_name' - The name of the transform node.
--
-- 'inputs', 'splitFields_inputs' - The data inputs identified by their node names.
--
-- 'paths', 'splitFields_paths' - A JSON path to a variable in the data structure.
newSplitFields ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  SplitFields
newSplitFields pName_ pInputs_ =
  SplitFields'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      paths = Prelude.mempty
    }

-- | The name of the transform node.
splitFields_name :: Lens.Lens' SplitFields Prelude.Text
splitFields_name = Lens.lens (\SplitFields' {name} -> name) (\s@SplitFields' {} a -> s {name = a} :: SplitFields)

-- | The data inputs identified by their node names.
splitFields_inputs :: Lens.Lens' SplitFields (Prelude.NonEmpty Prelude.Text)
splitFields_inputs = Lens.lens (\SplitFields' {inputs} -> inputs) (\s@SplitFields' {} a -> s {inputs = a} :: SplitFields) Prelude.. Lens.coerced

-- | A JSON path to a variable in the data structure.
splitFields_paths :: Lens.Lens' SplitFields [[Prelude.Text]]
splitFields_paths = Lens.lens (\SplitFields' {paths} -> paths) (\s@SplitFields' {} a -> s {paths = a} :: SplitFields) Prelude.. Lens.coerced

instance Data.FromJSON SplitFields where
  parseJSON =
    Data.withObject
      "SplitFields"
      ( \x ->
          SplitFields'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..:? "Paths" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SplitFields where
  hashWithSalt _salt SplitFields' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` paths

instance Prelude.NFData SplitFields where
  rnf SplitFields' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf inputs `Prelude.seq`
        Prelude.rnf paths

instance Data.ToJSON SplitFields where
  toJSON SplitFields' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Paths" Data..= paths)
          ]
      )

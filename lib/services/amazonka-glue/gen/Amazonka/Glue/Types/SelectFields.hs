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
-- Module      : Amazonka.Glue.Types.SelectFields
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SelectFields where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that chooses the data property keys that you want
-- to keep.
--
-- /See:/ 'newSelectFields' smart constructor.
data SelectFields = SelectFields'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | A JSON path to a variable in the data structure.
    paths :: [[Prelude.Text]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'selectFields_name' - The name of the transform node.
--
-- 'inputs', 'selectFields_inputs' - The data inputs identified by their node names.
--
-- 'paths', 'selectFields_paths' - A JSON path to a variable in the data structure.
newSelectFields ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  SelectFields
newSelectFields pName_ pInputs_ =
  SelectFields'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      paths = Prelude.mempty
    }

-- | The name of the transform node.
selectFields_name :: Lens.Lens' SelectFields Prelude.Text
selectFields_name = Lens.lens (\SelectFields' {name} -> name) (\s@SelectFields' {} a -> s {name = a} :: SelectFields)

-- | The data inputs identified by their node names.
selectFields_inputs :: Lens.Lens' SelectFields (Prelude.NonEmpty Prelude.Text)
selectFields_inputs = Lens.lens (\SelectFields' {inputs} -> inputs) (\s@SelectFields' {} a -> s {inputs = a} :: SelectFields) Prelude.. Lens.coerced

-- | A JSON path to a variable in the data structure.
selectFields_paths :: Lens.Lens' SelectFields [[Prelude.Text]]
selectFields_paths = Lens.lens (\SelectFields' {paths} -> paths) (\s@SelectFields' {} a -> s {paths = a} :: SelectFields) Prelude.. Lens.coerced

instance Data.FromJSON SelectFields where
  parseJSON =
    Data.withObject
      "SelectFields"
      ( \x ->
          SelectFields'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..:? "Paths" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SelectFields where
  hashWithSalt _salt SelectFields' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` paths

instance Prelude.NFData SelectFields where
  rnf SelectFields' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf inputs `Prelude.seq`
        Prelude.rnf paths

instance Data.ToJSON SelectFields where
  toJSON SelectFields' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Paths" Data..= paths)
          ]
      )

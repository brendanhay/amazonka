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
-- Module      : Amazonka.Glue.Types.SelectFromCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SelectFromCollection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that chooses one @DynamicFrame@ from a collection
-- of @DynamicFrames@. The output is the selected @DynamicFrame@
--
-- /See:/ 'newSelectFromCollection' smart constructor.
data SelectFromCollection = SelectFromCollection'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The index for the DynamicFrame to be selected.
    index :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectFromCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'selectFromCollection_name' - The name of the transform node.
--
-- 'inputs', 'selectFromCollection_inputs' - The data inputs identified by their node names.
--
-- 'index', 'selectFromCollection_index' - The index for the DynamicFrame to be selected.
newSelectFromCollection ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'index'
  Prelude.Natural ->
  SelectFromCollection
newSelectFromCollection pName_ pInputs_ pIndex_ =
  SelectFromCollection'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      index = pIndex_
    }

-- | The name of the transform node.
selectFromCollection_name :: Lens.Lens' SelectFromCollection Prelude.Text
selectFromCollection_name = Lens.lens (\SelectFromCollection' {name} -> name) (\s@SelectFromCollection' {} a -> s {name = a} :: SelectFromCollection)

-- | The data inputs identified by their node names.
selectFromCollection_inputs :: Lens.Lens' SelectFromCollection (Prelude.NonEmpty Prelude.Text)
selectFromCollection_inputs = Lens.lens (\SelectFromCollection' {inputs} -> inputs) (\s@SelectFromCollection' {} a -> s {inputs = a} :: SelectFromCollection) Prelude.. Lens.coerced

-- | The index for the DynamicFrame to be selected.
selectFromCollection_index :: Lens.Lens' SelectFromCollection Prelude.Natural
selectFromCollection_index = Lens.lens (\SelectFromCollection' {index} -> index) (\s@SelectFromCollection' {} a -> s {index = a} :: SelectFromCollection)

instance Data.FromJSON SelectFromCollection where
  parseJSON =
    Data.withObject
      "SelectFromCollection"
      ( \x ->
          SelectFromCollection'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Index")
      )

instance Prelude.Hashable SelectFromCollection where
  hashWithSalt _salt SelectFromCollection' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` index

instance Prelude.NFData SelectFromCollection where
  rnf SelectFromCollection' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf index

instance Data.ToJSON SelectFromCollection where
  toJSON SelectFromCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Index" Data..= index)
          ]
      )

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
-- Module      : Amazonka.Glue.Types.FillMissingValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FillMissingValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that locates records in the dataset that have
-- missing values and adds a new field with a value determined by
-- imputation. The input data set is used to train the machine learning
-- model that determines what the missing value should be.
--
-- /See:/ 'newFillMissingValues' smart constructor.
data FillMissingValues = FillMissingValues'
  { -- | A JSON path to a variable in the data structure for the dataset that is
    -- filled.
    filledPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | A JSON path to a variable in the data structure for the dataset that is
    -- imputed.
    imputedPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FillMissingValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filledPath', 'fillMissingValues_filledPath' - A JSON path to a variable in the data structure for the dataset that is
-- filled.
--
-- 'name', 'fillMissingValues_name' - The name of the transform node.
--
-- 'inputs', 'fillMissingValues_inputs' - The data inputs identified by their node names.
--
-- 'imputedPath', 'fillMissingValues_imputedPath' - A JSON path to a variable in the data structure for the dataset that is
-- imputed.
newFillMissingValues ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'imputedPath'
  Prelude.Text ->
  FillMissingValues
newFillMissingValues pName_ pInputs_ pImputedPath_ =
  FillMissingValues'
    { filledPath = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      imputedPath = pImputedPath_
    }

-- | A JSON path to a variable in the data structure for the dataset that is
-- filled.
fillMissingValues_filledPath :: Lens.Lens' FillMissingValues (Prelude.Maybe Prelude.Text)
fillMissingValues_filledPath = Lens.lens (\FillMissingValues' {filledPath} -> filledPath) (\s@FillMissingValues' {} a -> s {filledPath = a} :: FillMissingValues)

-- | The name of the transform node.
fillMissingValues_name :: Lens.Lens' FillMissingValues Prelude.Text
fillMissingValues_name = Lens.lens (\FillMissingValues' {name} -> name) (\s@FillMissingValues' {} a -> s {name = a} :: FillMissingValues)

-- | The data inputs identified by their node names.
fillMissingValues_inputs :: Lens.Lens' FillMissingValues (Prelude.NonEmpty Prelude.Text)
fillMissingValues_inputs = Lens.lens (\FillMissingValues' {inputs} -> inputs) (\s@FillMissingValues' {} a -> s {inputs = a} :: FillMissingValues) Prelude.. Lens.coerced

-- | A JSON path to a variable in the data structure for the dataset that is
-- imputed.
fillMissingValues_imputedPath :: Lens.Lens' FillMissingValues Prelude.Text
fillMissingValues_imputedPath = Lens.lens (\FillMissingValues' {imputedPath} -> imputedPath) (\s@FillMissingValues' {} a -> s {imputedPath = a} :: FillMissingValues)

instance Data.FromJSON FillMissingValues where
  parseJSON =
    Data.withObject
      "FillMissingValues"
      ( \x ->
          FillMissingValues'
            Prelude.<$> (x Data..:? "FilledPath")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "ImputedPath")
      )

instance Prelude.Hashable FillMissingValues where
  hashWithSalt _salt FillMissingValues' {..} =
    _salt
      `Prelude.hashWithSalt` filledPath
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` imputedPath

instance Prelude.NFData FillMissingValues where
  rnf FillMissingValues' {..} =
    Prelude.rnf filledPath
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf imputedPath

instance Data.ToJSON FillMissingValues where
  toJSON FillMissingValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilledPath" Data..=) Prelude.<$> filledPath,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("ImputedPath" Data..= imputedPath)
          ]
      )

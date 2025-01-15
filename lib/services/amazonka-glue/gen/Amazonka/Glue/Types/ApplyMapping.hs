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
-- Module      : Amazonka.Glue.Types.ApplyMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ApplyMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Mapping
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that maps data property keys in the data source to
-- data property keys in the data target. You can rename keys, modify the
-- data types for keys, and choose which keys to drop from the dataset.
--
-- /See:/ 'newApplyMapping' smart constructor.
data ApplyMapping = ApplyMapping'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies the mapping of data property keys in the data source to data
    -- property keys in the data target.
    mapping :: [Mapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplyMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'applyMapping_name' - The name of the transform node.
--
-- 'inputs', 'applyMapping_inputs' - The data inputs identified by their node names.
--
-- 'mapping', 'applyMapping_mapping' - Specifies the mapping of data property keys in the data source to data
-- property keys in the data target.
newApplyMapping ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  ApplyMapping
newApplyMapping pName_ pInputs_ =
  ApplyMapping'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      mapping = Prelude.mempty
    }

-- | The name of the transform node.
applyMapping_name :: Lens.Lens' ApplyMapping Prelude.Text
applyMapping_name = Lens.lens (\ApplyMapping' {name} -> name) (\s@ApplyMapping' {} a -> s {name = a} :: ApplyMapping)

-- | The data inputs identified by their node names.
applyMapping_inputs :: Lens.Lens' ApplyMapping (Prelude.NonEmpty Prelude.Text)
applyMapping_inputs = Lens.lens (\ApplyMapping' {inputs} -> inputs) (\s@ApplyMapping' {} a -> s {inputs = a} :: ApplyMapping) Prelude.. Lens.coerced

-- | Specifies the mapping of data property keys in the data source to data
-- property keys in the data target.
applyMapping_mapping :: Lens.Lens' ApplyMapping [Mapping]
applyMapping_mapping = Lens.lens (\ApplyMapping' {mapping} -> mapping) (\s@ApplyMapping' {} a -> s {mapping = a} :: ApplyMapping) Prelude.. Lens.coerced

instance Data.FromJSON ApplyMapping where
  parseJSON =
    Data.withObject
      "ApplyMapping"
      ( \x ->
          ApplyMapping'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..:? "Mapping" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ApplyMapping where
  hashWithSalt _salt ApplyMapping' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` mapping

instance Prelude.NFData ApplyMapping where
  rnf ApplyMapping' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf inputs `Prelude.seq`
        Prelude.rnf mapping

instance Data.ToJSON ApplyMapping where
  toJSON ApplyMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Mapping" Data..= mapping)
          ]
      )

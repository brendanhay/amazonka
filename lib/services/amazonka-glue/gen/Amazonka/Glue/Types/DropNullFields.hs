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
-- Module      : Amazonka.Glue.Types.DropNullFields
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DropNullFields where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.NullCheckBoxList
import Amazonka.Glue.Types.NullValueField
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that removes columns from the dataset if all
-- values in the column are \'null\'. By default, Glue Studio will
-- recognize null objects, but some values such as empty strings, strings
-- that are \"null\", -1 integers or other placeholders such as zeros, are
-- not automatically recognized as nulls.
--
-- /See:/ 'newDropNullFields' smart constructor.
data DropNullFields = DropNullFields'
  { -- | A structure that represents whether certain values are recognized as
    -- null values for removal.
    nullCheckBoxList :: Prelude.Maybe NullCheckBoxList,
    -- | A structure that specifies a list of NullValueField structures that
    -- represent a custom null value such as zero or other value being used as
    -- a null placeholder unique to the dataset.
    --
    -- The @DropNullFields@ transform removes custom null values only if both
    -- the value of the null placeholder and the datatype match the data.
    nullTextList :: Prelude.Maybe [NullValueField],
    -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DropNullFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nullCheckBoxList', 'dropNullFields_nullCheckBoxList' - A structure that represents whether certain values are recognized as
-- null values for removal.
--
-- 'nullTextList', 'dropNullFields_nullTextList' - A structure that specifies a list of NullValueField structures that
-- represent a custom null value such as zero or other value being used as
-- a null placeholder unique to the dataset.
--
-- The @DropNullFields@ transform removes custom null values only if both
-- the value of the null placeholder and the datatype match the data.
--
-- 'name', 'dropNullFields_name' - The name of the transform node.
--
-- 'inputs', 'dropNullFields_inputs' - The data inputs identified by their node names.
newDropNullFields ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  DropNullFields
newDropNullFields pName_ pInputs_ =
  DropNullFields'
    { nullCheckBoxList = Prelude.Nothing,
      nullTextList = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_
    }

-- | A structure that represents whether certain values are recognized as
-- null values for removal.
dropNullFields_nullCheckBoxList :: Lens.Lens' DropNullFields (Prelude.Maybe NullCheckBoxList)
dropNullFields_nullCheckBoxList = Lens.lens (\DropNullFields' {nullCheckBoxList} -> nullCheckBoxList) (\s@DropNullFields' {} a -> s {nullCheckBoxList = a} :: DropNullFields)

-- | A structure that specifies a list of NullValueField structures that
-- represent a custom null value such as zero or other value being used as
-- a null placeholder unique to the dataset.
--
-- The @DropNullFields@ transform removes custom null values only if both
-- the value of the null placeholder and the datatype match the data.
dropNullFields_nullTextList :: Lens.Lens' DropNullFields (Prelude.Maybe [NullValueField])
dropNullFields_nullTextList = Lens.lens (\DropNullFields' {nullTextList} -> nullTextList) (\s@DropNullFields' {} a -> s {nullTextList = a} :: DropNullFields) Prelude.. Lens.mapping Lens.coerced

-- | The name of the transform node.
dropNullFields_name :: Lens.Lens' DropNullFields Prelude.Text
dropNullFields_name = Lens.lens (\DropNullFields' {name} -> name) (\s@DropNullFields' {} a -> s {name = a} :: DropNullFields)

-- | The data inputs identified by their node names.
dropNullFields_inputs :: Lens.Lens' DropNullFields (Prelude.NonEmpty Prelude.Text)
dropNullFields_inputs = Lens.lens (\DropNullFields' {inputs} -> inputs) (\s@DropNullFields' {} a -> s {inputs = a} :: DropNullFields) Prelude.. Lens.coerced

instance Data.FromJSON DropNullFields where
  parseJSON =
    Data.withObject
      "DropNullFields"
      ( \x ->
          DropNullFields'
            Prelude.<$> (x Data..:? "NullCheckBoxList")
            Prelude.<*> (x Data..:? "NullTextList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
      )

instance Prelude.Hashable DropNullFields where
  hashWithSalt _salt DropNullFields' {..} =
    _salt
      `Prelude.hashWithSalt` nullCheckBoxList
      `Prelude.hashWithSalt` nullTextList
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs

instance Prelude.NFData DropNullFields where
  rnf DropNullFields' {..} =
    Prelude.rnf nullCheckBoxList
      `Prelude.seq` Prelude.rnf nullTextList
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs

instance Data.ToJSON DropNullFields where
  toJSON DropNullFields' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NullCheckBoxList" Data..=)
              Prelude.<$> nullCheckBoxList,
            ("NullTextList" Data..=) Prelude.<$> nullTextList,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs)
          ]
      )

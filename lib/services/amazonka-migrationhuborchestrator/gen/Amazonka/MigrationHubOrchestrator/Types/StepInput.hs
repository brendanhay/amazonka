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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.StepInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.StepInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A map of key value pairs that is generated when you create a migration
-- workflow. The key value pairs will differ based on your selection of the
-- template.
--
-- /See:/ 'newStepInput' smart constructor.
data StepInput = StepInput'
  { -- | The value of the integer.
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | List of string values.
    listOfStringsValue :: Prelude.Maybe [Prelude.Text],
    -- | Map of string values.
    mapOfStringValue :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | String value.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integerValue', 'stepInput_integerValue' - The value of the integer.
--
-- 'listOfStringsValue', 'stepInput_listOfStringsValue' - List of string values.
--
-- 'mapOfStringValue', 'stepInput_mapOfStringValue' - Map of string values.
--
-- 'stringValue', 'stepInput_stringValue' - String value.
newStepInput ::
  StepInput
newStepInput =
  StepInput'
    { integerValue = Prelude.Nothing,
      listOfStringsValue = Prelude.Nothing,
      mapOfStringValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | The value of the integer.
stepInput_integerValue :: Lens.Lens' StepInput (Prelude.Maybe Prelude.Int)
stepInput_integerValue = Lens.lens (\StepInput' {integerValue} -> integerValue) (\s@StepInput' {} a -> s {integerValue = a} :: StepInput)

-- | List of string values.
stepInput_listOfStringsValue :: Lens.Lens' StepInput (Prelude.Maybe [Prelude.Text])
stepInput_listOfStringsValue = Lens.lens (\StepInput' {listOfStringsValue} -> listOfStringsValue) (\s@StepInput' {} a -> s {listOfStringsValue = a} :: StepInput) Prelude.. Lens.mapping Lens.coerced

-- | Map of string values.
stepInput_mapOfStringValue :: Lens.Lens' StepInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stepInput_mapOfStringValue = Lens.lens (\StepInput' {mapOfStringValue} -> mapOfStringValue) (\s@StepInput' {} a -> s {mapOfStringValue = a} :: StepInput) Prelude.. Lens.mapping Lens.coerced

-- | String value.
stepInput_stringValue :: Lens.Lens' StepInput (Prelude.Maybe Prelude.Text)
stepInput_stringValue = Lens.lens (\StepInput' {stringValue} -> stringValue) (\s@StepInput' {} a -> s {stringValue = a} :: StepInput)

instance Data.FromJSON StepInput where
  parseJSON =
    Data.withObject
      "StepInput"
      ( \x ->
          StepInput'
            Prelude.<$> (x Data..:? "integerValue")
            Prelude.<*> ( x
                            Data..:? "listOfStringsValue"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "mapOfStringValue"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable StepInput where
  hashWithSalt _salt StepInput' {..} =
    _salt
      `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` listOfStringsValue
      `Prelude.hashWithSalt` mapOfStringValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData StepInput where
  rnf StepInput' {..} =
    Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf listOfStringsValue
      `Prelude.seq` Prelude.rnf mapOfStringValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON StepInput where
  toJSON StepInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("integerValue" Data..=) Prelude.<$> integerValue,
            ("listOfStringsValue" Data..=)
              Prelude.<$> listOfStringsValue,
            ("mapOfStringValue" Data..=)
              Prelude.<$> mapOfStringValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )

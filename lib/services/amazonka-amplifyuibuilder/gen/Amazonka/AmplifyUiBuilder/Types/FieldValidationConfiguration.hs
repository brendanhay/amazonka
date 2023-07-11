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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FieldValidationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FieldValidationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the validation configuration for a field.
--
-- /See:/ 'newFieldValidationConfiguration' smart constructor.
data FieldValidationConfiguration = FieldValidationConfiguration'
  { -- | The validation to perform on a number value.
    numValues :: Prelude.Maybe [Prelude.Int],
    -- | The validation to perform on a string value.
    strValues :: Prelude.Maybe [Prelude.Text],
    -- | The validation message to display.
    validationMessage :: Prelude.Maybe Prelude.Text,
    -- | The validation to perform on an object type.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numValues', 'fieldValidationConfiguration_numValues' - The validation to perform on a number value.
--
-- 'strValues', 'fieldValidationConfiguration_strValues' - The validation to perform on a string value.
--
-- 'validationMessage', 'fieldValidationConfiguration_validationMessage' - The validation message to display.
--
-- 'type'', 'fieldValidationConfiguration_type' - The validation to perform on an object type.
newFieldValidationConfiguration ::
  -- | 'type''
  Prelude.Text ->
  FieldValidationConfiguration
newFieldValidationConfiguration pType_ =
  FieldValidationConfiguration'
    { numValues =
        Prelude.Nothing,
      strValues = Prelude.Nothing,
      validationMessage = Prelude.Nothing,
      type' = pType_
    }

-- | The validation to perform on a number value.
fieldValidationConfiguration_numValues :: Lens.Lens' FieldValidationConfiguration (Prelude.Maybe [Prelude.Int])
fieldValidationConfiguration_numValues = Lens.lens (\FieldValidationConfiguration' {numValues} -> numValues) (\s@FieldValidationConfiguration' {} a -> s {numValues = a} :: FieldValidationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The validation to perform on a string value.
fieldValidationConfiguration_strValues :: Lens.Lens' FieldValidationConfiguration (Prelude.Maybe [Prelude.Text])
fieldValidationConfiguration_strValues = Lens.lens (\FieldValidationConfiguration' {strValues} -> strValues) (\s@FieldValidationConfiguration' {} a -> s {strValues = a} :: FieldValidationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The validation message to display.
fieldValidationConfiguration_validationMessage :: Lens.Lens' FieldValidationConfiguration (Prelude.Maybe Prelude.Text)
fieldValidationConfiguration_validationMessage = Lens.lens (\FieldValidationConfiguration' {validationMessage} -> validationMessage) (\s@FieldValidationConfiguration' {} a -> s {validationMessage = a} :: FieldValidationConfiguration)

-- | The validation to perform on an object type.
fieldValidationConfiguration_type :: Lens.Lens' FieldValidationConfiguration Prelude.Text
fieldValidationConfiguration_type = Lens.lens (\FieldValidationConfiguration' {type'} -> type') (\s@FieldValidationConfiguration' {} a -> s {type' = a} :: FieldValidationConfiguration)

instance Data.FromJSON FieldValidationConfiguration where
  parseJSON =
    Data.withObject
      "FieldValidationConfiguration"
      ( \x ->
          FieldValidationConfiguration'
            Prelude.<$> (x Data..:? "numValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "strValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "validationMessage")
            Prelude.<*> (x Data..: "type")
      )

instance
  Prelude.Hashable
    FieldValidationConfiguration
  where
  hashWithSalt _salt FieldValidationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` numValues
      `Prelude.hashWithSalt` strValues
      `Prelude.hashWithSalt` validationMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FieldValidationConfiguration where
  rnf FieldValidationConfiguration' {..} =
    Prelude.rnf numValues
      `Prelude.seq` Prelude.rnf strValues
      `Prelude.seq` Prelude.rnf validationMessage
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON FieldValidationConfiguration where
  toJSON FieldValidationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("numValues" Data..=) Prelude.<$> numValues,
            ("strValues" Data..=) Prelude.<$> strValues,
            ("validationMessage" Data..=)
              Prelude.<$> validationMessage,
            Prelude.Just ("type" Data..= type')
          ]
      )

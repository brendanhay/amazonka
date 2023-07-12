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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FieldConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FieldConfig where

import Amazonka.AmplifyUiBuilder.Types.FieldInputConfig
import Amazonka.AmplifyUiBuilder.Types.FieldPosition
import Amazonka.AmplifyUiBuilder.Types.FieldValidationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration information for a field in a table.
--
-- /See:/ 'newFieldConfig' smart constructor.
data FieldConfig = FieldConfig'
  { -- | Specifies whether to hide a field.
    excluded :: Prelude.Maybe Prelude.Bool,
    -- | Describes the configuration for the default input value to display for a
    -- field.
    inputType :: Prelude.Maybe FieldInputConfig,
    -- | The label for the field.
    label :: Prelude.Maybe Prelude.Text,
    -- | Specifies the field position.
    position :: Prelude.Maybe FieldPosition,
    -- | The validations to perform on the value in the field.
    validations :: Prelude.Maybe [FieldValidationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excluded', 'fieldConfig_excluded' - Specifies whether to hide a field.
--
-- 'inputType', 'fieldConfig_inputType' - Describes the configuration for the default input value to display for a
-- field.
--
-- 'label', 'fieldConfig_label' - The label for the field.
--
-- 'position', 'fieldConfig_position' - Specifies the field position.
--
-- 'validations', 'fieldConfig_validations' - The validations to perform on the value in the field.
newFieldConfig ::
  FieldConfig
newFieldConfig =
  FieldConfig'
    { excluded = Prelude.Nothing,
      inputType = Prelude.Nothing,
      label = Prelude.Nothing,
      position = Prelude.Nothing,
      validations = Prelude.Nothing
    }

-- | Specifies whether to hide a field.
fieldConfig_excluded :: Lens.Lens' FieldConfig (Prelude.Maybe Prelude.Bool)
fieldConfig_excluded = Lens.lens (\FieldConfig' {excluded} -> excluded) (\s@FieldConfig' {} a -> s {excluded = a} :: FieldConfig)

-- | Describes the configuration for the default input value to display for a
-- field.
fieldConfig_inputType :: Lens.Lens' FieldConfig (Prelude.Maybe FieldInputConfig)
fieldConfig_inputType = Lens.lens (\FieldConfig' {inputType} -> inputType) (\s@FieldConfig' {} a -> s {inputType = a} :: FieldConfig)

-- | The label for the field.
fieldConfig_label :: Lens.Lens' FieldConfig (Prelude.Maybe Prelude.Text)
fieldConfig_label = Lens.lens (\FieldConfig' {label} -> label) (\s@FieldConfig' {} a -> s {label = a} :: FieldConfig)

-- | Specifies the field position.
fieldConfig_position :: Lens.Lens' FieldConfig (Prelude.Maybe FieldPosition)
fieldConfig_position = Lens.lens (\FieldConfig' {position} -> position) (\s@FieldConfig' {} a -> s {position = a} :: FieldConfig)

-- | The validations to perform on the value in the field.
fieldConfig_validations :: Lens.Lens' FieldConfig (Prelude.Maybe [FieldValidationConfiguration])
fieldConfig_validations = Lens.lens (\FieldConfig' {validations} -> validations) (\s@FieldConfig' {} a -> s {validations = a} :: FieldConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FieldConfig where
  parseJSON =
    Data.withObject
      "FieldConfig"
      ( \x ->
          FieldConfig'
            Prelude.<$> (x Data..:? "excluded")
            Prelude.<*> (x Data..:? "inputType")
            Prelude.<*> (x Data..:? "label")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "validations" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FieldConfig where
  hashWithSalt _salt FieldConfig' {..} =
    _salt
      `Prelude.hashWithSalt` excluded
      `Prelude.hashWithSalt` inputType
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` validations

instance Prelude.NFData FieldConfig where
  rnf FieldConfig' {..} =
    Prelude.rnf excluded
      `Prelude.seq` Prelude.rnf inputType
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf validations

instance Data.ToJSON FieldConfig where
  toJSON FieldConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("excluded" Data..=) Prelude.<$> excluded,
            ("inputType" Data..=) Prelude.<$> inputType,
            ("label" Data..=) Prelude.<$> label,
            ("position" Data..=) Prelude.<$> position,
            ("validations" Data..=) Prelude.<$> validations
          ]
      )

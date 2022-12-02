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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The label for the field.
    label :: Prelude.Maybe Prelude.Text,
    -- | The validations to perform on the value in the field.
    validations :: Prelude.Maybe [FieldValidationConfiguration],
    -- | Describes the configuration for the default input value to display for a
    -- field.
    inputType :: Prelude.Maybe FieldInputConfig,
    -- | Specifies the field position.
    position :: Prelude.Maybe FieldPosition,
    -- | Specifies whether to hide a field.
    excluded :: Prelude.Maybe Prelude.Bool
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
-- 'label', 'fieldConfig_label' - The label for the field.
--
-- 'validations', 'fieldConfig_validations' - The validations to perform on the value in the field.
--
-- 'inputType', 'fieldConfig_inputType' - Describes the configuration for the default input value to display for a
-- field.
--
-- 'position', 'fieldConfig_position' - Specifies the field position.
--
-- 'excluded', 'fieldConfig_excluded' - Specifies whether to hide a field.
newFieldConfig ::
  FieldConfig
newFieldConfig =
  FieldConfig'
    { label = Prelude.Nothing,
      validations = Prelude.Nothing,
      inputType = Prelude.Nothing,
      position = Prelude.Nothing,
      excluded = Prelude.Nothing
    }

-- | The label for the field.
fieldConfig_label :: Lens.Lens' FieldConfig (Prelude.Maybe Prelude.Text)
fieldConfig_label = Lens.lens (\FieldConfig' {label} -> label) (\s@FieldConfig' {} a -> s {label = a} :: FieldConfig)

-- | The validations to perform on the value in the field.
fieldConfig_validations :: Lens.Lens' FieldConfig (Prelude.Maybe [FieldValidationConfiguration])
fieldConfig_validations = Lens.lens (\FieldConfig' {validations} -> validations) (\s@FieldConfig' {} a -> s {validations = a} :: FieldConfig) Prelude.. Lens.mapping Lens.coerced

-- | Describes the configuration for the default input value to display for a
-- field.
fieldConfig_inputType :: Lens.Lens' FieldConfig (Prelude.Maybe FieldInputConfig)
fieldConfig_inputType = Lens.lens (\FieldConfig' {inputType} -> inputType) (\s@FieldConfig' {} a -> s {inputType = a} :: FieldConfig)

-- | Specifies the field position.
fieldConfig_position :: Lens.Lens' FieldConfig (Prelude.Maybe FieldPosition)
fieldConfig_position = Lens.lens (\FieldConfig' {position} -> position) (\s@FieldConfig' {} a -> s {position = a} :: FieldConfig)

-- | Specifies whether to hide a field.
fieldConfig_excluded :: Lens.Lens' FieldConfig (Prelude.Maybe Prelude.Bool)
fieldConfig_excluded = Lens.lens (\FieldConfig' {excluded} -> excluded) (\s@FieldConfig' {} a -> s {excluded = a} :: FieldConfig)

instance Data.FromJSON FieldConfig where
  parseJSON =
    Data.withObject
      "FieldConfig"
      ( \x ->
          FieldConfig'
            Prelude.<$> (x Data..:? "label")
            Prelude.<*> (x Data..:? "validations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "inputType")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "excluded")
      )

instance Prelude.Hashable FieldConfig where
  hashWithSalt _salt FieldConfig' {..} =
    _salt `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` validations
      `Prelude.hashWithSalt` inputType
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` excluded

instance Prelude.NFData FieldConfig where
  rnf FieldConfig' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf validations
      `Prelude.seq` Prelude.rnf inputType
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf excluded

instance Data.ToJSON FieldConfig where
  toJSON FieldConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("label" Data..=) Prelude.<$> label,
            ("validations" Data..=) Prelude.<$> validations,
            ("inputType" Data..=) Prelude.<$> inputType,
            ("position" Data..=) Prelude.<$> position,
            ("excluded" Data..=) Prelude.<$> excluded
          ]
      )

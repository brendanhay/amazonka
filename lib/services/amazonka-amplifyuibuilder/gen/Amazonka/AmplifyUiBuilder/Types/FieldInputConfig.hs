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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FieldInputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FieldInputConfig where

import Amazonka.AmplifyUiBuilder.Types.FileUploaderFieldConfig
import Amazonka.AmplifyUiBuilder.Types.ValueMappings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for the default input values to display for
-- a field.
--
-- /See:/ 'newFieldInputConfig' smart constructor.
data FieldInputConfig = FieldInputConfig'
  { -- | Specifies whether a field has a default value.
    defaultChecked :: Prelude.Maybe Prelude.Bool,
    -- | The default country code for a phone number.
    defaultCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The default value for the field.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The text to display to describe the field.
    descriptiveText :: Prelude.Maybe Prelude.Text,
    -- | The configuration for the file uploader field.
    fileUploaderConfig :: Prelude.Maybe FileUploaderFieldConfig,
    -- | Specifies whether to render the field as an array. This property is
    -- ignored if the @dataSourceType@ for the form is a Data Store.
    isArray :: Prelude.Maybe Prelude.Bool,
    -- | The maximum value to display for the field.
    maxValue :: Prelude.Maybe Prelude.Double,
    -- | The minimum value to display for the field.
    minValue :: Prelude.Maybe Prelude.Double,
    -- | The name of the field.
    name :: Prelude.Maybe Prelude.Text,
    -- | The text to display as a placeholder for the field.
    placeholder :: Prelude.Maybe Prelude.Text,
    -- | Specifies a read only field.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a field that requires input.
    required :: Prelude.Maybe Prelude.Bool,
    -- | The stepping increment for a numeric value in a field.
    step :: Prelude.Maybe Prelude.Double,
    -- | The value for the field.
    value :: Prelude.Maybe Prelude.Text,
    -- | The information to use to customize the input fields with data at
    -- runtime.
    valueMappings :: Prelude.Maybe ValueMappings,
    -- | The input type for the field.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldInputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultChecked', 'fieldInputConfig_defaultChecked' - Specifies whether a field has a default value.
--
-- 'defaultCountryCode', 'fieldInputConfig_defaultCountryCode' - The default country code for a phone number.
--
-- 'defaultValue', 'fieldInputConfig_defaultValue' - The default value for the field.
--
-- 'descriptiveText', 'fieldInputConfig_descriptiveText' - The text to display to describe the field.
--
-- 'fileUploaderConfig', 'fieldInputConfig_fileUploaderConfig' - The configuration for the file uploader field.
--
-- 'isArray', 'fieldInputConfig_isArray' - Specifies whether to render the field as an array. This property is
-- ignored if the @dataSourceType@ for the form is a Data Store.
--
-- 'maxValue', 'fieldInputConfig_maxValue' - The maximum value to display for the field.
--
-- 'minValue', 'fieldInputConfig_minValue' - The minimum value to display for the field.
--
-- 'name', 'fieldInputConfig_name' - The name of the field.
--
-- 'placeholder', 'fieldInputConfig_placeholder' - The text to display as a placeholder for the field.
--
-- 'readOnly', 'fieldInputConfig_readOnly' - Specifies a read only field.
--
-- 'required', 'fieldInputConfig_required' - Specifies a field that requires input.
--
-- 'step', 'fieldInputConfig_step' - The stepping increment for a numeric value in a field.
--
-- 'value', 'fieldInputConfig_value' - The value for the field.
--
-- 'valueMappings', 'fieldInputConfig_valueMappings' - The information to use to customize the input fields with data at
-- runtime.
--
-- 'type'', 'fieldInputConfig_type' - The input type for the field.
newFieldInputConfig ::
  -- | 'type''
  Prelude.Text ->
  FieldInputConfig
newFieldInputConfig pType_ =
  FieldInputConfig'
    { defaultChecked = Prelude.Nothing,
      defaultCountryCode = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      descriptiveText = Prelude.Nothing,
      fileUploaderConfig = Prelude.Nothing,
      isArray = Prelude.Nothing,
      maxValue = Prelude.Nothing,
      minValue = Prelude.Nothing,
      name = Prelude.Nothing,
      placeholder = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      required = Prelude.Nothing,
      step = Prelude.Nothing,
      value = Prelude.Nothing,
      valueMappings = Prelude.Nothing,
      type' = pType_
    }

-- | Specifies whether a field has a default value.
fieldInputConfig_defaultChecked :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Bool)
fieldInputConfig_defaultChecked = Lens.lens (\FieldInputConfig' {defaultChecked} -> defaultChecked) (\s@FieldInputConfig' {} a -> s {defaultChecked = a} :: FieldInputConfig)

-- | The default country code for a phone number.
fieldInputConfig_defaultCountryCode :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Text)
fieldInputConfig_defaultCountryCode = Lens.lens (\FieldInputConfig' {defaultCountryCode} -> defaultCountryCode) (\s@FieldInputConfig' {} a -> s {defaultCountryCode = a} :: FieldInputConfig)

-- | The default value for the field.
fieldInputConfig_defaultValue :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Text)
fieldInputConfig_defaultValue = Lens.lens (\FieldInputConfig' {defaultValue} -> defaultValue) (\s@FieldInputConfig' {} a -> s {defaultValue = a} :: FieldInputConfig)

-- | The text to display to describe the field.
fieldInputConfig_descriptiveText :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Text)
fieldInputConfig_descriptiveText = Lens.lens (\FieldInputConfig' {descriptiveText} -> descriptiveText) (\s@FieldInputConfig' {} a -> s {descriptiveText = a} :: FieldInputConfig)

-- | The configuration for the file uploader field.
fieldInputConfig_fileUploaderConfig :: Lens.Lens' FieldInputConfig (Prelude.Maybe FileUploaderFieldConfig)
fieldInputConfig_fileUploaderConfig = Lens.lens (\FieldInputConfig' {fileUploaderConfig} -> fileUploaderConfig) (\s@FieldInputConfig' {} a -> s {fileUploaderConfig = a} :: FieldInputConfig)

-- | Specifies whether to render the field as an array. This property is
-- ignored if the @dataSourceType@ for the form is a Data Store.
fieldInputConfig_isArray :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Bool)
fieldInputConfig_isArray = Lens.lens (\FieldInputConfig' {isArray} -> isArray) (\s@FieldInputConfig' {} a -> s {isArray = a} :: FieldInputConfig)

-- | The maximum value to display for the field.
fieldInputConfig_maxValue :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Double)
fieldInputConfig_maxValue = Lens.lens (\FieldInputConfig' {maxValue} -> maxValue) (\s@FieldInputConfig' {} a -> s {maxValue = a} :: FieldInputConfig)

-- | The minimum value to display for the field.
fieldInputConfig_minValue :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Double)
fieldInputConfig_minValue = Lens.lens (\FieldInputConfig' {minValue} -> minValue) (\s@FieldInputConfig' {} a -> s {minValue = a} :: FieldInputConfig)

-- | The name of the field.
fieldInputConfig_name :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Text)
fieldInputConfig_name = Lens.lens (\FieldInputConfig' {name} -> name) (\s@FieldInputConfig' {} a -> s {name = a} :: FieldInputConfig)

-- | The text to display as a placeholder for the field.
fieldInputConfig_placeholder :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Text)
fieldInputConfig_placeholder = Lens.lens (\FieldInputConfig' {placeholder} -> placeholder) (\s@FieldInputConfig' {} a -> s {placeholder = a} :: FieldInputConfig)

-- | Specifies a read only field.
fieldInputConfig_readOnly :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Bool)
fieldInputConfig_readOnly = Lens.lens (\FieldInputConfig' {readOnly} -> readOnly) (\s@FieldInputConfig' {} a -> s {readOnly = a} :: FieldInputConfig)

-- | Specifies a field that requires input.
fieldInputConfig_required :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Bool)
fieldInputConfig_required = Lens.lens (\FieldInputConfig' {required} -> required) (\s@FieldInputConfig' {} a -> s {required = a} :: FieldInputConfig)

-- | The stepping increment for a numeric value in a field.
fieldInputConfig_step :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Double)
fieldInputConfig_step = Lens.lens (\FieldInputConfig' {step} -> step) (\s@FieldInputConfig' {} a -> s {step = a} :: FieldInputConfig)

-- | The value for the field.
fieldInputConfig_value :: Lens.Lens' FieldInputConfig (Prelude.Maybe Prelude.Text)
fieldInputConfig_value = Lens.lens (\FieldInputConfig' {value} -> value) (\s@FieldInputConfig' {} a -> s {value = a} :: FieldInputConfig)

-- | The information to use to customize the input fields with data at
-- runtime.
fieldInputConfig_valueMappings :: Lens.Lens' FieldInputConfig (Prelude.Maybe ValueMappings)
fieldInputConfig_valueMappings = Lens.lens (\FieldInputConfig' {valueMappings} -> valueMappings) (\s@FieldInputConfig' {} a -> s {valueMappings = a} :: FieldInputConfig)

-- | The input type for the field.
fieldInputConfig_type :: Lens.Lens' FieldInputConfig Prelude.Text
fieldInputConfig_type = Lens.lens (\FieldInputConfig' {type'} -> type') (\s@FieldInputConfig' {} a -> s {type' = a} :: FieldInputConfig)

instance Data.FromJSON FieldInputConfig where
  parseJSON =
    Data.withObject
      "FieldInputConfig"
      ( \x ->
          FieldInputConfig'
            Prelude.<$> (x Data..:? "defaultChecked")
            Prelude.<*> (x Data..:? "defaultCountryCode")
            Prelude.<*> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "descriptiveText")
            Prelude.<*> (x Data..:? "fileUploaderConfig")
            Prelude.<*> (x Data..:? "isArray")
            Prelude.<*> (x Data..:? "maxValue")
            Prelude.<*> (x Data..:? "minValue")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "placeholder")
            Prelude.<*> (x Data..:? "readOnly")
            Prelude.<*> (x Data..:? "required")
            Prelude.<*> (x Data..:? "step")
            Prelude.<*> (x Data..:? "value")
            Prelude.<*> (x Data..:? "valueMappings")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable FieldInputConfig where
  hashWithSalt _salt FieldInputConfig' {..} =
    _salt
      `Prelude.hashWithSalt` defaultChecked
      `Prelude.hashWithSalt` defaultCountryCode
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` descriptiveText
      `Prelude.hashWithSalt` fileUploaderConfig
      `Prelude.hashWithSalt` isArray
      `Prelude.hashWithSalt` maxValue
      `Prelude.hashWithSalt` minValue
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` placeholder
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` required
      `Prelude.hashWithSalt` step
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` valueMappings
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FieldInputConfig where
  rnf FieldInputConfig' {..} =
    Prelude.rnf defaultChecked
      `Prelude.seq` Prelude.rnf defaultCountryCode
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf descriptiveText
      `Prelude.seq` Prelude.rnf fileUploaderConfig
      `Prelude.seq` Prelude.rnf isArray
      `Prelude.seq` Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf placeholder
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf required
      `Prelude.seq` Prelude.rnf step
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf valueMappings
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON FieldInputConfig where
  toJSON FieldInputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultChecked" Data..=)
              Prelude.<$> defaultChecked,
            ("defaultCountryCode" Data..=)
              Prelude.<$> defaultCountryCode,
            ("defaultValue" Data..=) Prelude.<$> defaultValue,
            ("descriptiveText" Data..=)
              Prelude.<$> descriptiveText,
            ("fileUploaderConfig" Data..=)
              Prelude.<$> fileUploaderConfig,
            ("isArray" Data..=) Prelude.<$> isArray,
            ("maxValue" Data..=) Prelude.<$> maxValue,
            ("minValue" Data..=) Prelude.<$> minValue,
            ("name" Data..=) Prelude.<$> name,
            ("placeholder" Data..=) Prelude.<$> placeholder,
            ("readOnly" Data..=) Prelude.<$> readOnly,
            ("required" Data..=) Prelude.<$> required,
            ("step" Data..=) Prelude.<$> step,
            ("value" Data..=) Prelude.<$> value,
            ("valueMappings" Data..=) Prelude.<$> valueMappings,
            Prelude.Just ("type" Data..= type')
          ]
      )

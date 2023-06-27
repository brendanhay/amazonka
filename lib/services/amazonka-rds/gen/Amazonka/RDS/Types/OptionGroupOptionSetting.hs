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
-- Module      : Amazonka.RDS.Types.OptionGroupOptionSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.OptionGroupOptionSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.MinimumEngineVersionPerAllowedValue

-- | Option group option settings are used to display settings available for
-- each option with their default values and other information. These
-- values are used with the DescribeOptionGroupOptions action.
--
-- /See:/ 'newOptionGroupOptionSetting' smart constructor.
data OptionGroupOptionSetting = OptionGroupOptionSetting'
  { -- | Indicates the acceptable values for the option group option.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The DB engine specific parameter type for the option group option.
    applyType :: Prelude.Maybe Prelude.Text,
    -- | The default value for the option group option.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Boolean value where true indicates that this option group option can be
    -- changed from the default value.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | Boolean value where true indicates that a value must be specified for
    -- this option setting of the option group option.
    isRequired :: Prelude.Maybe Prelude.Bool,
    -- | The minimum DB engine version required for the corresponding allowed
    -- value for this option setting.
    minimumEngineVersionPerAllowedValue :: Prelude.Maybe [MinimumEngineVersionPerAllowedValue],
    -- | The description of the option group option.
    settingDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the option group option.
    settingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionGroupOptionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'optionGroupOptionSetting_allowedValues' - Indicates the acceptable values for the option group option.
--
-- 'applyType', 'optionGroupOptionSetting_applyType' - The DB engine specific parameter type for the option group option.
--
-- 'defaultValue', 'optionGroupOptionSetting_defaultValue' - The default value for the option group option.
--
-- 'isModifiable', 'optionGroupOptionSetting_isModifiable' - Boolean value where true indicates that this option group option can be
-- changed from the default value.
--
-- 'isRequired', 'optionGroupOptionSetting_isRequired' - Boolean value where true indicates that a value must be specified for
-- this option setting of the option group option.
--
-- 'minimumEngineVersionPerAllowedValue', 'optionGroupOptionSetting_minimumEngineVersionPerAllowedValue' - The minimum DB engine version required for the corresponding allowed
-- value for this option setting.
--
-- 'settingDescription', 'optionGroupOptionSetting_settingDescription' - The description of the option group option.
--
-- 'settingName', 'optionGroupOptionSetting_settingName' - The name of the option group option.
newOptionGroupOptionSetting ::
  OptionGroupOptionSetting
newOptionGroupOptionSetting =
  OptionGroupOptionSetting'
    { allowedValues =
        Prelude.Nothing,
      applyType = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      isRequired = Prelude.Nothing,
      minimumEngineVersionPerAllowedValue =
        Prelude.Nothing,
      settingDescription = Prelude.Nothing,
      settingName = Prelude.Nothing
    }

-- | Indicates the acceptable values for the option group option.
optionGroupOptionSetting_allowedValues :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe Prelude.Text)
optionGroupOptionSetting_allowedValues = Lens.lens (\OptionGroupOptionSetting' {allowedValues} -> allowedValues) (\s@OptionGroupOptionSetting' {} a -> s {allowedValues = a} :: OptionGroupOptionSetting)

-- | The DB engine specific parameter type for the option group option.
optionGroupOptionSetting_applyType :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe Prelude.Text)
optionGroupOptionSetting_applyType = Lens.lens (\OptionGroupOptionSetting' {applyType} -> applyType) (\s@OptionGroupOptionSetting' {} a -> s {applyType = a} :: OptionGroupOptionSetting)

-- | The default value for the option group option.
optionGroupOptionSetting_defaultValue :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe Prelude.Text)
optionGroupOptionSetting_defaultValue = Lens.lens (\OptionGroupOptionSetting' {defaultValue} -> defaultValue) (\s@OptionGroupOptionSetting' {} a -> s {defaultValue = a} :: OptionGroupOptionSetting)

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
optionGroupOptionSetting_isModifiable :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe Prelude.Bool)
optionGroupOptionSetting_isModifiable = Lens.lens (\OptionGroupOptionSetting' {isModifiable} -> isModifiable) (\s@OptionGroupOptionSetting' {} a -> s {isModifiable = a} :: OptionGroupOptionSetting)

-- | Boolean value where true indicates that a value must be specified for
-- this option setting of the option group option.
optionGroupOptionSetting_isRequired :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe Prelude.Bool)
optionGroupOptionSetting_isRequired = Lens.lens (\OptionGroupOptionSetting' {isRequired} -> isRequired) (\s@OptionGroupOptionSetting' {} a -> s {isRequired = a} :: OptionGroupOptionSetting)

-- | The minimum DB engine version required for the corresponding allowed
-- value for this option setting.
optionGroupOptionSetting_minimumEngineVersionPerAllowedValue :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe [MinimumEngineVersionPerAllowedValue])
optionGroupOptionSetting_minimumEngineVersionPerAllowedValue = Lens.lens (\OptionGroupOptionSetting' {minimumEngineVersionPerAllowedValue} -> minimumEngineVersionPerAllowedValue) (\s@OptionGroupOptionSetting' {} a -> s {minimumEngineVersionPerAllowedValue = a} :: OptionGroupOptionSetting) Prelude.. Lens.mapping Lens.coerced

-- | The description of the option group option.
optionGroupOptionSetting_settingDescription :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe Prelude.Text)
optionGroupOptionSetting_settingDescription = Lens.lens (\OptionGroupOptionSetting' {settingDescription} -> settingDescription) (\s@OptionGroupOptionSetting' {} a -> s {settingDescription = a} :: OptionGroupOptionSetting)

-- | The name of the option group option.
optionGroupOptionSetting_settingName :: Lens.Lens' OptionGroupOptionSetting (Prelude.Maybe Prelude.Text)
optionGroupOptionSetting_settingName = Lens.lens (\OptionGroupOptionSetting' {settingName} -> settingName) (\s@OptionGroupOptionSetting' {} a -> s {settingName = a} :: OptionGroupOptionSetting)

instance Data.FromXML OptionGroupOptionSetting where
  parseXML x =
    OptionGroupOptionSetting'
      Prelude.<$> (x Data..@? "AllowedValues")
      Prelude.<*> (x Data..@? "ApplyType")
      Prelude.<*> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "IsModifiable")
      Prelude.<*> (x Data..@? "IsRequired")
      Prelude.<*> ( x
                      Data..@? "MinimumEngineVersionPerAllowedValue"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "MinimumEngineVersionPerAllowedValue"
                        )
                  )
      Prelude.<*> (x Data..@? "SettingDescription")
      Prelude.<*> (x Data..@? "SettingName")

instance Prelude.Hashable OptionGroupOptionSetting where
  hashWithSalt _salt OptionGroupOptionSetting' {..} =
    _salt
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` applyType
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` isModifiable
      `Prelude.hashWithSalt` isRequired
      `Prelude.hashWithSalt` minimumEngineVersionPerAllowedValue
      `Prelude.hashWithSalt` settingDescription
      `Prelude.hashWithSalt` settingName

instance Prelude.NFData OptionGroupOptionSetting where
  rnf OptionGroupOptionSetting' {..} =
    Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf applyType
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf isModifiable
      `Prelude.seq` Prelude.rnf isRequired
      `Prelude.seq` Prelude.rnf minimumEngineVersionPerAllowedValue
      `Prelude.seq` Prelude.rnf settingDescription
      `Prelude.seq` Prelude.rnf settingName

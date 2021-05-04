{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseParameter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the parameters of a database.
--
-- /See:/ 'newRelationalDatabaseParameter' smart constructor.
data RelationalDatabaseParameter = RelationalDatabaseParameter'
  { -- | Specifies the valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | Specifies the engine-specific parameter type.
    applyType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | Provides a description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates when parameter updates are applied.
    --
    -- Can be @immediate@ or @pending-reboot@.
    applyMethod :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the parameter can be modified.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the valid data type for the parameter.
    dataType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'relationalDatabaseParameter_allowedValues' - Specifies the valid range of values for the parameter.
--
-- 'parameterValue', 'relationalDatabaseParameter_parameterValue' - Specifies the value of the parameter.
--
-- 'applyType', 'relationalDatabaseParameter_applyType' - Specifies the engine-specific parameter type.
--
-- 'parameterName', 'relationalDatabaseParameter_parameterName' - Specifies the name of the parameter.
--
-- 'description', 'relationalDatabaseParameter_description' - Provides a description of the parameter.
--
-- 'applyMethod', 'relationalDatabaseParameter_applyMethod' - Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@.
--
-- 'isModifiable', 'relationalDatabaseParameter_isModifiable' - A Boolean value indicating whether the parameter can be modified.
--
-- 'dataType', 'relationalDatabaseParameter_dataType' - Specifies the valid data type for the parameter.
newRelationalDatabaseParameter ::
  RelationalDatabaseParameter
newRelationalDatabaseParameter =
  RelationalDatabaseParameter'
    { allowedValues =
        Prelude.Nothing,
      parameterValue = Prelude.Nothing,
      applyType = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      description = Prelude.Nothing,
      applyMethod = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | Specifies the valid range of values for the parameter.
relationalDatabaseParameter_allowedValues :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_allowedValues = Lens.lens (\RelationalDatabaseParameter' {allowedValues} -> allowedValues) (\s@RelationalDatabaseParameter' {} a -> s {allowedValues = a} :: RelationalDatabaseParameter)

-- | Specifies the value of the parameter.
relationalDatabaseParameter_parameterValue :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_parameterValue = Lens.lens (\RelationalDatabaseParameter' {parameterValue} -> parameterValue) (\s@RelationalDatabaseParameter' {} a -> s {parameterValue = a} :: RelationalDatabaseParameter)

-- | Specifies the engine-specific parameter type.
relationalDatabaseParameter_applyType :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_applyType = Lens.lens (\RelationalDatabaseParameter' {applyType} -> applyType) (\s@RelationalDatabaseParameter' {} a -> s {applyType = a} :: RelationalDatabaseParameter)

-- | Specifies the name of the parameter.
relationalDatabaseParameter_parameterName :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_parameterName = Lens.lens (\RelationalDatabaseParameter' {parameterName} -> parameterName) (\s@RelationalDatabaseParameter' {} a -> s {parameterName = a} :: RelationalDatabaseParameter)

-- | Provides a description of the parameter.
relationalDatabaseParameter_description :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_description = Lens.lens (\RelationalDatabaseParameter' {description} -> description) (\s@RelationalDatabaseParameter' {} a -> s {description = a} :: RelationalDatabaseParameter)

-- | Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@.
relationalDatabaseParameter_applyMethod :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_applyMethod = Lens.lens (\RelationalDatabaseParameter' {applyMethod} -> applyMethod) (\s@RelationalDatabaseParameter' {} a -> s {applyMethod = a} :: RelationalDatabaseParameter)

-- | A Boolean value indicating whether the parameter can be modified.
relationalDatabaseParameter_isModifiable :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Bool)
relationalDatabaseParameter_isModifiable = Lens.lens (\RelationalDatabaseParameter' {isModifiable} -> isModifiable) (\s@RelationalDatabaseParameter' {} a -> s {isModifiable = a} :: RelationalDatabaseParameter)

-- | Specifies the valid data type for the parameter.
relationalDatabaseParameter_dataType :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_dataType = Lens.lens (\RelationalDatabaseParameter' {dataType} -> dataType) (\s@RelationalDatabaseParameter' {} a -> s {dataType = a} :: RelationalDatabaseParameter)

instance Prelude.FromJSON RelationalDatabaseParameter where
  parseJSON =
    Prelude.withObject
      "RelationalDatabaseParameter"
      ( \x ->
          RelationalDatabaseParameter'
            Prelude.<$> (x Prelude..:? "allowedValues")
            Prelude.<*> (x Prelude..:? "parameterValue")
            Prelude.<*> (x Prelude..:? "applyType")
            Prelude.<*> (x Prelude..:? "parameterName")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "applyMethod")
            Prelude.<*> (x Prelude..:? "isModifiable")
            Prelude.<*> (x Prelude..:? "dataType")
      )

instance Prelude.Hashable RelationalDatabaseParameter

instance Prelude.NFData RelationalDatabaseParameter

instance Prelude.ToJSON RelationalDatabaseParameter where
  toJSON RelationalDatabaseParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("allowedValues" Prelude..=)
              Prelude.<$> allowedValues,
            ("parameterValue" Prelude..=)
              Prelude.<$> parameterValue,
            ("applyType" Prelude..=) Prelude.<$> applyType,
            ("parameterName" Prelude..=)
              Prelude.<$> parameterName,
            ("description" Prelude..=) Prelude.<$> description,
            ("applyMethod" Prelude..=) Prelude.<$> applyMethod,
            ("isModifiable" Prelude..=) Prelude.<$> isModifiable,
            ("dataType" Prelude..=) Prelude.<$> dataType
          ]
      )

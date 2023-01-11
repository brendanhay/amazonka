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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the parameters of a database.
--
-- /See:/ 'newRelationalDatabaseParameter' smart constructor.
data RelationalDatabaseParameter = RelationalDatabaseParameter'
  { -- | Specifies the valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | Indicates when parameter updates are applied.
    --
    -- Can be @immediate@ or @pending-reboot@.
    applyMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies the engine-specific parameter type.
    applyType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the valid data type for the parameter.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Provides a description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the parameter can be modified.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'applyMethod', 'relationalDatabaseParameter_applyMethod' - Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@.
--
-- 'applyType', 'relationalDatabaseParameter_applyType' - Specifies the engine-specific parameter type.
--
-- 'dataType', 'relationalDatabaseParameter_dataType' - Specifies the valid data type for the parameter.
--
-- 'description', 'relationalDatabaseParameter_description' - Provides a description of the parameter.
--
-- 'isModifiable', 'relationalDatabaseParameter_isModifiable' - A Boolean value indicating whether the parameter can be modified.
--
-- 'parameterName', 'relationalDatabaseParameter_parameterName' - Specifies the name of the parameter.
--
-- 'parameterValue', 'relationalDatabaseParameter_parameterValue' - Specifies the value of the parameter.
newRelationalDatabaseParameter ::
  RelationalDatabaseParameter
newRelationalDatabaseParameter =
  RelationalDatabaseParameter'
    { allowedValues =
        Prelude.Nothing,
      applyMethod = Prelude.Nothing,
      applyType = Prelude.Nothing,
      dataType = Prelude.Nothing,
      description = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      parameterValue = Prelude.Nothing
    }

-- | Specifies the valid range of values for the parameter.
relationalDatabaseParameter_allowedValues :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_allowedValues = Lens.lens (\RelationalDatabaseParameter' {allowedValues} -> allowedValues) (\s@RelationalDatabaseParameter' {} a -> s {allowedValues = a} :: RelationalDatabaseParameter)

-- | Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@.
relationalDatabaseParameter_applyMethod :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_applyMethod = Lens.lens (\RelationalDatabaseParameter' {applyMethod} -> applyMethod) (\s@RelationalDatabaseParameter' {} a -> s {applyMethod = a} :: RelationalDatabaseParameter)

-- | Specifies the engine-specific parameter type.
relationalDatabaseParameter_applyType :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_applyType = Lens.lens (\RelationalDatabaseParameter' {applyType} -> applyType) (\s@RelationalDatabaseParameter' {} a -> s {applyType = a} :: RelationalDatabaseParameter)

-- | Specifies the valid data type for the parameter.
relationalDatabaseParameter_dataType :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_dataType = Lens.lens (\RelationalDatabaseParameter' {dataType} -> dataType) (\s@RelationalDatabaseParameter' {} a -> s {dataType = a} :: RelationalDatabaseParameter)

-- | Provides a description of the parameter.
relationalDatabaseParameter_description :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_description = Lens.lens (\RelationalDatabaseParameter' {description} -> description) (\s@RelationalDatabaseParameter' {} a -> s {description = a} :: RelationalDatabaseParameter)

-- | A Boolean value indicating whether the parameter can be modified.
relationalDatabaseParameter_isModifiable :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Bool)
relationalDatabaseParameter_isModifiable = Lens.lens (\RelationalDatabaseParameter' {isModifiable} -> isModifiable) (\s@RelationalDatabaseParameter' {} a -> s {isModifiable = a} :: RelationalDatabaseParameter)

-- | Specifies the name of the parameter.
relationalDatabaseParameter_parameterName :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_parameterName = Lens.lens (\RelationalDatabaseParameter' {parameterName} -> parameterName) (\s@RelationalDatabaseParameter' {} a -> s {parameterName = a} :: RelationalDatabaseParameter)

-- | Specifies the value of the parameter.
relationalDatabaseParameter_parameterValue :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_parameterValue = Lens.lens (\RelationalDatabaseParameter' {parameterValue} -> parameterValue) (\s@RelationalDatabaseParameter' {} a -> s {parameterValue = a} :: RelationalDatabaseParameter)

instance Data.FromJSON RelationalDatabaseParameter where
  parseJSON =
    Data.withObject
      "RelationalDatabaseParameter"
      ( \x ->
          RelationalDatabaseParameter'
            Prelude.<$> (x Data..:? "allowedValues")
            Prelude.<*> (x Data..:? "applyMethod")
            Prelude.<*> (x Data..:? "applyType")
            Prelude.<*> (x Data..:? "dataType")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "isModifiable")
            Prelude.<*> (x Data..:? "parameterName")
            Prelude.<*> (x Data..:? "parameterValue")
      )

instance Prelude.Hashable RelationalDatabaseParameter where
  hashWithSalt _salt RelationalDatabaseParameter' {..} =
    _salt `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` applyMethod
      `Prelude.hashWithSalt` applyType
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isModifiable
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterValue

instance Prelude.NFData RelationalDatabaseParameter where
  rnf RelationalDatabaseParameter' {..} =
    Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf applyMethod
      `Prelude.seq` Prelude.rnf applyType
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isModifiable
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf parameterValue

instance Data.ToJSON RelationalDatabaseParameter where
  toJSON RelationalDatabaseParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowedValues" Data..=) Prelude.<$> allowedValues,
            ("applyMethod" Data..=) Prelude.<$> applyMethod,
            ("applyType" Data..=) Prelude.<$> applyType,
            ("dataType" Data..=) Prelude.<$> dataType,
            ("description" Data..=) Prelude.<$> description,
            ("isModifiable" Data..=) Prelude.<$> isModifiable,
            ("parameterName" Data..=) Prelude.<$> parameterName,
            ("parameterValue" Data..=)
              Prelude.<$> parameterValue
          ]
      )

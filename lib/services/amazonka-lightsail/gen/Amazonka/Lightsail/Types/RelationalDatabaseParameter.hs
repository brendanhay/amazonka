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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the parameters of a database.
--
-- /See:/ 'newRelationalDatabaseParameter' smart constructor.
data RelationalDatabaseParameter = RelationalDatabaseParameter'
  { -- | Specifies the engine-specific parameter type.
    applyType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | Indicates when parameter updates are applied.
    --
    -- Can be @immediate@ or @pending-reboot@.
    applyMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies the valid data type for the parameter.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the parameter can be modified.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | Provides a description of the parameter.
    description :: Prelude.Maybe Prelude.Text
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
-- 'applyType', 'relationalDatabaseParameter_applyType' - Specifies the engine-specific parameter type.
--
-- 'parameterValue', 'relationalDatabaseParameter_parameterValue' - Specifies the value of the parameter.
--
-- 'applyMethod', 'relationalDatabaseParameter_applyMethod' - Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@.
--
-- 'dataType', 'relationalDatabaseParameter_dataType' - Specifies the valid data type for the parameter.
--
-- 'isModifiable', 'relationalDatabaseParameter_isModifiable' - A Boolean value indicating whether the parameter can be modified.
--
-- 'allowedValues', 'relationalDatabaseParameter_allowedValues' - Specifies the valid range of values for the parameter.
--
-- 'parameterName', 'relationalDatabaseParameter_parameterName' - Specifies the name of the parameter.
--
-- 'description', 'relationalDatabaseParameter_description' - Provides a description of the parameter.
newRelationalDatabaseParameter ::
  RelationalDatabaseParameter
newRelationalDatabaseParameter =
  RelationalDatabaseParameter'
    { applyType =
        Prelude.Nothing,
      parameterValue = Prelude.Nothing,
      applyMethod = Prelude.Nothing,
      dataType = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Specifies the engine-specific parameter type.
relationalDatabaseParameter_applyType :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_applyType = Lens.lens (\RelationalDatabaseParameter' {applyType} -> applyType) (\s@RelationalDatabaseParameter' {} a -> s {applyType = a} :: RelationalDatabaseParameter)

-- | Specifies the value of the parameter.
relationalDatabaseParameter_parameterValue :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_parameterValue = Lens.lens (\RelationalDatabaseParameter' {parameterValue} -> parameterValue) (\s@RelationalDatabaseParameter' {} a -> s {parameterValue = a} :: RelationalDatabaseParameter)

-- | Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@.
relationalDatabaseParameter_applyMethod :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_applyMethod = Lens.lens (\RelationalDatabaseParameter' {applyMethod} -> applyMethod) (\s@RelationalDatabaseParameter' {} a -> s {applyMethod = a} :: RelationalDatabaseParameter)

-- | Specifies the valid data type for the parameter.
relationalDatabaseParameter_dataType :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_dataType = Lens.lens (\RelationalDatabaseParameter' {dataType} -> dataType) (\s@RelationalDatabaseParameter' {} a -> s {dataType = a} :: RelationalDatabaseParameter)

-- | A Boolean value indicating whether the parameter can be modified.
relationalDatabaseParameter_isModifiable :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Bool)
relationalDatabaseParameter_isModifiable = Lens.lens (\RelationalDatabaseParameter' {isModifiable} -> isModifiable) (\s@RelationalDatabaseParameter' {} a -> s {isModifiable = a} :: RelationalDatabaseParameter)

-- | Specifies the valid range of values for the parameter.
relationalDatabaseParameter_allowedValues :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_allowedValues = Lens.lens (\RelationalDatabaseParameter' {allowedValues} -> allowedValues) (\s@RelationalDatabaseParameter' {} a -> s {allowedValues = a} :: RelationalDatabaseParameter)

-- | Specifies the name of the parameter.
relationalDatabaseParameter_parameterName :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_parameterName = Lens.lens (\RelationalDatabaseParameter' {parameterName} -> parameterName) (\s@RelationalDatabaseParameter' {} a -> s {parameterName = a} :: RelationalDatabaseParameter)

-- | Provides a description of the parameter.
relationalDatabaseParameter_description :: Lens.Lens' RelationalDatabaseParameter (Prelude.Maybe Prelude.Text)
relationalDatabaseParameter_description = Lens.lens (\RelationalDatabaseParameter' {description} -> description) (\s@RelationalDatabaseParameter' {} a -> s {description = a} :: RelationalDatabaseParameter)

instance Core.FromJSON RelationalDatabaseParameter where
  parseJSON =
    Core.withObject
      "RelationalDatabaseParameter"
      ( \x ->
          RelationalDatabaseParameter'
            Prelude.<$> (x Core..:? "applyType")
            Prelude.<*> (x Core..:? "parameterValue")
            Prelude.<*> (x Core..:? "applyMethod")
            Prelude.<*> (x Core..:? "dataType")
            Prelude.<*> (x Core..:? "isModifiable")
            Prelude.<*> (x Core..:? "allowedValues")
            Prelude.<*> (x Core..:? "parameterName")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable RelationalDatabaseParameter where
  hashWithSalt salt' RelationalDatabaseParameter' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` isModifiable
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` applyMethod
      `Prelude.hashWithSalt` parameterValue
      `Prelude.hashWithSalt` applyType

instance Prelude.NFData RelationalDatabaseParameter where
  rnf RelationalDatabaseParameter' {..} =
    Prelude.rnf applyType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf isModifiable
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf applyMethod
      `Prelude.seq` Prelude.rnf parameterValue

instance Core.ToJSON RelationalDatabaseParameter where
  toJSON RelationalDatabaseParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("applyType" Core..=) Prelude.<$> applyType,
            ("parameterValue" Core..=)
              Prelude.<$> parameterValue,
            ("applyMethod" Core..=) Prelude.<$> applyMethod,
            ("dataType" Core..=) Prelude.<$> dataType,
            ("isModifiable" Core..=) Prelude.<$> isModifiable,
            ("allowedValues" Core..=) Prelude.<$> allowedValues,
            ("parameterName" Core..=) Prelude.<$> parameterName,
            ("description" Core..=) Prelude.<$> description
          ]
      )

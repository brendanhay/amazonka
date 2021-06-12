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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the parameters of a database.
--
-- /See:/ 'newRelationalDatabaseParameter' smart constructor.
data RelationalDatabaseParameter = RelationalDatabaseParameter'
  { -- | Specifies the valid range of values for the parameter.
    allowedValues :: Core.Maybe Core.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Core.Maybe Core.Text,
    -- | Specifies the engine-specific parameter type.
    applyType :: Core.Maybe Core.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Core.Maybe Core.Text,
    -- | Provides a description of the parameter.
    description :: Core.Maybe Core.Text,
    -- | Indicates when parameter updates are applied.
    --
    -- Can be @immediate@ or @pending-reboot@.
    applyMethod :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether the parameter can be modified.
    isModifiable :: Core.Maybe Core.Bool,
    -- | Specifies the valid data type for the parameter.
    dataType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      parameterValue = Core.Nothing,
      applyType = Core.Nothing,
      parameterName = Core.Nothing,
      description = Core.Nothing,
      applyMethod = Core.Nothing,
      isModifiable = Core.Nothing,
      dataType = Core.Nothing
    }

-- | Specifies the valid range of values for the parameter.
relationalDatabaseParameter_allowedValues :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Text)
relationalDatabaseParameter_allowedValues = Lens.lens (\RelationalDatabaseParameter' {allowedValues} -> allowedValues) (\s@RelationalDatabaseParameter' {} a -> s {allowedValues = a} :: RelationalDatabaseParameter)

-- | Specifies the value of the parameter.
relationalDatabaseParameter_parameterValue :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Text)
relationalDatabaseParameter_parameterValue = Lens.lens (\RelationalDatabaseParameter' {parameterValue} -> parameterValue) (\s@RelationalDatabaseParameter' {} a -> s {parameterValue = a} :: RelationalDatabaseParameter)

-- | Specifies the engine-specific parameter type.
relationalDatabaseParameter_applyType :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Text)
relationalDatabaseParameter_applyType = Lens.lens (\RelationalDatabaseParameter' {applyType} -> applyType) (\s@RelationalDatabaseParameter' {} a -> s {applyType = a} :: RelationalDatabaseParameter)

-- | Specifies the name of the parameter.
relationalDatabaseParameter_parameterName :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Text)
relationalDatabaseParameter_parameterName = Lens.lens (\RelationalDatabaseParameter' {parameterName} -> parameterName) (\s@RelationalDatabaseParameter' {} a -> s {parameterName = a} :: RelationalDatabaseParameter)

-- | Provides a description of the parameter.
relationalDatabaseParameter_description :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Text)
relationalDatabaseParameter_description = Lens.lens (\RelationalDatabaseParameter' {description} -> description) (\s@RelationalDatabaseParameter' {} a -> s {description = a} :: RelationalDatabaseParameter)

-- | Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@.
relationalDatabaseParameter_applyMethod :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Text)
relationalDatabaseParameter_applyMethod = Lens.lens (\RelationalDatabaseParameter' {applyMethod} -> applyMethod) (\s@RelationalDatabaseParameter' {} a -> s {applyMethod = a} :: RelationalDatabaseParameter)

-- | A Boolean value indicating whether the parameter can be modified.
relationalDatabaseParameter_isModifiable :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Bool)
relationalDatabaseParameter_isModifiable = Lens.lens (\RelationalDatabaseParameter' {isModifiable} -> isModifiable) (\s@RelationalDatabaseParameter' {} a -> s {isModifiable = a} :: RelationalDatabaseParameter)

-- | Specifies the valid data type for the parameter.
relationalDatabaseParameter_dataType :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Text)
relationalDatabaseParameter_dataType = Lens.lens (\RelationalDatabaseParameter' {dataType} -> dataType) (\s@RelationalDatabaseParameter' {} a -> s {dataType = a} :: RelationalDatabaseParameter)

instance Core.FromJSON RelationalDatabaseParameter where
  parseJSON =
    Core.withObject
      "RelationalDatabaseParameter"
      ( \x ->
          RelationalDatabaseParameter'
            Core.<$> (x Core..:? "allowedValues")
            Core.<*> (x Core..:? "parameterValue")
            Core.<*> (x Core..:? "applyType")
            Core.<*> (x Core..:? "parameterName")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "applyMethod")
            Core.<*> (x Core..:? "isModifiable")
            Core.<*> (x Core..:? "dataType")
      )

instance Core.Hashable RelationalDatabaseParameter

instance Core.NFData RelationalDatabaseParameter

instance Core.ToJSON RelationalDatabaseParameter where
  toJSON RelationalDatabaseParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("allowedValues" Core..=) Core.<$> allowedValues,
            ("parameterValue" Core..=) Core.<$> parameterValue,
            ("applyType" Core..=) Core.<$> applyType,
            ("parameterName" Core..=) Core.<$> parameterName,
            ("description" Core..=) Core.<$> description,
            ("applyMethod" Core..=) Core.<$> applyMethod,
            ("isModifiable" Core..=) Core.<$> isModifiable,
            ("dataType" Core..=) Core.<$> dataType
          ]
      )

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
-- Module      : Amazonka.MemoryDb.Types.Parameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Parameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an individual setting that controls some aspect of MemoryDB
-- behavior.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The name of the parameter
    name :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter
    description :: Prelude.Maybe Prelude.Text,
    -- | The earliest engine version to which the parameter can apply.
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The parameter\'s data type
    dataType :: Prelude.Maybe Prelude.Text,
    -- | The value of the parameter
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'parameter_name' - The name of the parameter
--
-- 'description', 'parameter_description' - A description of the parameter
--
-- 'minimumEngineVersion', 'parameter_minimumEngineVersion' - The earliest engine version to which the parameter can apply.
--
-- 'allowedValues', 'parameter_allowedValues' - The valid range of values for the parameter.
--
-- 'dataType', 'parameter_dataType' - The parameter\'s data type
--
-- 'value', 'parameter_value' - The value of the parameter
newParameter ::
  Parameter
newParameter =
  Parameter'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      dataType = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the parameter
parameter_name :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_name = Lens.lens (\Parameter' {name} -> name) (\s@Parameter' {} a -> s {name = a} :: Parameter)

-- | A description of the parameter
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The earliest engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

-- | The valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The parameter\'s data type
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | The value of the parameter
parameter_value :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_value = Lens.lens (\Parameter' {value} -> value) (\s@Parameter' {} a -> s {value = a} :: Parameter)

instance Core.FromJSON Parameter where
  parseJSON =
    Core.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "MinimumEngineVersion")
            Prelude.<*> (x Core..:? "AllowedValues")
            Prelude.<*> (x Core..:? "DataType")
            Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable Parameter where
  hashWithSalt _salt Parameter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` minimumEngineVersion
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` value

instance Prelude.NFData Parameter where
  rnf Parameter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf minimumEngineVersion
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf value

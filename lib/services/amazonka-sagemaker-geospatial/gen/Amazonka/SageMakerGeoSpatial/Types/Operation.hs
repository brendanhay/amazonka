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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.Operation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.Operation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.OutputType

-- |
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | The type of the operation.
    outputType :: Prelude.Maybe OutputType,
    equation :: Prelude.Text,
    -- | The name of the operation.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Operation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputType', 'operation_outputType' - The type of the operation.
--
-- 'equation', 'operation_equation' -
--
-- 'name', 'operation_name' - The name of the operation.
newOperation ::
  -- | 'equation'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Operation
newOperation pEquation_ pName_ =
  Operation'
    { outputType = Prelude.Nothing,
      equation = pEquation_,
      name = pName_
    }

-- | The type of the operation.
operation_outputType :: Lens.Lens' Operation (Prelude.Maybe OutputType)
operation_outputType = Lens.lens (\Operation' {outputType} -> outputType) (\s@Operation' {} a -> s {outputType = a} :: Operation)

-- |
operation_equation :: Lens.Lens' Operation Prelude.Text
operation_equation = Lens.lens (\Operation' {equation} -> equation) (\s@Operation' {} a -> s {equation = a} :: Operation)

-- | The name of the operation.
operation_name :: Lens.Lens' Operation Prelude.Text
operation_name = Lens.lens (\Operation' {name} -> name) (\s@Operation' {} a -> s {name = a} :: Operation)

instance Data.FromJSON Operation where
  parseJSON =
    Data.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Data..:? "OutputType")
            Prelude.<*> (x Data..: "Equation")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Operation where
  hashWithSalt _salt Operation' {..} =
    _salt `Prelude.hashWithSalt` outputType
      `Prelude.hashWithSalt` equation
      `Prelude.hashWithSalt` name

instance Prelude.NFData Operation where
  rnf Operation' {..} =
    Prelude.rnf outputType
      `Prelude.seq` Prelude.rnf equation
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON Operation where
  toJSON Operation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OutputType" Data..=) Prelude.<$> outputType,
            Prelude.Just ("Equation" Data..= equation),
            Prelude.Just ("Name" Data..= name)
          ]
      )

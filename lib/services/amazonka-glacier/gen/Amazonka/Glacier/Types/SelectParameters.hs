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
-- Module      : Amazonka.Glacier.Types.SelectParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.SelectParameters where

import qualified Amazonka.Core as Core
import Amazonka.Glacier.Types.ExpressionType
import Amazonka.Glacier.Types.InputSerialization
import Amazonka.Glacier.Types.OutputSerialization
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the parameters used for a select.
--
-- /See:/ 'newSelectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { -- | The type of the provided expression, for example @SQL@.
    expressionType :: Prelude.Maybe ExpressionType,
    -- | Describes how the results of the select job are serialized.
    outputSerialization :: Prelude.Maybe OutputSerialization,
    -- | The expression that is used to select the object.
    expression :: Prelude.Maybe Prelude.Text,
    -- | Describes the serialization format of the object.
    inputSerialization :: Prelude.Maybe InputSerialization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expressionType', 'selectParameters_expressionType' - The type of the provided expression, for example @SQL@.
--
-- 'outputSerialization', 'selectParameters_outputSerialization' - Describes how the results of the select job are serialized.
--
-- 'expression', 'selectParameters_expression' - The expression that is used to select the object.
--
-- 'inputSerialization', 'selectParameters_inputSerialization' - Describes the serialization format of the object.
newSelectParameters ::
  SelectParameters
newSelectParameters =
  SelectParameters'
    { expressionType = Prelude.Nothing,
      outputSerialization = Prelude.Nothing,
      expression = Prelude.Nothing,
      inputSerialization = Prelude.Nothing
    }

-- | The type of the provided expression, for example @SQL@.
selectParameters_expressionType :: Lens.Lens' SelectParameters (Prelude.Maybe ExpressionType)
selectParameters_expressionType = Lens.lens (\SelectParameters' {expressionType} -> expressionType) (\s@SelectParameters' {} a -> s {expressionType = a} :: SelectParameters)

-- | Describes how the results of the select job are serialized.
selectParameters_outputSerialization :: Lens.Lens' SelectParameters (Prelude.Maybe OutputSerialization)
selectParameters_outputSerialization = Lens.lens (\SelectParameters' {outputSerialization} -> outputSerialization) (\s@SelectParameters' {} a -> s {outputSerialization = a} :: SelectParameters)

-- | The expression that is used to select the object.
selectParameters_expression :: Lens.Lens' SelectParameters (Prelude.Maybe Prelude.Text)
selectParameters_expression = Lens.lens (\SelectParameters' {expression} -> expression) (\s@SelectParameters' {} a -> s {expression = a} :: SelectParameters)

-- | Describes the serialization format of the object.
selectParameters_inputSerialization :: Lens.Lens' SelectParameters (Prelude.Maybe InputSerialization)
selectParameters_inputSerialization = Lens.lens (\SelectParameters' {inputSerialization} -> inputSerialization) (\s@SelectParameters' {} a -> s {inputSerialization = a} :: SelectParameters)

instance Core.FromJSON SelectParameters where
  parseJSON =
    Core.withObject
      "SelectParameters"
      ( \x ->
          SelectParameters'
            Prelude.<$> (x Core..:? "ExpressionType")
            Prelude.<*> (x Core..:? "OutputSerialization")
            Prelude.<*> (x Core..:? "Expression")
            Prelude.<*> (x Core..:? "InputSerialization")
      )

instance Prelude.Hashable SelectParameters where
  hashWithSalt _salt SelectParameters' {..} =
    _salt `Prelude.hashWithSalt` expressionType
      `Prelude.hashWithSalt` outputSerialization
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` inputSerialization

instance Prelude.NFData SelectParameters where
  rnf SelectParameters' {..} =
    Prelude.rnf expressionType
      `Prelude.seq` Prelude.rnf outputSerialization
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf inputSerialization

instance Core.ToJSON SelectParameters where
  toJSON SelectParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExpressionType" Core..=)
              Prelude.<$> expressionType,
            ("OutputSerialization" Core..=)
              Prelude.<$> outputSerialization,
            ("Expression" Core..=) Prelude.<$> expression,
            ("InputSerialization" Core..=)
              Prelude.<$> inputSerialization
          ]
      )

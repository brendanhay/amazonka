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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.SelectParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.ExpressionType
import Amazonka.Glacier.Types.InputSerialization
import Amazonka.Glacier.Types.OutputSerialization
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the parameters used for a select.
--
-- /See:/ 'newSelectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { -- | The expression that is used to select the object.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The type of the provided expression, for example @SQL@.
    expressionType :: Prelude.Maybe ExpressionType,
    -- | Describes the serialization format of the object.
    inputSerialization :: Prelude.Maybe InputSerialization,
    -- | Describes how the results of the select job are serialized.
    outputSerialization :: Prelude.Maybe OutputSerialization
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
-- 'expression', 'selectParameters_expression' - The expression that is used to select the object.
--
-- 'expressionType', 'selectParameters_expressionType' - The type of the provided expression, for example @SQL@.
--
-- 'inputSerialization', 'selectParameters_inputSerialization' - Describes the serialization format of the object.
--
-- 'outputSerialization', 'selectParameters_outputSerialization' - Describes how the results of the select job are serialized.
newSelectParameters ::
  SelectParameters
newSelectParameters =
  SelectParameters'
    { expression = Prelude.Nothing,
      expressionType = Prelude.Nothing,
      inputSerialization = Prelude.Nothing,
      outputSerialization = Prelude.Nothing
    }

-- | The expression that is used to select the object.
selectParameters_expression :: Lens.Lens' SelectParameters (Prelude.Maybe Prelude.Text)
selectParameters_expression = Lens.lens (\SelectParameters' {expression} -> expression) (\s@SelectParameters' {} a -> s {expression = a} :: SelectParameters)

-- | The type of the provided expression, for example @SQL@.
selectParameters_expressionType :: Lens.Lens' SelectParameters (Prelude.Maybe ExpressionType)
selectParameters_expressionType = Lens.lens (\SelectParameters' {expressionType} -> expressionType) (\s@SelectParameters' {} a -> s {expressionType = a} :: SelectParameters)

-- | Describes the serialization format of the object.
selectParameters_inputSerialization :: Lens.Lens' SelectParameters (Prelude.Maybe InputSerialization)
selectParameters_inputSerialization = Lens.lens (\SelectParameters' {inputSerialization} -> inputSerialization) (\s@SelectParameters' {} a -> s {inputSerialization = a} :: SelectParameters)

-- | Describes how the results of the select job are serialized.
selectParameters_outputSerialization :: Lens.Lens' SelectParameters (Prelude.Maybe OutputSerialization)
selectParameters_outputSerialization = Lens.lens (\SelectParameters' {outputSerialization} -> outputSerialization) (\s@SelectParameters' {} a -> s {outputSerialization = a} :: SelectParameters)

instance Data.FromJSON SelectParameters where
  parseJSON =
    Data.withObject
      "SelectParameters"
      ( \x ->
          SelectParameters'
            Prelude.<$> (x Data..:? "Expression")
            Prelude.<*> (x Data..:? "ExpressionType")
            Prelude.<*> (x Data..:? "InputSerialization")
            Prelude.<*> (x Data..:? "OutputSerialization")
      )

instance Prelude.Hashable SelectParameters where
  hashWithSalt _salt SelectParameters' {..} =
    _salt
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` expressionType
      `Prelude.hashWithSalt` inputSerialization
      `Prelude.hashWithSalt` outputSerialization

instance Prelude.NFData SelectParameters where
  rnf SelectParameters' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf expressionType
      `Prelude.seq` Prelude.rnf inputSerialization
      `Prelude.seq` Prelude.rnf outputSerialization

instance Data.ToJSON SelectParameters where
  toJSON SelectParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Expression" Data..=) Prelude.<$> expression,
            ("ExpressionType" Data..=)
              Prelude.<$> expressionType,
            ("InputSerialization" Data..=)
              Prelude.<$> inputSerialization,
            ("OutputSerialization" Data..=)
              Prelude.<$> outputSerialization
          ]
      )

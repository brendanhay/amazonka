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
-- Module      : Network.AWS.Glacier.Types.SelectParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.SelectParameters where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.ExpressionType
import Network.AWS.Glacier.Types.InputSerialization
import Network.AWS.Glacier.Types.OutputSerialization
import qualified Network.AWS.Lens as Lens

-- | Contains information about the parameters used for a select.
--
-- /See:/ 'newSelectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { -- | The type of the provided expression, for example @SQL@.
    expressionType :: Core.Maybe ExpressionType,
    -- | Describes how the results of the select job are serialized.
    outputSerialization :: Core.Maybe OutputSerialization,
    -- | Describes the serialization format of the object.
    inputSerialization :: Core.Maybe InputSerialization,
    -- | The expression that is used to select the object.
    expression :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'inputSerialization', 'selectParameters_inputSerialization' - Describes the serialization format of the object.
--
-- 'expression', 'selectParameters_expression' - The expression that is used to select the object.
newSelectParameters ::
  SelectParameters
newSelectParameters =
  SelectParameters'
    { expressionType = Core.Nothing,
      outputSerialization = Core.Nothing,
      inputSerialization = Core.Nothing,
      expression = Core.Nothing
    }

-- | The type of the provided expression, for example @SQL@.
selectParameters_expressionType :: Lens.Lens' SelectParameters (Core.Maybe ExpressionType)
selectParameters_expressionType = Lens.lens (\SelectParameters' {expressionType} -> expressionType) (\s@SelectParameters' {} a -> s {expressionType = a} :: SelectParameters)

-- | Describes how the results of the select job are serialized.
selectParameters_outputSerialization :: Lens.Lens' SelectParameters (Core.Maybe OutputSerialization)
selectParameters_outputSerialization = Lens.lens (\SelectParameters' {outputSerialization} -> outputSerialization) (\s@SelectParameters' {} a -> s {outputSerialization = a} :: SelectParameters)

-- | Describes the serialization format of the object.
selectParameters_inputSerialization :: Lens.Lens' SelectParameters (Core.Maybe InputSerialization)
selectParameters_inputSerialization = Lens.lens (\SelectParameters' {inputSerialization} -> inputSerialization) (\s@SelectParameters' {} a -> s {inputSerialization = a} :: SelectParameters)

-- | The expression that is used to select the object.
selectParameters_expression :: Lens.Lens' SelectParameters (Core.Maybe Core.Text)
selectParameters_expression = Lens.lens (\SelectParameters' {expression} -> expression) (\s@SelectParameters' {} a -> s {expression = a} :: SelectParameters)

instance Core.FromJSON SelectParameters where
  parseJSON =
    Core.withObject
      "SelectParameters"
      ( \x ->
          SelectParameters'
            Core.<$> (x Core..:? "ExpressionType")
            Core.<*> (x Core..:? "OutputSerialization")
            Core.<*> (x Core..:? "InputSerialization")
            Core.<*> (x Core..:? "Expression")
      )

instance Core.Hashable SelectParameters

instance Core.NFData SelectParameters

instance Core.ToJSON SelectParameters where
  toJSON SelectParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExpressionType" Core..=) Core.<$> expressionType,
            ("OutputSerialization" Core..=)
              Core.<$> outputSerialization,
            ("InputSerialization" Core..=)
              Core.<$> inputSerialization,
            ("Expression" Core..=) Core.<$> expression
          ]
      )

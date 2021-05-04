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
-- Module      : Network.AWS.S3.Types.SelectParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SelectParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ExpressionType
import Network.AWS.S3.Types.InputSerialization
import Network.AWS.S3.Types.OutputSerialization

-- | Describes the parameters for Select job types.
--
-- /See:/ 'newSelectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { -- | Describes the serialization format of the object.
    inputSerialization :: InputSerialization,
    -- | The type of the provided expression (for example, SQL).
    expressionType :: ExpressionType,
    -- | The expression that is used to query the object.
    expression :: Prelude.Text,
    -- | Describes how the results of the Select job are serialized.
    outputSerialization :: OutputSerialization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SelectParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSerialization', 'selectParameters_inputSerialization' - Describes the serialization format of the object.
--
-- 'expressionType', 'selectParameters_expressionType' - The type of the provided expression (for example, SQL).
--
-- 'expression', 'selectParameters_expression' - The expression that is used to query the object.
--
-- 'outputSerialization', 'selectParameters_outputSerialization' - Describes how the results of the Select job are serialized.
newSelectParameters ::
  -- | 'inputSerialization'
  InputSerialization ->
  -- | 'expressionType'
  ExpressionType ->
  -- | 'expression'
  Prelude.Text ->
  -- | 'outputSerialization'
  OutputSerialization ->
  SelectParameters
newSelectParameters
  pInputSerialization_
  pExpressionType_
  pExpression_
  pOutputSerialization_ =
    SelectParameters'
      { inputSerialization =
          pInputSerialization_,
        expressionType = pExpressionType_,
        expression = pExpression_,
        outputSerialization = pOutputSerialization_
      }

-- | Describes the serialization format of the object.
selectParameters_inputSerialization :: Lens.Lens' SelectParameters InputSerialization
selectParameters_inputSerialization = Lens.lens (\SelectParameters' {inputSerialization} -> inputSerialization) (\s@SelectParameters' {} a -> s {inputSerialization = a} :: SelectParameters)

-- | The type of the provided expression (for example, SQL).
selectParameters_expressionType :: Lens.Lens' SelectParameters ExpressionType
selectParameters_expressionType = Lens.lens (\SelectParameters' {expressionType} -> expressionType) (\s@SelectParameters' {} a -> s {expressionType = a} :: SelectParameters)

-- | The expression that is used to query the object.
selectParameters_expression :: Lens.Lens' SelectParameters Prelude.Text
selectParameters_expression = Lens.lens (\SelectParameters' {expression} -> expression) (\s@SelectParameters' {} a -> s {expression = a} :: SelectParameters)

-- | Describes how the results of the Select job are serialized.
selectParameters_outputSerialization :: Lens.Lens' SelectParameters OutputSerialization
selectParameters_outputSerialization = Lens.lens (\SelectParameters' {outputSerialization} -> outputSerialization) (\s@SelectParameters' {} a -> s {outputSerialization = a} :: SelectParameters)

instance Prelude.Hashable SelectParameters

instance Prelude.NFData SelectParameters

instance Prelude.ToXML SelectParameters where
  toXML SelectParameters' {..} =
    Prelude.mconcat
      [ "InputSerialization" Prelude.@= inputSerialization,
        "ExpressionType" Prelude.@= expressionType,
        "Expression" Prelude.@= expression,
        "OutputSerialization" Prelude.@= outputSerialization
      ]

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SelectParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SelectParameters
  ( SelectParameters (..),

    -- * Smart constructor
    mkSelectParameters,

    -- * Lenses
    spExpressionType,
    spOutputSerialization,
    spExpression,
    spInputSerialization,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ExpressionType
import Network.AWS.S3.Types.InputSerialization
import Network.AWS.S3.Types.OutputSerialization

-- | Describes the parameters for Select job types.
--
-- /See:/ 'mkSelectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { -- | The type of the provided expression (for example, SQL).
    expressionType :: ExpressionType,
    -- | Describes how the results of the Select job are serialized.
    outputSerialization :: OutputSerialization,
    -- | The expression that is used to query the object.
    expression :: Lude.Text,
    -- | Describes the serialization format of the object.
    inputSerialization :: InputSerialization
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectParameters' with the minimum fields required to make a request.
--
-- * 'expressionType' - The type of the provided expression (for example, SQL).
-- * 'outputSerialization' - Describes how the results of the Select job are serialized.
-- * 'expression' - The expression that is used to query the object.
-- * 'inputSerialization' - Describes the serialization format of the object.
mkSelectParameters ::
  -- | 'expressionType'
  ExpressionType ->
  -- | 'outputSerialization'
  OutputSerialization ->
  -- | 'expression'
  Lude.Text ->
  -- | 'inputSerialization'
  InputSerialization ->
  SelectParameters
mkSelectParameters
  pExpressionType_
  pOutputSerialization_
  pExpression_
  pInputSerialization_ =
    SelectParameters'
      { expressionType = pExpressionType_,
        outputSerialization = pOutputSerialization_,
        expression = pExpression_,
        inputSerialization = pInputSerialization_
      }

-- | The type of the provided expression (for example, SQL).
--
-- /Note:/ Consider using 'expressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spExpressionType :: Lens.Lens' SelectParameters ExpressionType
spExpressionType = Lens.lens (expressionType :: SelectParameters -> ExpressionType) (\s a -> s {expressionType = a} :: SelectParameters)
{-# DEPRECATED spExpressionType "Use generic-lens or generic-optics with 'expressionType' instead." #-}

-- | Describes how the results of the Select job are serialized.
--
-- /Note:/ Consider using 'outputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spOutputSerialization :: Lens.Lens' SelectParameters OutputSerialization
spOutputSerialization = Lens.lens (outputSerialization :: SelectParameters -> OutputSerialization) (\s a -> s {outputSerialization = a} :: SelectParameters)
{-# DEPRECATED spOutputSerialization "Use generic-lens or generic-optics with 'outputSerialization' instead." #-}

-- | The expression that is used to query the object.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spExpression :: Lens.Lens' SelectParameters Lude.Text
spExpression = Lens.lens (expression :: SelectParameters -> Lude.Text) (\s a -> s {expression = a} :: SelectParameters)
{-# DEPRECATED spExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | Describes the serialization format of the object.
--
-- /Note:/ Consider using 'inputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spInputSerialization :: Lens.Lens' SelectParameters InputSerialization
spInputSerialization = Lens.lens (inputSerialization :: SelectParameters -> InputSerialization) (\s a -> s {inputSerialization = a} :: SelectParameters)
{-# DEPRECATED spInputSerialization "Use generic-lens or generic-optics with 'inputSerialization' instead." #-}

instance Lude.ToXML SelectParameters where
  toXML SelectParameters' {..} =
    Lude.mconcat
      [ "ExpressionType" Lude.@= expressionType,
        "OutputSerialization" Lude.@= outputSerialization,
        "Expression" Lude.@= expression,
        "InputSerialization" Lude.@= inputSerialization
      ]

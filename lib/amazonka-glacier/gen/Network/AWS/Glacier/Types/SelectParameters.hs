-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.SelectParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.SelectParameters
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

import Network.AWS.Glacier.Types.ExpressionType
import Network.AWS.Glacier.Types.InputSerialization
import Network.AWS.Glacier.Types.OutputSerialization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the parameters used for a select.
--
-- /See:/ 'mkSelectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { expressionType ::
      Lude.Maybe ExpressionType,
    outputSerialization :: Lude.Maybe OutputSerialization,
    expression :: Lude.Maybe Lude.Text,
    inputSerialization :: Lude.Maybe InputSerialization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectParameters' with the minimum fields required to make a request.
--
-- * 'expression' - The expression that is used to select the object.
-- * 'expressionType' - The type of the provided expression, for example @SQL@ .
-- * 'inputSerialization' - Describes the serialization format of the object.
-- * 'outputSerialization' - Describes how the results of the select job are serialized.
mkSelectParameters ::
  SelectParameters
mkSelectParameters =
  SelectParameters'
    { expressionType = Lude.Nothing,
      outputSerialization = Lude.Nothing,
      expression = Lude.Nothing,
      inputSerialization = Lude.Nothing
    }

-- | The type of the provided expression, for example @SQL@ .
--
-- /Note:/ Consider using 'expressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spExpressionType :: Lens.Lens' SelectParameters (Lude.Maybe ExpressionType)
spExpressionType = Lens.lens (expressionType :: SelectParameters -> Lude.Maybe ExpressionType) (\s a -> s {expressionType = a} :: SelectParameters)
{-# DEPRECATED spExpressionType "Use generic-lens or generic-optics with 'expressionType' instead." #-}

-- | Describes how the results of the select job are serialized.
--
-- /Note:/ Consider using 'outputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spOutputSerialization :: Lens.Lens' SelectParameters (Lude.Maybe OutputSerialization)
spOutputSerialization = Lens.lens (outputSerialization :: SelectParameters -> Lude.Maybe OutputSerialization) (\s a -> s {outputSerialization = a} :: SelectParameters)
{-# DEPRECATED spOutputSerialization "Use generic-lens or generic-optics with 'outputSerialization' instead." #-}

-- | The expression that is used to select the object.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spExpression :: Lens.Lens' SelectParameters (Lude.Maybe Lude.Text)
spExpression = Lens.lens (expression :: SelectParameters -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: SelectParameters)
{-# DEPRECATED spExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | Describes the serialization format of the object.
--
-- /Note:/ Consider using 'inputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spInputSerialization :: Lens.Lens' SelectParameters (Lude.Maybe InputSerialization)
spInputSerialization = Lens.lens (inputSerialization :: SelectParameters -> Lude.Maybe InputSerialization) (\s a -> s {inputSerialization = a} :: SelectParameters)
{-# DEPRECATED spInputSerialization "Use generic-lens or generic-optics with 'inputSerialization' instead." #-}

instance Lude.FromJSON SelectParameters where
  parseJSON =
    Lude.withObject
      "SelectParameters"
      ( \x ->
          SelectParameters'
            Lude.<$> (x Lude..:? "ExpressionType")
            Lude.<*> (x Lude..:? "OutputSerialization")
            Lude.<*> (x Lude..:? "Expression")
            Lude.<*> (x Lude..:? "InputSerialization")
      )

instance Lude.ToJSON SelectParameters where
  toJSON SelectParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpressionType" Lude..=) Lude.<$> expressionType,
            ("OutputSerialization" Lude..=) Lude.<$> outputSerialization,
            ("Expression" Lude..=) Lude.<$> expression,
            ("InputSerialization" Lude..=) Lude.<$> inputSerialization
          ]
      )

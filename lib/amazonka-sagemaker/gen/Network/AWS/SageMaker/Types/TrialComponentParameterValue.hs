-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentParameterValue
  ( TrialComponentParameterValue (..),

    -- * Smart constructor
    mkTrialComponentParameterValue,

    -- * Lenses
    tcpvNumberValue,
    tcpvStringValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The value of a hyperparameter. Only one of @NumberValue@ or @StringValue@ can be specified.
--
-- This object is specified in the 'CreateTrialComponent' request.
--
-- /See:/ 'mkTrialComponentParameterValue' smart constructor.
data TrialComponentParameterValue = TrialComponentParameterValue'
  { numberValue ::
      Lude.Maybe Lude.Double,
    stringValue ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponentParameterValue' with the minimum fields required to make a request.
--
-- * 'numberValue' - The numeric value of a numeric hyperparameter. If you specify a value for this parameter, you can't specify the @StringValue@ parameter.
-- * 'stringValue' - The string value of a categorical hyperparameter. If you specify a value for this parameter, you can't specify the @NumberValue@ parameter.
mkTrialComponentParameterValue ::
  TrialComponentParameterValue
mkTrialComponentParameterValue =
  TrialComponentParameterValue'
    { numberValue = Lude.Nothing,
      stringValue = Lude.Nothing
    }

-- | The numeric value of a numeric hyperparameter. If you specify a value for this parameter, you can't specify the @StringValue@ parameter.
--
-- /Note:/ Consider using 'numberValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcpvNumberValue :: Lens.Lens' TrialComponentParameterValue (Lude.Maybe Lude.Double)
tcpvNumberValue = Lens.lens (numberValue :: TrialComponentParameterValue -> Lude.Maybe Lude.Double) (\s a -> s {numberValue = a} :: TrialComponentParameterValue)
{-# DEPRECATED tcpvNumberValue "Use generic-lens or generic-optics with 'numberValue' instead." #-}

-- | The string value of a categorical hyperparameter. If you specify a value for this parameter, you can't specify the @NumberValue@ parameter.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcpvStringValue :: Lens.Lens' TrialComponentParameterValue (Lude.Maybe Lude.Text)
tcpvStringValue = Lens.lens (stringValue :: TrialComponentParameterValue -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: TrialComponentParameterValue)
{-# DEPRECATED tcpvStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

instance Lude.FromJSON TrialComponentParameterValue where
  parseJSON =
    Lude.withObject
      "TrialComponentParameterValue"
      ( \x ->
          TrialComponentParameterValue'
            Lude.<$> (x Lude..:? "NumberValue") Lude.<*> (x Lude..:? "StringValue")
      )

instance Lude.ToJSON TrialComponentParameterValue where
  toJSON TrialComponentParameterValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberValue" Lude..=) Lude.<$> numberValue,
            ("StringValue" Lude..=) Lude.<$> stringValue
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.HumanLoopActivationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopActivationOutput
  ( HumanLoopActivationOutput (..),

    -- * Smart constructor
    mkHumanLoopActivationOutput,

    -- * Lenses
    hlaoHumanLoopActivationReasons,
    hlaoHumanLoopARN,
    hlaoHumanLoopActivationConditionsEvaluationResults,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Shows the results of the human in the loop evaluation. If there is no HumanLoopArn, the input did not trigger human review.
--
-- /See:/ 'mkHumanLoopActivationOutput' smart constructor.
data HumanLoopActivationOutput = HumanLoopActivationOutput'
  { humanLoopActivationReasons ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    humanLoopARN :: Lude.Maybe Lude.Text,
    humanLoopActivationConditionsEvaluationResults ::
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

-- | Creates a value of 'HumanLoopActivationOutput' with the minimum fields required to make a request.
--
-- * 'humanLoopARN' - The Amazon Resource Name (ARN) of the HumanLoop created.
-- * 'humanLoopActivationConditionsEvaluationResults' - Shows the result of condition evaluations, including those conditions which activated a human review.
-- * 'humanLoopActivationReasons' - Shows if and why human review was needed.
mkHumanLoopActivationOutput ::
  HumanLoopActivationOutput
mkHumanLoopActivationOutput =
  HumanLoopActivationOutput'
    { humanLoopActivationReasons =
        Lude.Nothing,
      humanLoopARN = Lude.Nothing,
      humanLoopActivationConditionsEvaluationResults = Lude.Nothing
    }

-- | Shows if and why human review was needed.
--
-- /Note:/ Consider using 'humanLoopActivationReasons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaoHumanLoopActivationReasons :: Lens.Lens' HumanLoopActivationOutput (Lude.Maybe (Lude.NonEmpty Lude.Text))
hlaoHumanLoopActivationReasons = Lens.lens (humanLoopActivationReasons :: HumanLoopActivationOutput -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {humanLoopActivationReasons = a} :: HumanLoopActivationOutput)
{-# DEPRECATED hlaoHumanLoopActivationReasons "Use generic-lens or generic-optics with 'humanLoopActivationReasons' instead." #-}

-- | The Amazon Resource Name (ARN) of the HumanLoop created.
--
-- /Note:/ Consider using 'humanLoopARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaoHumanLoopARN :: Lens.Lens' HumanLoopActivationOutput (Lude.Maybe Lude.Text)
hlaoHumanLoopARN = Lens.lens (humanLoopARN :: HumanLoopActivationOutput -> Lude.Maybe Lude.Text) (\s a -> s {humanLoopARN = a} :: HumanLoopActivationOutput)
{-# DEPRECATED hlaoHumanLoopARN "Use generic-lens or generic-optics with 'humanLoopARN' instead." #-}

-- | Shows the result of condition evaluations, including those conditions which activated a human review.
--
-- /Note:/ Consider using 'humanLoopActivationConditionsEvaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlaoHumanLoopActivationConditionsEvaluationResults :: Lens.Lens' HumanLoopActivationOutput (Lude.Maybe Lude.Text)
hlaoHumanLoopActivationConditionsEvaluationResults = Lens.lens (humanLoopActivationConditionsEvaluationResults :: HumanLoopActivationOutput -> Lude.Maybe Lude.Text) (\s a -> s {humanLoopActivationConditionsEvaluationResults = a} :: HumanLoopActivationOutput)
{-# DEPRECATED hlaoHumanLoopActivationConditionsEvaluationResults "Use generic-lens or generic-optics with 'humanLoopActivationConditionsEvaluationResults' instead." #-}

instance Lude.FromJSON HumanLoopActivationOutput where
  parseJSON =
    Lude.withObject
      "HumanLoopActivationOutput"
      ( \x ->
          HumanLoopActivationOutput'
            Lude.<$> (x Lude..:? "HumanLoopActivationReasons")
            Lude.<*> (x Lude..:? "HumanLoopArn")
            Lude.<*> (x Lude..:? "HumanLoopActivationConditionsEvaluationResults")
      )

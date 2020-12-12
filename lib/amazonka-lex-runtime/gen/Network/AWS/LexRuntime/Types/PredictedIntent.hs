{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.PredictedIntent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.PredictedIntent
  ( PredictedIntent (..),

    -- * Smart constructor
    mkPredictedIntent,

    -- * Lenses
    piNluIntentConfidence,
    piSlots,
    piIntentName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.IntentConfidence
import qualified Network.AWS.Prelude as Lude

-- | An intent that Amazon Lex suggests satisfies the user's intent. Includes the name of the intent, the confidence that Amazon Lex has that the user's intent is satisfied, and the slots defined for the intent.
--
-- /See:/ 'mkPredictedIntent' smart constructor.
data PredictedIntent = PredictedIntent'
  { nluIntentConfidence ::
      Lude.Maybe IntentConfidence,
    slots :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    intentName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PredictedIntent' with the minimum fields required to make a request.
--
-- * 'intentName' - The name of the intent that Amazon Lex suggests satisfies the user's intent.
-- * 'nluIntentConfidence' - Indicates how confident Amazon Lex is that an intent satisfies the user's intent.
-- * 'slots' - The slot and slot values associated with the predicted intent.
mkPredictedIntent ::
  PredictedIntent
mkPredictedIntent =
  PredictedIntent'
    { nluIntentConfidence = Lude.Nothing,
      slots = Lude.Nothing,
      intentName = Lude.Nothing
    }

-- | Indicates how confident Amazon Lex is that an intent satisfies the user's intent.
--
-- /Note:/ Consider using 'nluIntentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piNluIntentConfidence :: Lens.Lens' PredictedIntent (Lude.Maybe IntentConfidence)
piNluIntentConfidence = Lens.lens (nluIntentConfidence :: PredictedIntent -> Lude.Maybe IntentConfidence) (\s a -> s {nluIntentConfidence = a} :: PredictedIntent)
{-# DEPRECATED piNluIntentConfidence "Use generic-lens or generic-optics with 'nluIntentConfidence' instead." #-}

-- | The slot and slot values associated with the predicted intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSlots :: Lens.Lens' PredictedIntent (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
piSlots = Lens.lens (slots :: PredictedIntent -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {slots = a} :: PredictedIntent)
{-# DEPRECATED piSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The name of the intent that Amazon Lex suggests satisfies the user's intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piIntentName :: Lens.Lens' PredictedIntent (Lude.Maybe Lude.Text)
piIntentName = Lens.lens (intentName :: PredictedIntent -> Lude.Maybe Lude.Text) (\s a -> s {intentName = a} :: PredictedIntent)
{-# DEPRECATED piIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

instance Lude.FromJSON PredictedIntent where
  parseJSON =
    Lude.withObject
      "PredictedIntent"
      ( \x ->
          PredictedIntent'
            Lude.<$> (x Lude..:? "nluIntentConfidence")
            Lude.<*> (x Lude..:? "slots" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "intentName")
      )

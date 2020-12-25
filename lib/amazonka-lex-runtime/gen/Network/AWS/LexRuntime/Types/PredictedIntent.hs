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
    piIntentName,
    piNluIntentConfidence,
    piSlots,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types.IntentConfidence as Types
import qualified Network.AWS.LexRuntime.Types.IntentName as Types
import qualified Network.AWS.LexRuntime.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | An intent that Amazon Lex suggests satisfies the user's intent. Includes the name of the intent, the confidence that Amazon Lex has that the user's intent is satisfied, and the slots defined for the intent.
--
-- /See:/ 'mkPredictedIntent' smart constructor.
data PredictedIntent = PredictedIntent'
  { -- | The name of the intent that Amazon Lex suggests satisfies the user's intent.
    intentName :: Core.Maybe Types.IntentName,
    -- | Indicates how confident Amazon Lex is that an intent satisfies the user's intent.
    nluIntentConfidence :: Core.Maybe Types.IntentConfidence,
    -- | The slot and slot values associated with the predicted intent.
    slots :: Core.Maybe (Core.HashMap Types.String Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PredictedIntent' value with any optional fields omitted.
mkPredictedIntent ::
  PredictedIntent
mkPredictedIntent =
  PredictedIntent'
    { intentName = Core.Nothing,
      nluIntentConfidence = Core.Nothing,
      slots = Core.Nothing
    }

-- | The name of the intent that Amazon Lex suggests satisfies the user's intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piIntentName :: Lens.Lens' PredictedIntent (Core.Maybe Types.IntentName)
piIntentName = Lens.field @"intentName"
{-# DEPRECATED piIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | Indicates how confident Amazon Lex is that an intent satisfies the user's intent.
--
-- /Note:/ Consider using 'nluIntentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piNluIntentConfidence :: Lens.Lens' PredictedIntent (Core.Maybe Types.IntentConfidence)
piNluIntentConfidence = Lens.field @"nluIntentConfidence"
{-# DEPRECATED piNluIntentConfidence "Use generic-lens or generic-optics with 'nluIntentConfidence' instead." #-}

-- | The slot and slot values associated with the predicted intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSlots :: Lens.Lens' PredictedIntent (Core.Maybe (Core.HashMap Types.String Types.String))
piSlots = Lens.field @"slots"
{-# DEPRECATED piSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

instance Core.FromJSON PredictedIntent where
  parseJSON =
    Core.withObject "PredictedIntent" Core.$
      \x ->
        PredictedIntent'
          Core.<$> (x Core..:? "intentName")
          Core.<*> (x Core..:? "nluIntentConfidence")
          Core.<*> (x Core..:? "slots")

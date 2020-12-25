{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.IntentConfidence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.IntentConfidence
  ( IntentConfidence (..),

    -- * Smart constructor
    mkIntentConfidence,

    -- * Lenses
    icScore,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a score that indicates the confidence that Amazon Lex has that an intent is the one that satisfies the user's intent.
--
-- /See:/ 'mkIntentConfidence' smart constructor.
newtype IntentConfidence = IntentConfidence'
  { -- | A score that indicates how confident Amazon Lex is that an intent satisfies the user's intent. Ranges between 0.00 and 1.00. Higher scores indicate higher confidence.
    score :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'IntentConfidence' value with any optional fields omitted.
mkIntentConfidence ::
  IntentConfidence
mkIntentConfidence = IntentConfidence' {score = Core.Nothing}

-- | A score that indicates how confident Amazon Lex is that an intent satisfies the user's intent. Ranges between 0.00 and 1.00. Higher scores indicate higher confidence.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icScore :: Lens.Lens' IntentConfidence (Core.Maybe Core.Double)
icScore = Lens.field @"score"
{-# DEPRECATED icScore "Use generic-lens or generic-optics with 'score' instead." #-}

instance Core.FromJSON IntentConfidence where
  parseJSON =
    Core.withObject "IntentConfidence" Core.$
      \x -> IntentConfidence' Core.<$> (x Core..:? "score")

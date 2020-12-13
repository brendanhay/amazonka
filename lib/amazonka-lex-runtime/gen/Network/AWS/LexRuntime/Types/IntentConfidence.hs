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
import qualified Network.AWS.Prelude as Lude

-- | Provides a score that indicates the confidence that Amazon Lex has that an intent is the one that satisfies the user's intent.
--
-- /See:/ 'mkIntentConfidence' smart constructor.
newtype IntentConfidence = IntentConfidence'
  { -- | A score that indicates how confident Amazon Lex is that an intent satisfies the user's intent. Ranges between 0.00 and 1.00. Higher scores indicate higher confidence.
    score :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntentConfidence' with the minimum fields required to make a request.
--
-- * 'score' - A score that indicates how confident Amazon Lex is that an intent satisfies the user's intent. Ranges between 0.00 and 1.00. Higher scores indicate higher confidence.
mkIntentConfidence ::
  IntentConfidence
mkIntentConfidence = IntentConfidence' {score = Lude.Nothing}

-- | A score that indicates how confident Amazon Lex is that an intent satisfies the user's intent. Ranges between 0.00 and 1.00. Higher scores indicate higher confidence.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icScore :: Lens.Lens' IntentConfidence (Lude.Maybe Lude.Double)
icScore = Lens.lens (score :: IntentConfidence -> Lude.Maybe Lude.Double) (\s a -> s {score = a} :: IntentConfidence)
{-# DEPRECATED icScore "Use generic-lens or generic-optics with 'score' instead." #-}

instance Lude.FromJSON IntentConfidence where
  parseJSON =
    Lude.withObject
      "IntentConfidence"
      (\x -> IntentConfidence' Lude.<$> (x Lude..:? "score"))

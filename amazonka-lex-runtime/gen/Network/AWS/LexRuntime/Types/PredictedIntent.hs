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
-- Module      : Network.AWS.LexRuntime.Types.PredictedIntent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.PredictedIntent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.IntentConfidence

-- | An intent that Amazon Lex suggests satisfies the user\'s intent.
-- Includes the name of the intent, the confidence that Amazon Lex has that
-- the user\'s intent is satisfied, and the slots defined for the intent.
--
-- /See:/ 'newPredictedIntent' smart constructor.
data PredictedIntent = PredictedIntent'
  { -- | The name of the intent that Amazon Lex suggests satisfies the user\'s
    -- intent.
    intentName :: Core.Maybe Core.Text,
    -- | The slot and slot values associated with the predicted intent.
    slots :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | Indicates how confident Amazon Lex is that an intent satisfies the
    -- user\'s intent.
    nluIntentConfidence :: Core.Maybe IntentConfidence
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PredictedIntent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'predictedIntent_intentName' - The name of the intent that Amazon Lex suggests satisfies the user\'s
-- intent.
--
-- 'slots', 'predictedIntent_slots' - The slot and slot values associated with the predicted intent.
--
-- 'nluIntentConfidence', 'predictedIntent_nluIntentConfidence' - Indicates how confident Amazon Lex is that an intent satisfies the
-- user\'s intent.
newPredictedIntent ::
  PredictedIntent
newPredictedIntent =
  PredictedIntent'
    { intentName = Core.Nothing,
      slots = Core.Nothing,
      nluIntentConfidence = Core.Nothing
    }

-- | The name of the intent that Amazon Lex suggests satisfies the user\'s
-- intent.
predictedIntent_intentName :: Lens.Lens' PredictedIntent (Core.Maybe Core.Text)
predictedIntent_intentName = Lens.lens (\PredictedIntent' {intentName} -> intentName) (\s@PredictedIntent' {} a -> s {intentName = a} :: PredictedIntent)

-- | The slot and slot values associated with the predicted intent.
predictedIntent_slots :: Lens.Lens' PredictedIntent (Core.Maybe (Core.HashMap Core.Text Core.Text))
predictedIntent_slots = Lens.lens (\PredictedIntent' {slots} -> slots) (\s@PredictedIntent' {} a -> s {slots = a} :: PredictedIntent) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | Indicates how confident Amazon Lex is that an intent satisfies the
-- user\'s intent.
predictedIntent_nluIntentConfidence :: Lens.Lens' PredictedIntent (Core.Maybe IntentConfidence)
predictedIntent_nluIntentConfidence = Lens.lens (\PredictedIntent' {nluIntentConfidence} -> nluIntentConfidence) (\s@PredictedIntent' {} a -> s {nluIntentConfidence = a} :: PredictedIntent)

instance Core.FromJSON PredictedIntent where
  parseJSON =
    Core.withObject
      "PredictedIntent"
      ( \x ->
          PredictedIntent'
            Core.<$> (x Core..:? "intentName")
            Core.<*> (x Core..:? "slots" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nluIntentConfidence")
      )

instance Core.Hashable PredictedIntent

instance Core.NFData PredictedIntent

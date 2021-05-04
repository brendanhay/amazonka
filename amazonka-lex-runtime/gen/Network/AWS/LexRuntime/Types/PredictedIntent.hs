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
-- Module      : Network.AWS.LexRuntime.Types.PredictedIntent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.PredictedIntent where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.IntentConfidence
import qualified Network.AWS.Prelude as Prelude

-- | An intent that Amazon Lex suggests satisfies the user\'s intent.
-- Includes the name of the intent, the confidence that Amazon Lex has that
-- the user\'s intent is satisfied, and the slots defined for the intent.
--
-- /See:/ 'newPredictedIntent' smart constructor.
data PredictedIntent = PredictedIntent'
  { -- | The name of the intent that Amazon Lex suggests satisfies the user\'s
    -- intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | The slot and slot values associated with the predicted intent.
    slots :: Prelude.Maybe (Prelude.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Indicates how confident Amazon Lex is that an intent satisfies the
    -- user\'s intent.
    nluIntentConfidence :: Prelude.Maybe IntentConfidence
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { intentName = Prelude.Nothing,
      slots = Prelude.Nothing,
      nluIntentConfidence = Prelude.Nothing
    }

-- | The name of the intent that Amazon Lex suggests satisfies the user\'s
-- intent.
predictedIntent_intentName :: Lens.Lens' PredictedIntent (Prelude.Maybe Prelude.Text)
predictedIntent_intentName = Lens.lens (\PredictedIntent' {intentName} -> intentName) (\s@PredictedIntent' {} a -> s {intentName = a} :: PredictedIntent)

-- | The slot and slot values associated with the predicted intent.
predictedIntent_slots :: Lens.Lens' PredictedIntent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
predictedIntent_slots = Lens.lens (\PredictedIntent' {slots} -> slots) (\s@PredictedIntent' {} a -> s {slots = a} :: PredictedIntent) Prelude.. Lens.mapping (Prelude._Sensitive Prelude.. Prelude._Coerce)

-- | Indicates how confident Amazon Lex is that an intent satisfies the
-- user\'s intent.
predictedIntent_nluIntentConfidence :: Lens.Lens' PredictedIntent (Prelude.Maybe IntentConfidence)
predictedIntent_nluIntentConfidence = Lens.lens (\PredictedIntent' {nluIntentConfidence} -> nluIntentConfidence) (\s@PredictedIntent' {} a -> s {nluIntentConfidence = a} :: PredictedIntent)

instance Prelude.FromJSON PredictedIntent where
  parseJSON =
    Prelude.withObject
      "PredictedIntent"
      ( \x ->
          PredictedIntent'
            Prelude.<$> (x Prelude..:? "intentName")
            Prelude.<*> (x Prelude..:? "slots" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "nluIntentConfidence")
      )

instance Prelude.Hashable PredictedIntent

instance Prelude.NFData PredictedIntent

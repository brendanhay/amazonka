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
-- Module      : Amazonka.LexRuntime.Types.PredictedIntent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.PredictedIntent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexRuntime.Types.IntentConfidence
import qualified Amazonka.Prelude as Prelude

-- | An intent that Amazon Lex suggests satisfies the user\'s intent.
-- Includes the name of the intent, the confidence that Amazon Lex has that
-- the user\'s intent is satisfied, and the slots defined for the intent.
--
-- /See:/ 'newPredictedIntent' smart constructor.
data PredictedIntent = PredictedIntent'
  { -- | The name of the intent that Amazon Lex suggests satisfies the user\'s
    -- intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | Indicates how confident Amazon Lex is that an intent satisfies the
    -- user\'s intent.
    nluIntentConfidence :: Prelude.Maybe IntentConfidence,
    -- | The slot and slot values associated with the predicted intent.
    slots :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'nluIntentConfidence', 'predictedIntent_nluIntentConfidence' - Indicates how confident Amazon Lex is that an intent satisfies the
-- user\'s intent.
--
-- 'slots', 'predictedIntent_slots' - The slot and slot values associated with the predicted intent.
newPredictedIntent ::
  PredictedIntent
newPredictedIntent =
  PredictedIntent'
    { intentName = Prelude.Nothing,
      nluIntentConfidence = Prelude.Nothing,
      slots = Prelude.Nothing
    }

-- | The name of the intent that Amazon Lex suggests satisfies the user\'s
-- intent.
predictedIntent_intentName :: Lens.Lens' PredictedIntent (Prelude.Maybe Prelude.Text)
predictedIntent_intentName = Lens.lens (\PredictedIntent' {intentName} -> intentName) (\s@PredictedIntent' {} a -> s {intentName = a} :: PredictedIntent)

-- | Indicates how confident Amazon Lex is that an intent satisfies the
-- user\'s intent.
predictedIntent_nluIntentConfidence :: Lens.Lens' PredictedIntent (Prelude.Maybe IntentConfidence)
predictedIntent_nluIntentConfidence = Lens.lens (\PredictedIntent' {nluIntentConfidence} -> nluIntentConfidence) (\s@PredictedIntent' {} a -> s {nluIntentConfidence = a} :: PredictedIntent)

-- | The slot and slot values associated with the predicted intent.
predictedIntent_slots :: Lens.Lens' PredictedIntent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
predictedIntent_slots = Lens.lens (\PredictedIntent' {slots} -> slots) (\s@PredictedIntent' {} a -> s {slots = a} :: PredictedIntent) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON PredictedIntent where
  parseJSON =
    Data.withObject
      "PredictedIntent"
      ( \x ->
          PredictedIntent'
            Prelude.<$> (x Data..:? "intentName")
            Prelude.<*> (x Data..:? "nluIntentConfidence")
            Prelude.<*> (x Data..:? "slots" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PredictedIntent where
  hashWithSalt _salt PredictedIntent' {..} =
    _salt `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` nluIntentConfidence
      `Prelude.hashWithSalt` slots

instance Prelude.NFData PredictedIntent where
  rnf PredictedIntent' {..} =
    Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf nluIntentConfidence
      `Prelude.seq` Prelude.rnf slots

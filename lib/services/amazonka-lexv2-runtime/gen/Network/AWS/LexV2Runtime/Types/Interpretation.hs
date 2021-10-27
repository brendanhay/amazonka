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
-- Module      : Network.AWS.LexV2Runtime.Types.Interpretation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.Interpretation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.ConfidenceScore
import Network.AWS.LexV2Runtime.Types.Intent
import Network.AWS.LexV2Runtime.Types.SentimentResponse
import qualified Network.AWS.Prelude as Prelude

-- | An intent that Amazon Lex V2 determined might satisfy the user\'s
-- utterance. The intents are ordered by the confidence score.
--
-- /See:/ 'newInterpretation' smart constructor.
data Interpretation = Interpretation'
  { -- | The sentiment expressed in an utterance.
    --
    -- When the bot is configured to send utterances to Amazon Comprehend for
    -- sentiment analysis, this field contains the result of the analysis.
    sentimentResponse :: Prelude.Maybe SentimentResponse,
    -- | A list of intents that might satisfy the user\'s utterance. The intents
    -- are ordered by the confidence score.
    intent :: Prelude.Maybe Intent,
    -- | Determines the threshold where Amazon Lex V2 will insert the
    -- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
    -- returning alternative intents in a response. @AMAZON.FallbackIntent@ and
    -- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
    -- the bot.
    nluConfidence :: Prelude.Maybe ConfidenceScore
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Interpretation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentimentResponse', 'interpretation_sentimentResponse' - The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for
-- sentiment analysis, this field contains the result of the analysis.
--
-- 'intent', 'interpretation_intent' - A list of intents that might satisfy the user\'s utterance. The intents
-- are ordered by the confidence score.
--
-- 'nluConfidence', 'interpretation_nluConfidence' - Determines the threshold where Amazon Lex V2 will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a response. @AMAZON.FallbackIntent@ and
-- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
-- the bot.
newInterpretation ::
  Interpretation
newInterpretation =
  Interpretation'
    { sentimentResponse =
        Prelude.Nothing,
      intent = Prelude.Nothing,
      nluConfidence = Prelude.Nothing
    }

-- | The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for
-- sentiment analysis, this field contains the result of the analysis.
interpretation_sentimentResponse :: Lens.Lens' Interpretation (Prelude.Maybe SentimentResponse)
interpretation_sentimentResponse = Lens.lens (\Interpretation' {sentimentResponse} -> sentimentResponse) (\s@Interpretation' {} a -> s {sentimentResponse = a} :: Interpretation)

-- | A list of intents that might satisfy the user\'s utterance. The intents
-- are ordered by the confidence score.
interpretation_intent :: Lens.Lens' Interpretation (Prelude.Maybe Intent)
interpretation_intent = Lens.lens (\Interpretation' {intent} -> intent) (\s@Interpretation' {} a -> s {intent = a} :: Interpretation)

-- | Determines the threshold where Amazon Lex V2 will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a response. @AMAZON.FallbackIntent@ and
-- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
-- the bot.
interpretation_nluConfidence :: Lens.Lens' Interpretation (Prelude.Maybe ConfidenceScore)
interpretation_nluConfidence = Lens.lens (\Interpretation' {nluConfidence} -> nluConfidence) (\s@Interpretation' {} a -> s {nluConfidence = a} :: Interpretation)

instance Core.FromJSON Interpretation where
  parseJSON =
    Core.withObject
      "Interpretation"
      ( \x ->
          Interpretation'
            Prelude.<$> (x Core..:? "sentimentResponse")
            Prelude.<*> (x Core..:? "intent")
            Prelude.<*> (x Core..:? "nluConfidence")
      )

instance Prelude.Hashable Interpretation

instance Prelude.NFData Interpretation

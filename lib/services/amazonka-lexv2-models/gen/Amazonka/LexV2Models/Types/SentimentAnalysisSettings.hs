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
-- Module      : Amazonka.LexV2Models.Types.SentimentAnalysisSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SentimentAnalysisSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines whether Amazon Lex will use Amazon Comprehend to detect the
-- sentiment of user utterances.
--
-- /See:/ 'newSentimentAnalysisSettings' smart constructor.
data SentimentAnalysisSettings = SentimentAnalysisSettings'
  { -- | Sets whether Amazon Lex uses Amazon Comprehend to detect the sentiment
    -- of user utterances.
    detectSentiment :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SentimentAnalysisSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectSentiment', 'sentimentAnalysisSettings_detectSentiment' - Sets whether Amazon Lex uses Amazon Comprehend to detect the sentiment
-- of user utterances.
newSentimentAnalysisSettings ::
  -- | 'detectSentiment'
  Prelude.Bool ->
  SentimentAnalysisSettings
newSentimentAnalysisSettings pDetectSentiment_ =
  SentimentAnalysisSettings'
    { detectSentiment =
        pDetectSentiment_
    }

-- | Sets whether Amazon Lex uses Amazon Comprehend to detect the sentiment
-- of user utterances.
sentimentAnalysisSettings_detectSentiment :: Lens.Lens' SentimentAnalysisSettings Prelude.Bool
sentimentAnalysisSettings_detectSentiment = Lens.lens (\SentimentAnalysisSettings' {detectSentiment} -> detectSentiment) (\s@SentimentAnalysisSettings' {} a -> s {detectSentiment = a} :: SentimentAnalysisSettings)

instance Data.FromJSON SentimentAnalysisSettings where
  parseJSON =
    Data.withObject
      "SentimentAnalysisSettings"
      ( \x ->
          SentimentAnalysisSettings'
            Prelude.<$> (x Data..: "detectSentiment")
      )

instance Prelude.Hashable SentimentAnalysisSettings where
  hashWithSalt _salt SentimentAnalysisSettings' {..} =
    _salt `Prelude.hashWithSalt` detectSentiment

instance Prelude.NFData SentimentAnalysisSettings where
  rnf SentimentAnalysisSettings' {..} =
    Prelude.rnf detectSentiment

instance Data.ToJSON SentimentAnalysisSettings where
  toJSON SentimentAnalysisSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("detectSentiment" Data..= detectSentiment)
          ]
      )

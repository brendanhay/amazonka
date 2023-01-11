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
-- Module      : Amazonka.LexV2Models.Types.RecommendedIntentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.RecommendedIntentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains a summary of a recommended intent.
--
-- /See:/ 'newRecommendedIntentSummary' smart constructor.
data RecommendedIntentSummary = RecommendedIntentSummary'
  { -- | The unique identifier of a recommended intent associated with the bot
    -- recommendation.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The name of a recommended intent associated with the bot recommendation.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | The count of sample utterances of a recommended intent that is
    -- associated with a bot recommendation.
    sampleUtterancesCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendedIntentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentId', 'recommendedIntentSummary_intentId' - The unique identifier of a recommended intent associated with the bot
-- recommendation.
--
-- 'intentName', 'recommendedIntentSummary_intentName' - The name of a recommended intent associated with the bot recommendation.
--
-- 'sampleUtterancesCount', 'recommendedIntentSummary_sampleUtterancesCount' - The count of sample utterances of a recommended intent that is
-- associated with a bot recommendation.
newRecommendedIntentSummary ::
  RecommendedIntentSummary
newRecommendedIntentSummary =
  RecommendedIntentSummary'
    { intentId =
        Prelude.Nothing,
      intentName = Prelude.Nothing,
      sampleUtterancesCount = Prelude.Nothing
    }

-- | The unique identifier of a recommended intent associated with the bot
-- recommendation.
recommendedIntentSummary_intentId :: Lens.Lens' RecommendedIntentSummary (Prelude.Maybe Prelude.Text)
recommendedIntentSummary_intentId = Lens.lens (\RecommendedIntentSummary' {intentId} -> intentId) (\s@RecommendedIntentSummary' {} a -> s {intentId = a} :: RecommendedIntentSummary)

-- | The name of a recommended intent associated with the bot recommendation.
recommendedIntentSummary_intentName :: Lens.Lens' RecommendedIntentSummary (Prelude.Maybe Prelude.Text)
recommendedIntentSummary_intentName = Lens.lens (\RecommendedIntentSummary' {intentName} -> intentName) (\s@RecommendedIntentSummary' {} a -> s {intentName = a} :: RecommendedIntentSummary)

-- | The count of sample utterances of a recommended intent that is
-- associated with a bot recommendation.
recommendedIntentSummary_sampleUtterancesCount :: Lens.Lens' RecommendedIntentSummary (Prelude.Maybe Prelude.Int)
recommendedIntentSummary_sampleUtterancesCount = Lens.lens (\RecommendedIntentSummary' {sampleUtterancesCount} -> sampleUtterancesCount) (\s@RecommendedIntentSummary' {} a -> s {sampleUtterancesCount = a} :: RecommendedIntentSummary)

instance Data.FromJSON RecommendedIntentSummary where
  parseJSON =
    Data.withObject
      "RecommendedIntentSummary"
      ( \x ->
          RecommendedIntentSummary'
            Prelude.<$> (x Data..:? "intentId")
            Prelude.<*> (x Data..:? "intentName")
            Prelude.<*> (x Data..:? "sampleUtterancesCount")
      )

instance Prelude.Hashable RecommendedIntentSummary where
  hashWithSalt _salt RecommendedIntentSummary' {..} =
    _salt `Prelude.hashWithSalt` intentId
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` sampleUtterancesCount

instance Prelude.NFData RecommendedIntentSummary where
  rnf RecommendedIntentSummary' {..} =
    Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf sampleUtterancesCount

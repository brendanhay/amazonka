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
-- Module      : Amazonka.LexV2Models.Types.BotRecommendationResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotRecommendationResults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BotRecommendationResultStatistics
import qualified Amazonka.Prelude as Prelude

-- | The object representing the URL of the bot definition, the URL of the
-- associated transcript, and a statistical summary of the bot
-- recommendation results.
--
-- /See:/ 'newBotRecommendationResults' smart constructor.
data BotRecommendationResults = BotRecommendationResults'
  { -- | The presigned url link of the associated transcript.
    associatedTranscriptsUrl :: Prelude.Maybe Prelude.Text,
    -- | The presigned URL link of the recommended bot definition.
    botLocaleExportUrl :: Prelude.Maybe Prelude.Text,
    -- | The statistical summary of the bot recommendation results.
    statistics :: Prelude.Maybe BotRecommendationResultStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotRecommendationResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedTranscriptsUrl', 'botRecommendationResults_associatedTranscriptsUrl' - The presigned url link of the associated transcript.
--
-- 'botLocaleExportUrl', 'botRecommendationResults_botLocaleExportUrl' - The presigned URL link of the recommended bot definition.
--
-- 'statistics', 'botRecommendationResults_statistics' - The statistical summary of the bot recommendation results.
newBotRecommendationResults ::
  BotRecommendationResults
newBotRecommendationResults =
  BotRecommendationResults'
    { associatedTranscriptsUrl =
        Prelude.Nothing,
      botLocaleExportUrl = Prelude.Nothing,
      statistics = Prelude.Nothing
    }

-- | The presigned url link of the associated transcript.
botRecommendationResults_associatedTranscriptsUrl :: Lens.Lens' BotRecommendationResults (Prelude.Maybe Prelude.Text)
botRecommendationResults_associatedTranscriptsUrl = Lens.lens (\BotRecommendationResults' {associatedTranscriptsUrl} -> associatedTranscriptsUrl) (\s@BotRecommendationResults' {} a -> s {associatedTranscriptsUrl = a} :: BotRecommendationResults)

-- | The presigned URL link of the recommended bot definition.
botRecommendationResults_botLocaleExportUrl :: Lens.Lens' BotRecommendationResults (Prelude.Maybe Prelude.Text)
botRecommendationResults_botLocaleExportUrl = Lens.lens (\BotRecommendationResults' {botLocaleExportUrl} -> botLocaleExportUrl) (\s@BotRecommendationResults' {} a -> s {botLocaleExportUrl = a} :: BotRecommendationResults)

-- | The statistical summary of the bot recommendation results.
botRecommendationResults_statistics :: Lens.Lens' BotRecommendationResults (Prelude.Maybe BotRecommendationResultStatistics)
botRecommendationResults_statistics = Lens.lens (\BotRecommendationResults' {statistics} -> statistics) (\s@BotRecommendationResults' {} a -> s {statistics = a} :: BotRecommendationResults)

instance Data.FromJSON BotRecommendationResults where
  parseJSON =
    Data.withObject
      "BotRecommendationResults"
      ( \x ->
          BotRecommendationResults'
            Prelude.<$> (x Data..:? "associatedTranscriptsUrl")
            Prelude.<*> (x Data..:? "botLocaleExportUrl")
            Prelude.<*> (x Data..:? "statistics")
      )

instance Prelude.Hashable BotRecommendationResults where
  hashWithSalt _salt BotRecommendationResults' {..} =
    _salt
      `Prelude.hashWithSalt` associatedTranscriptsUrl
      `Prelude.hashWithSalt` botLocaleExportUrl
      `Prelude.hashWithSalt` statistics

instance Prelude.NFData BotRecommendationResults where
  rnf BotRecommendationResults' {..} =
    Prelude.rnf associatedTranscriptsUrl `Prelude.seq`
      Prelude.rnf botLocaleExportUrl `Prelude.seq`
        Prelude.rnf statistics

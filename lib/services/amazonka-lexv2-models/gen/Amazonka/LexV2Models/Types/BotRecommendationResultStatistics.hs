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
-- Module      : Amazonka.LexV2Models.Types.BotRecommendationResultStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotRecommendationResultStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.IntentStatistics
import Amazonka.LexV2Models.Types.SlotTypeStatistics
import qualified Amazonka.Prelude as Prelude

-- | A statistical summary of the bot recommendation results.
--
-- /See:/ 'newBotRecommendationResultStatistics' smart constructor.
data BotRecommendationResultStatistics = BotRecommendationResultStatistics'
  { -- | Statistical information about about the intents associated with the bot
    -- recommendation results.
    intents :: Prelude.Maybe IntentStatistics,
    -- | Statistical information about the slot types associated with the bot
    -- recommendation results.
    slotTypes :: Prelude.Maybe SlotTypeStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotRecommendationResultStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intents', 'botRecommendationResultStatistics_intents' - Statistical information about about the intents associated with the bot
-- recommendation results.
--
-- 'slotTypes', 'botRecommendationResultStatistics_slotTypes' - Statistical information about the slot types associated with the bot
-- recommendation results.
newBotRecommendationResultStatistics ::
  BotRecommendationResultStatistics
newBotRecommendationResultStatistics =
  BotRecommendationResultStatistics'
    { intents =
        Prelude.Nothing,
      slotTypes = Prelude.Nothing
    }

-- | Statistical information about about the intents associated with the bot
-- recommendation results.
botRecommendationResultStatistics_intents :: Lens.Lens' BotRecommendationResultStatistics (Prelude.Maybe IntentStatistics)
botRecommendationResultStatistics_intents = Lens.lens (\BotRecommendationResultStatistics' {intents} -> intents) (\s@BotRecommendationResultStatistics' {} a -> s {intents = a} :: BotRecommendationResultStatistics)

-- | Statistical information about the slot types associated with the bot
-- recommendation results.
botRecommendationResultStatistics_slotTypes :: Lens.Lens' BotRecommendationResultStatistics (Prelude.Maybe SlotTypeStatistics)
botRecommendationResultStatistics_slotTypes = Lens.lens (\BotRecommendationResultStatistics' {slotTypes} -> slotTypes) (\s@BotRecommendationResultStatistics' {} a -> s {slotTypes = a} :: BotRecommendationResultStatistics)

instance
  Data.FromJSON
    BotRecommendationResultStatistics
  where
  parseJSON =
    Data.withObject
      "BotRecommendationResultStatistics"
      ( \x ->
          BotRecommendationResultStatistics'
            Prelude.<$> (x Data..:? "intents")
            Prelude.<*> (x Data..:? "slotTypes")
      )

instance
  Prelude.Hashable
    BotRecommendationResultStatistics
  where
  hashWithSalt
    _salt
    BotRecommendationResultStatistics' {..} =
      _salt `Prelude.hashWithSalt` intents
        `Prelude.hashWithSalt` slotTypes

instance
  Prelude.NFData
    BotRecommendationResultStatistics
  where
  rnf BotRecommendationResultStatistics' {..} =
    Prelude.rnf intents
      `Prelude.seq` Prelude.rnf slotTypes

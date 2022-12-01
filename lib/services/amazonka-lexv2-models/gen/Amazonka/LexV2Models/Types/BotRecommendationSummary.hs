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
-- Module      : Amazonka.LexV2Models.Types.BotRecommendationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotRecommendationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.BotRecommendationStatus
import qualified Amazonka.Prelude as Prelude

-- | A summary of the bot recommendation.
--
-- /See:/ 'newBotRecommendationSummary' smart constructor.
data BotRecommendationSummary = BotRecommendationSummary'
  { -- | A timestamp of the date and time that the bot recommendation was
    -- created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | A timestamp of the date and time that the bot recommendation was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the bot recommendation.
    --
    -- If the status is Failed, then the reasons for the failure are listed in
    -- the failureReasons field.
    botRecommendationStatus :: BotRecommendationStatus,
    -- | The unique identifier of the bot recommendation to be updated.
    botRecommendationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotRecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'botRecommendationSummary_creationDateTime' - A timestamp of the date and time that the bot recommendation was
-- created.
--
-- 'lastUpdatedDateTime', 'botRecommendationSummary_lastUpdatedDateTime' - A timestamp of the date and time that the bot recommendation was last
-- updated.
--
-- 'botRecommendationStatus', 'botRecommendationSummary_botRecommendationStatus' - The status of the bot recommendation.
--
-- If the status is Failed, then the reasons for the failure are listed in
-- the failureReasons field.
--
-- 'botRecommendationId', 'botRecommendationSummary_botRecommendationId' - The unique identifier of the bot recommendation to be updated.
newBotRecommendationSummary ::
  -- | 'botRecommendationStatus'
  BotRecommendationStatus ->
  -- | 'botRecommendationId'
  Prelude.Text ->
  BotRecommendationSummary
newBotRecommendationSummary
  pBotRecommendationStatus_
  pBotRecommendationId_ =
    BotRecommendationSummary'
      { creationDateTime =
          Prelude.Nothing,
        lastUpdatedDateTime = Prelude.Nothing,
        botRecommendationStatus =
          pBotRecommendationStatus_,
        botRecommendationId = pBotRecommendationId_
      }

-- | A timestamp of the date and time that the bot recommendation was
-- created.
botRecommendationSummary_creationDateTime :: Lens.Lens' BotRecommendationSummary (Prelude.Maybe Prelude.UTCTime)
botRecommendationSummary_creationDateTime = Lens.lens (\BotRecommendationSummary' {creationDateTime} -> creationDateTime) (\s@BotRecommendationSummary' {} a -> s {creationDateTime = a} :: BotRecommendationSummary) Prelude.. Lens.mapping Core._Time

-- | A timestamp of the date and time that the bot recommendation was last
-- updated.
botRecommendationSummary_lastUpdatedDateTime :: Lens.Lens' BotRecommendationSummary (Prelude.Maybe Prelude.UTCTime)
botRecommendationSummary_lastUpdatedDateTime = Lens.lens (\BotRecommendationSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BotRecommendationSummary' {} a -> s {lastUpdatedDateTime = a} :: BotRecommendationSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the bot recommendation.
--
-- If the status is Failed, then the reasons for the failure are listed in
-- the failureReasons field.
botRecommendationSummary_botRecommendationStatus :: Lens.Lens' BotRecommendationSummary BotRecommendationStatus
botRecommendationSummary_botRecommendationStatus = Lens.lens (\BotRecommendationSummary' {botRecommendationStatus} -> botRecommendationStatus) (\s@BotRecommendationSummary' {} a -> s {botRecommendationStatus = a} :: BotRecommendationSummary)

-- | The unique identifier of the bot recommendation to be updated.
botRecommendationSummary_botRecommendationId :: Lens.Lens' BotRecommendationSummary Prelude.Text
botRecommendationSummary_botRecommendationId = Lens.lens (\BotRecommendationSummary' {botRecommendationId} -> botRecommendationId) (\s@BotRecommendationSummary' {} a -> s {botRecommendationId = a} :: BotRecommendationSummary)

instance Core.FromJSON BotRecommendationSummary where
  parseJSON =
    Core.withObject
      "BotRecommendationSummary"
      ( \x ->
          BotRecommendationSummary'
            Prelude.<$> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..: "botRecommendationStatus")
            Prelude.<*> (x Core..: "botRecommendationId")
      )

instance Prelude.Hashable BotRecommendationSummary where
  hashWithSalt _salt BotRecommendationSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` botRecommendationStatus
      `Prelude.hashWithSalt` botRecommendationId

instance Prelude.NFData BotRecommendationSummary where
  rnf BotRecommendationSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf botRecommendationStatus
      `Prelude.seq` Prelude.rnf botRecommendationId

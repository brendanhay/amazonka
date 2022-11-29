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
-- Module      : Amazonka.LexV2Models.Types.BotLocaleSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotLocaleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.BotLocaleStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information about bot locales returned by the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListBotLocales.html ListBotLocales>
-- operation.
--
-- /See:/ 'newBotLocaleSummary' smart constructor.
data BotLocaleSummary = BotLocaleSummary'
  { -- | The name of the bot locale.
    localeName :: Prelude.Maybe Prelude.Text,
    -- | The language and locale of the bot locale.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The description of the bot locale.
    description :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot locale was last built.
    lastBuildSubmittedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The current status of the bot locale. When the status is @Built@ the
    -- locale is ready for use.
    botLocaleStatus :: Prelude.Maybe BotLocaleStatus,
    -- | A timestamp of the date and time that the bot locale was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotLocaleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localeName', 'botLocaleSummary_localeName' - The name of the bot locale.
--
-- 'localeId', 'botLocaleSummary_localeId' - The language and locale of the bot locale.
--
-- 'description', 'botLocaleSummary_description' - The description of the bot locale.
--
-- 'lastBuildSubmittedDateTime', 'botLocaleSummary_lastBuildSubmittedDateTime' - A timestamp of the date and time that the bot locale was last built.
--
-- 'botLocaleStatus', 'botLocaleSummary_botLocaleStatus' - The current status of the bot locale. When the status is @Built@ the
-- locale is ready for use.
--
-- 'lastUpdatedDateTime', 'botLocaleSummary_lastUpdatedDateTime' - A timestamp of the date and time that the bot locale was last updated.
newBotLocaleSummary ::
  BotLocaleSummary
newBotLocaleSummary =
  BotLocaleSummary'
    { localeName = Prelude.Nothing,
      localeId = Prelude.Nothing,
      description = Prelude.Nothing,
      lastBuildSubmittedDateTime = Prelude.Nothing,
      botLocaleStatus = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The name of the bot locale.
botLocaleSummary_localeName :: Lens.Lens' BotLocaleSummary (Prelude.Maybe Prelude.Text)
botLocaleSummary_localeName = Lens.lens (\BotLocaleSummary' {localeName} -> localeName) (\s@BotLocaleSummary' {} a -> s {localeName = a} :: BotLocaleSummary)

-- | The language and locale of the bot locale.
botLocaleSummary_localeId :: Lens.Lens' BotLocaleSummary (Prelude.Maybe Prelude.Text)
botLocaleSummary_localeId = Lens.lens (\BotLocaleSummary' {localeId} -> localeId) (\s@BotLocaleSummary' {} a -> s {localeId = a} :: BotLocaleSummary)

-- | The description of the bot locale.
botLocaleSummary_description :: Lens.Lens' BotLocaleSummary (Prelude.Maybe Prelude.Text)
botLocaleSummary_description = Lens.lens (\BotLocaleSummary' {description} -> description) (\s@BotLocaleSummary' {} a -> s {description = a} :: BotLocaleSummary)

-- | A timestamp of the date and time that the bot locale was last built.
botLocaleSummary_lastBuildSubmittedDateTime :: Lens.Lens' BotLocaleSummary (Prelude.Maybe Prelude.UTCTime)
botLocaleSummary_lastBuildSubmittedDateTime = Lens.lens (\BotLocaleSummary' {lastBuildSubmittedDateTime} -> lastBuildSubmittedDateTime) (\s@BotLocaleSummary' {} a -> s {lastBuildSubmittedDateTime = a} :: BotLocaleSummary) Prelude.. Lens.mapping Core._Time

-- | The current status of the bot locale. When the status is @Built@ the
-- locale is ready for use.
botLocaleSummary_botLocaleStatus :: Lens.Lens' BotLocaleSummary (Prelude.Maybe BotLocaleStatus)
botLocaleSummary_botLocaleStatus = Lens.lens (\BotLocaleSummary' {botLocaleStatus} -> botLocaleStatus) (\s@BotLocaleSummary' {} a -> s {botLocaleStatus = a} :: BotLocaleSummary)

-- | A timestamp of the date and time that the bot locale was last updated.
botLocaleSummary_lastUpdatedDateTime :: Lens.Lens' BotLocaleSummary (Prelude.Maybe Prelude.UTCTime)
botLocaleSummary_lastUpdatedDateTime = Lens.lens (\BotLocaleSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BotLocaleSummary' {} a -> s {lastUpdatedDateTime = a} :: BotLocaleSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON BotLocaleSummary where
  parseJSON =
    Core.withObject
      "BotLocaleSummary"
      ( \x ->
          BotLocaleSummary'
            Prelude.<$> (x Core..:? "localeName")
            Prelude.<*> (x Core..:? "localeId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "lastBuildSubmittedDateTime")
            Prelude.<*> (x Core..:? "botLocaleStatus")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable BotLocaleSummary where
  hashWithSalt _salt BotLocaleSummary' {..} =
    _salt `Prelude.hashWithSalt` localeName
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastBuildSubmittedDateTime
      `Prelude.hashWithSalt` botLocaleStatus
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData BotLocaleSummary where
  rnf BotLocaleSummary' {..} =
    Prelude.rnf localeName
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastBuildSubmittedDateTime
      `Prelude.seq` Prelude.rnf botLocaleStatus
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime

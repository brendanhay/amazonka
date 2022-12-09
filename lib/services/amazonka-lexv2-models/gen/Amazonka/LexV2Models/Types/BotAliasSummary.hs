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
-- Module      : Amazonka.LexV2Models.Types.BotAliasSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotAliasSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BotAliasStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information about bot aliases returned from the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListBotAliases.html ListBotAliases>
-- operation.
--
-- /See:/ 'newBotAliasSummary' smart constructor.
data BotAliasSummary = BotAliasSummary'
  { -- | The unique identifier assigned to the bot alias. You can use this ID to
    -- get detailed information about the alias using the
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeBotAlias.html DescribeBotAlias>
    -- operation.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot alias.
    botAliasName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the bot alias. If the status is @Available@, the
    -- alias is ready for use.
    botAliasStatus :: Prelude.Maybe BotAliasStatus,
    -- | The version of the bot that the bot alias references.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot alias was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the bot alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot alias was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotAliasSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasId', 'botAliasSummary_botAliasId' - The unique identifier assigned to the bot alias. You can use this ID to
-- get detailed information about the alias using the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeBotAlias.html DescribeBotAlias>
-- operation.
--
-- 'botAliasName', 'botAliasSummary_botAliasName' - The name of the bot alias.
--
-- 'botAliasStatus', 'botAliasSummary_botAliasStatus' - The current state of the bot alias. If the status is @Available@, the
-- alias is ready for use.
--
-- 'botVersion', 'botAliasSummary_botVersion' - The version of the bot that the bot alias references.
--
-- 'creationDateTime', 'botAliasSummary_creationDateTime' - A timestamp of the date and time that the bot alias was created.
--
-- 'description', 'botAliasSummary_description' - The description of the bot alias.
--
-- 'lastUpdatedDateTime', 'botAliasSummary_lastUpdatedDateTime' - A timestamp of the date and time that the bot alias was last updated.
newBotAliasSummary ::
  BotAliasSummary
newBotAliasSummary =
  BotAliasSummary'
    { botAliasId = Prelude.Nothing,
      botAliasName = Prelude.Nothing,
      botAliasStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The unique identifier assigned to the bot alias. You can use this ID to
-- get detailed information about the alias using the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeBotAlias.html DescribeBotAlias>
-- operation.
botAliasSummary_botAliasId :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_botAliasId = Lens.lens (\BotAliasSummary' {botAliasId} -> botAliasId) (\s@BotAliasSummary' {} a -> s {botAliasId = a} :: BotAliasSummary)

-- | The name of the bot alias.
botAliasSummary_botAliasName :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_botAliasName = Lens.lens (\BotAliasSummary' {botAliasName} -> botAliasName) (\s@BotAliasSummary' {} a -> s {botAliasName = a} :: BotAliasSummary)

-- | The current state of the bot alias. If the status is @Available@, the
-- alias is ready for use.
botAliasSummary_botAliasStatus :: Lens.Lens' BotAliasSummary (Prelude.Maybe BotAliasStatus)
botAliasSummary_botAliasStatus = Lens.lens (\BotAliasSummary' {botAliasStatus} -> botAliasStatus) (\s@BotAliasSummary' {} a -> s {botAliasStatus = a} :: BotAliasSummary)

-- | The version of the bot that the bot alias references.
botAliasSummary_botVersion :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_botVersion = Lens.lens (\BotAliasSummary' {botVersion} -> botVersion) (\s@BotAliasSummary' {} a -> s {botVersion = a} :: BotAliasSummary)

-- | A timestamp of the date and time that the bot alias was created.
botAliasSummary_creationDateTime :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.UTCTime)
botAliasSummary_creationDateTime = Lens.lens (\BotAliasSummary' {creationDateTime} -> creationDateTime) (\s@BotAliasSummary' {} a -> s {creationDateTime = a} :: BotAliasSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the bot alias.
botAliasSummary_description :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_description = Lens.lens (\BotAliasSummary' {description} -> description) (\s@BotAliasSummary' {} a -> s {description = a} :: BotAliasSummary)

-- | A timestamp of the date and time that the bot alias was last updated.
botAliasSummary_lastUpdatedDateTime :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.UTCTime)
botAliasSummary_lastUpdatedDateTime = Lens.lens (\BotAliasSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BotAliasSummary' {} a -> s {lastUpdatedDateTime = a} :: BotAliasSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON BotAliasSummary where
  parseJSON =
    Data.withObject
      "BotAliasSummary"
      ( \x ->
          BotAliasSummary'
            Prelude.<$> (x Data..:? "botAliasId")
            Prelude.<*> (x Data..:? "botAliasName")
            Prelude.<*> (x Data..:? "botAliasStatus")
            Prelude.<*> (x Data..:? "botVersion")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable BotAliasSummary where
  hashWithSalt _salt BotAliasSummary' {..} =
    _salt `Prelude.hashWithSalt` botAliasId
      `Prelude.hashWithSalt` botAliasName
      `Prelude.hashWithSalt` botAliasStatus
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData BotAliasSummary where
  rnf BotAliasSummary' {..} =
    Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf botAliasName
      `Prelude.seq` Prelude.rnf botAliasStatus
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime

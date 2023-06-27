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
-- Module      : Amazonka.LexV2Models.Types.TestSetDiscrepancyReportBotAliasTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetDiscrepancyReportBotAliasTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the bot alias used for the test set
-- discrepancy report.
--
-- /See:/ 'newTestSetDiscrepancyReportBotAliasTarget' smart constructor.
data TestSetDiscrepancyReportBotAliasTarget = TestSetDiscrepancyReportBotAliasTarget'
  { -- | The unique identifier for the bot alias.
    botId :: Prelude.Text,
    -- | The unique identifier for the bot associated with the bot alias.
    botAliasId :: Prelude.Text,
    -- | The unique identifier of the locale associated with the bot alias.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetDiscrepancyReportBotAliasTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'testSetDiscrepancyReportBotAliasTarget_botId' - The unique identifier for the bot alias.
--
-- 'botAliasId', 'testSetDiscrepancyReportBotAliasTarget_botAliasId' - The unique identifier for the bot associated with the bot alias.
--
-- 'localeId', 'testSetDiscrepancyReportBotAliasTarget_localeId' - The unique identifier of the locale associated with the bot alias.
newTestSetDiscrepancyReportBotAliasTarget ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  TestSetDiscrepancyReportBotAliasTarget
newTestSetDiscrepancyReportBotAliasTarget
  pBotId_
  pBotAliasId_
  pLocaleId_ =
    TestSetDiscrepancyReportBotAliasTarget'
      { botId =
          pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_
      }

-- | The unique identifier for the bot alias.
testSetDiscrepancyReportBotAliasTarget_botId :: Lens.Lens' TestSetDiscrepancyReportBotAliasTarget Prelude.Text
testSetDiscrepancyReportBotAliasTarget_botId = Lens.lens (\TestSetDiscrepancyReportBotAliasTarget' {botId} -> botId) (\s@TestSetDiscrepancyReportBotAliasTarget' {} a -> s {botId = a} :: TestSetDiscrepancyReportBotAliasTarget)

-- | The unique identifier for the bot associated with the bot alias.
testSetDiscrepancyReportBotAliasTarget_botAliasId :: Lens.Lens' TestSetDiscrepancyReportBotAliasTarget Prelude.Text
testSetDiscrepancyReportBotAliasTarget_botAliasId = Lens.lens (\TestSetDiscrepancyReportBotAliasTarget' {botAliasId} -> botAliasId) (\s@TestSetDiscrepancyReportBotAliasTarget' {} a -> s {botAliasId = a} :: TestSetDiscrepancyReportBotAliasTarget)

-- | The unique identifier of the locale associated with the bot alias.
testSetDiscrepancyReportBotAliasTarget_localeId :: Lens.Lens' TestSetDiscrepancyReportBotAliasTarget Prelude.Text
testSetDiscrepancyReportBotAliasTarget_localeId = Lens.lens (\TestSetDiscrepancyReportBotAliasTarget' {localeId} -> localeId) (\s@TestSetDiscrepancyReportBotAliasTarget' {} a -> s {localeId = a} :: TestSetDiscrepancyReportBotAliasTarget)

instance
  Data.FromJSON
    TestSetDiscrepancyReportBotAliasTarget
  where
  parseJSON =
    Data.withObject
      "TestSetDiscrepancyReportBotAliasTarget"
      ( \x ->
          TestSetDiscrepancyReportBotAliasTarget'
            Prelude.<$> (x Data..: "botId")
            Prelude.<*> (x Data..: "botAliasId")
            Prelude.<*> (x Data..: "localeId")
      )

instance
  Prelude.Hashable
    TestSetDiscrepancyReportBotAliasTarget
  where
  hashWithSalt
    _salt
    TestSetDiscrepancyReportBotAliasTarget' {..} =
      _salt
        `Prelude.hashWithSalt` botId
        `Prelude.hashWithSalt` botAliasId
        `Prelude.hashWithSalt` localeId

instance
  Prelude.NFData
    TestSetDiscrepancyReportBotAliasTarget
  where
  rnf TestSetDiscrepancyReportBotAliasTarget' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf localeId

instance
  Data.ToJSON
    TestSetDiscrepancyReportBotAliasTarget
  where
  toJSON TestSetDiscrepancyReportBotAliasTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botId" Data..= botId),
            Prelude.Just ("botAliasId" Data..= botAliasId),
            Prelude.Just ("localeId" Data..= localeId)
          ]
      )

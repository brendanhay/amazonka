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
-- Module      : Amazonka.LexV2Models.Types.TestSetDiscrepancyReportResourceTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetDiscrepancyReportResourceTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestSetDiscrepancyReportBotAliasTarget
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the resource used for the test set
-- discrepancy report.
--
-- /See:/ 'newTestSetDiscrepancyReportResourceTarget' smart constructor.
data TestSetDiscrepancyReportResourceTarget = TestSetDiscrepancyReportResourceTarget'
  { -- | Contains information about the bot alias used as the resource for the
    -- test set discrepancy report.
    botAliasTarget :: Prelude.Maybe TestSetDiscrepancyReportBotAliasTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetDiscrepancyReportResourceTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasTarget', 'testSetDiscrepancyReportResourceTarget_botAliasTarget' - Contains information about the bot alias used as the resource for the
-- test set discrepancy report.
newTestSetDiscrepancyReportResourceTarget ::
  TestSetDiscrepancyReportResourceTarget
newTestSetDiscrepancyReportResourceTarget =
  TestSetDiscrepancyReportResourceTarget'
    { botAliasTarget =
        Prelude.Nothing
    }

-- | Contains information about the bot alias used as the resource for the
-- test set discrepancy report.
testSetDiscrepancyReportResourceTarget_botAliasTarget :: Lens.Lens' TestSetDiscrepancyReportResourceTarget (Prelude.Maybe TestSetDiscrepancyReportBotAliasTarget)
testSetDiscrepancyReportResourceTarget_botAliasTarget = Lens.lens (\TestSetDiscrepancyReportResourceTarget' {botAliasTarget} -> botAliasTarget) (\s@TestSetDiscrepancyReportResourceTarget' {} a -> s {botAliasTarget = a} :: TestSetDiscrepancyReportResourceTarget)

instance
  Data.FromJSON
    TestSetDiscrepancyReportResourceTarget
  where
  parseJSON =
    Data.withObject
      "TestSetDiscrepancyReportResourceTarget"
      ( \x ->
          TestSetDiscrepancyReportResourceTarget'
            Prelude.<$> (x Data..:? "botAliasTarget")
      )

instance
  Prelude.Hashable
    TestSetDiscrepancyReportResourceTarget
  where
  hashWithSalt
    _salt
    TestSetDiscrepancyReportResourceTarget' {..} =
      _salt `Prelude.hashWithSalt` botAliasTarget

instance
  Prelude.NFData
    TestSetDiscrepancyReportResourceTarget
  where
  rnf TestSetDiscrepancyReportResourceTarget' {..} =
    Prelude.rnf botAliasTarget

instance
  Data.ToJSON
    TestSetDiscrepancyReportResourceTarget
  where
  toJSON TestSetDiscrepancyReportResourceTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("botAliasTarget" Data..=)
              Prelude.<$> botAliasTarget
          ]
      )

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
-- Module      : Amazonka.Forecast.Types.TestWindowSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.TestWindowSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status, start time, and end time of a backtest, as well as a failure
-- reason if applicable.
--
-- /See:/ 'newTestWindowSummary' smart constructor.
data TestWindowSummary = TestWindowSummary'
  { -- | If the test failed, the reason why it failed.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the test. Possible status values are:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_IN_PROGRESS@
    --
    -- -   @CREATE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The time at which the test ended.
    testWindowEnd :: Prelude.Maybe Core.POSIX,
    -- | The time at which the test began.
    testWindowStart :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestWindowSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'testWindowSummary_message' - If the test failed, the reason why it failed.
--
-- 'status', 'testWindowSummary_status' - The status of the test. Possible status values are:
--
-- -   @ACTIVE@
--
-- -   @CREATE_IN_PROGRESS@
--
-- -   @CREATE_FAILED@
--
-- 'testWindowEnd', 'testWindowSummary_testWindowEnd' - The time at which the test ended.
--
-- 'testWindowStart', 'testWindowSummary_testWindowStart' - The time at which the test began.
newTestWindowSummary ::
  TestWindowSummary
newTestWindowSummary =
  TestWindowSummary'
    { message = Prelude.Nothing,
      status = Prelude.Nothing,
      testWindowEnd = Prelude.Nothing,
      testWindowStart = Prelude.Nothing
    }

-- | If the test failed, the reason why it failed.
testWindowSummary_message :: Lens.Lens' TestWindowSummary (Prelude.Maybe Prelude.Text)
testWindowSummary_message = Lens.lens (\TestWindowSummary' {message} -> message) (\s@TestWindowSummary' {} a -> s {message = a} :: TestWindowSummary)

-- | The status of the test. Possible status values are:
--
-- -   @ACTIVE@
--
-- -   @CREATE_IN_PROGRESS@
--
-- -   @CREATE_FAILED@
testWindowSummary_status :: Lens.Lens' TestWindowSummary (Prelude.Maybe Prelude.Text)
testWindowSummary_status = Lens.lens (\TestWindowSummary' {status} -> status) (\s@TestWindowSummary' {} a -> s {status = a} :: TestWindowSummary)

-- | The time at which the test ended.
testWindowSummary_testWindowEnd :: Lens.Lens' TestWindowSummary (Prelude.Maybe Prelude.UTCTime)
testWindowSummary_testWindowEnd = Lens.lens (\TestWindowSummary' {testWindowEnd} -> testWindowEnd) (\s@TestWindowSummary' {} a -> s {testWindowEnd = a} :: TestWindowSummary) Prelude.. Lens.mapping Core._Time

-- | The time at which the test began.
testWindowSummary_testWindowStart :: Lens.Lens' TestWindowSummary (Prelude.Maybe Prelude.UTCTime)
testWindowSummary_testWindowStart = Lens.lens (\TestWindowSummary' {testWindowStart} -> testWindowStart) (\s@TestWindowSummary' {} a -> s {testWindowStart = a} :: TestWindowSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON TestWindowSummary where
  parseJSON =
    Core.withObject
      "TestWindowSummary"
      ( \x ->
          TestWindowSummary'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "TestWindowEnd")
            Prelude.<*> (x Core..:? "TestWindowStart")
      )

instance Prelude.Hashable TestWindowSummary where
  hashWithSalt _salt TestWindowSummary' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` testWindowEnd
      `Prelude.hashWithSalt` testWindowStart

instance Prelude.NFData TestWindowSummary where
  rnf TestWindowSummary' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf testWindowEnd
      `Prelude.seq` Prelude.rnf testWindowStart

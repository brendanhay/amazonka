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
-- Module      : Network.AWS.Forecast.Types.TestWindowSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.TestWindowSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status, start time, and end time of a backtest, as well as a failure
-- reason if applicable.
--
-- /See:/ 'newTestWindowSummary' smart constructor.
data TestWindowSummary = TestWindowSummary'
  { -- | The status of the test. Possible status values are:
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
    testWindowStart :: Prelude.Maybe Core.POSIX,
    -- | If the test failed, the reason why it failed.
    message :: Prelude.Maybe Prelude.Text
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
--
-- 'message', 'testWindowSummary_message' - If the test failed, the reason why it failed.
newTestWindowSummary ::
  TestWindowSummary
newTestWindowSummary =
  TestWindowSummary'
    { status = Prelude.Nothing,
      testWindowEnd = Prelude.Nothing,
      testWindowStart = Prelude.Nothing,
      message = Prelude.Nothing
    }

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

-- | If the test failed, the reason why it failed.
testWindowSummary_message :: Lens.Lens' TestWindowSummary (Prelude.Maybe Prelude.Text)
testWindowSummary_message = Lens.lens (\TestWindowSummary' {message} -> message) (\s@TestWindowSummary' {} a -> s {message = a} :: TestWindowSummary)

instance Core.FromJSON TestWindowSummary where
  parseJSON =
    Core.withObject
      "TestWindowSummary"
      ( \x ->
          TestWindowSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "TestWindowEnd")
            Prelude.<*> (x Core..:? "TestWindowStart")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable TestWindowSummary

instance Prelude.NFData TestWindowSummary

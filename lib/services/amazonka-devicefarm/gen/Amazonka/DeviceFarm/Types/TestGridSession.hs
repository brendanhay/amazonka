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
-- Module      : Amazonka.DeviceFarm.Types.TestGridSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TestGridSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types.TestGridSessionStatus
import qualified Amazonka.Prelude as Prelude

-- | A TestGridSession is a single instance of a browser launched from the
-- URL provided by a call to CreateTestGridUrl.
--
-- /See:/ 'newTestGridSession' smart constructor.
data TestGridSession = TestGridSession'
  { -- | A JSON object of options and parameters passed to the Selenium
    -- WebDriver.
    seleniumProperties :: Prelude.Maybe Prelude.Text,
    -- | The time the session ended.
    ended :: Prelude.Maybe Core.POSIX,
    -- | The time that the session was started.
    created :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the session.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the session.
    status :: Prelude.Maybe TestGridSessionStatus,
    -- | The number of billed minutes that were used for this session.
    billingMinutes :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestGridSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'seleniumProperties', 'testGridSession_seleniumProperties' - A JSON object of options and parameters passed to the Selenium
-- WebDriver.
--
-- 'ended', 'testGridSession_ended' - The time the session ended.
--
-- 'created', 'testGridSession_created' - The time that the session was started.
--
-- 'arn', 'testGridSession_arn' - The ARN of the session.
--
-- 'status', 'testGridSession_status' - The state of the session.
--
-- 'billingMinutes', 'testGridSession_billingMinutes' - The number of billed minutes that were used for this session.
newTestGridSession ::
  TestGridSession
newTestGridSession =
  TestGridSession'
    { seleniumProperties =
        Prelude.Nothing,
      ended = Prelude.Nothing,
      created = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      billingMinutes = Prelude.Nothing
    }

-- | A JSON object of options and parameters passed to the Selenium
-- WebDriver.
testGridSession_seleniumProperties :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.Text)
testGridSession_seleniumProperties = Lens.lens (\TestGridSession' {seleniumProperties} -> seleniumProperties) (\s@TestGridSession' {} a -> s {seleniumProperties = a} :: TestGridSession)

-- | The time the session ended.
testGridSession_ended :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.UTCTime)
testGridSession_ended = Lens.lens (\TestGridSession' {ended} -> ended) (\s@TestGridSession' {} a -> s {ended = a} :: TestGridSession) Prelude.. Lens.mapping Core._Time

-- | The time that the session was started.
testGridSession_created :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.UTCTime)
testGridSession_created = Lens.lens (\TestGridSession' {created} -> created) (\s@TestGridSession' {} a -> s {created = a} :: TestGridSession) Prelude.. Lens.mapping Core._Time

-- | The ARN of the session.
testGridSession_arn :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.Text)
testGridSession_arn = Lens.lens (\TestGridSession' {arn} -> arn) (\s@TestGridSession' {} a -> s {arn = a} :: TestGridSession)

-- | The state of the session.
testGridSession_status :: Lens.Lens' TestGridSession (Prelude.Maybe TestGridSessionStatus)
testGridSession_status = Lens.lens (\TestGridSession' {status} -> status) (\s@TestGridSession' {} a -> s {status = a} :: TestGridSession)

-- | The number of billed minutes that were used for this session.
testGridSession_billingMinutes :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.Double)
testGridSession_billingMinutes = Lens.lens (\TestGridSession' {billingMinutes} -> billingMinutes) (\s@TestGridSession' {} a -> s {billingMinutes = a} :: TestGridSession)

instance Core.FromJSON TestGridSession where
  parseJSON =
    Core.withObject
      "TestGridSession"
      ( \x ->
          TestGridSession'
            Prelude.<$> (x Core..:? "seleniumProperties")
            Prelude.<*> (x Core..:? "ended")
            Prelude.<*> (x Core..:? "created")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "billingMinutes")
      )

instance Prelude.Hashable TestGridSession where
  hashWithSalt _salt TestGridSession' {..} =
    _salt `Prelude.hashWithSalt` seleniumProperties
      `Prelude.hashWithSalt` ended
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` billingMinutes

instance Prelude.NFData TestGridSession where
  rnf TestGridSession' {..} =
    Prelude.rnf seleniumProperties
      `Prelude.seq` Prelude.rnf ended
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf billingMinutes

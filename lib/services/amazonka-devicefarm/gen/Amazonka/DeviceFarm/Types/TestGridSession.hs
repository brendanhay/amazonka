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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TestGridSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.TestGridSessionStatus
import qualified Amazonka.Prelude as Prelude

-- | A TestGridSession is a single instance of a browser launched from the
-- URL provided by a call to CreateTestGridUrl.
--
-- /See:/ 'newTestGridSession' smart constructor.
data TestGridSession = TestGridSession'
  { -- | The ARN of the session.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of billed minutes that were used for this session.
    billingMinutes :: Prelude.Maybe Prelude.Double,
    -- | The time that the session was started.
    created :: Prelude.Maybe Data.POSIX,
    -- | The time the session ended.
    ended :: Prelude.Maybe Data.POSIX,
    -- | A JSON object of options and parameters passed to the Selenium
    -- WebDriver.
    seleniumProperties :: Prelude.Maybe Prelude.Text,
    -- | The state of the session.
    status :: Prelude.Maybe TestGridSessionStatus
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
-- 'arn', 'testGridSession_arn' - The ARN of the session.
--
-- 'billingMinutes', 'testGridSession_billingMinutes' - The number of billed minutes that were used for this session.
--
-- 'created', 'testGridSession_created' - The time that the session was started.
--
-- 'ended', 'testGridSession_ended' - The time the session ended.
--
-- 'seleniumProperties', 'testGridSession_seleniumProperties' - A JSON object of options and parameters passed to the Selenium
-- WebDriver.
--
-- 'status', 'testGridSession_status' - The state of the session.
newTestGridSession ::
  TestGridSession
newTestGridSession =
  TestGridSession'
    { arn = Prelude.Nothing,
      billingMinutes = Prelude.Nothing,
      created = Prelude.Nothing,
      ended = Prelude.Nothing,
      seleniumProperties = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ARN of the session.
testGridSession_arn :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.Text)
testGridSession_arn = Lens.lens (\TestGridSession' {arn} -> arn) (\s@TestGridSession' {} a -> s {arn = a} :: TestGridSession)

-- | The number of billed minutes that were used for this session.
testGridSession_billingMinutes :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.Double)
testGridSession_billingMinutes = Lens.lens (\TestGridSession' {billingMinutes} -> billingMinutes) (\s@TestGridSession' {} a -> s {billingMinutes = a} :: TestGridSession)

-- | The time that the session was started.
testGridSession_created :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.UTCTime)
testGridSession_created = Lens.lens (\TestGridSession' {created} -> created) (\s@TestGridSession' {} a -> s {created = a} :: TestGridSession) Prelude.. Lens.mapping Data._Time

-- | The time the session ended.
testGridSession_ended :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.UTCTime)
testGridSession_ended = Lens.lens (\TestGridSession' {ended} -> ended) (\s@TestGridSession' {} a -> s {ended = a} :: TestGridSession) Prelude.. Lens.mapping Data._Time

-- | A JSON object of options and parameters passed to the Selenium
-- WebDriver.
testGridSession_seleniumProperties :: Lens.Lens' TestGridSession (Prelude.Maybe Prelude.Text)
testGridSession_seleniumProperties = Lens.lens (\TestGridSession' {seleniumProperties} -> seleniumProperties) (\s@TestGridSession' {} a -> s {seleniumProperties = a} :: TestGridSession)

-- | The state of the session.
testGridSession_status :: Lens.Lens' TestGridSession (Prelude.Maybe TestGridSessionStatus)
testGridSession_status = Lens.lens (\TestGridSession' {status} -> status) (\s@TestGridSession' {} a -> s {status = a} :: TestGridSession)

instance Data.FromJSON TestGridSession where
  parseJSON =
    Data.withObject
      "TestGridSession"
      ( \x ->
          TestGridSession'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "billingMinutes")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "ended")
            Prelude.<*> (x Data..:? "seleniumProperties")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable TestGridSession where
  hashWithSalt _salt TestGridSession' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` billingMinutes
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` ended
      `Prelude.hashWithSalt` seleniumProperties
      `Prelude.hashWithSalt` status

instance Prelude.NFData TestGridSession where
  rnf TestGridSession' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf billingMinutes
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf ended
      `Prelude.seq` Prelude.rnf seleniumProperties
      `Prelude.seq` Prelude.rnf status

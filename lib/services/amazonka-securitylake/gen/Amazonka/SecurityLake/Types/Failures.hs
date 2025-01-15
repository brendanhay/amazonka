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
-- Module      : Amazonka.SecurityLake.Types.Failures
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.Failures where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of all failures.
--
-- /See:/ 'newFailures' smart constructor.
data Failures = Failures'
  { -- | List of all exception messages.
    exceptionMessage :: Prelude.Text,
    -- | List of all remediation steps for failures.
    remediation :: Prelude.Text,
    -- | This error can occur if you configure the wrong timestamp format, or if
    -- the subset of entries used for validation had errors or missing values.
    timestamp :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Failures' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionMessage', 'failures_exceptionMessage' - List of all exception messages.
--
-- 'remediation', 'failures_remediation' - List of all remediation steps for failures.
--
-- 'timestamp', 'failures_timestamp' - This error can occur if you configure the wrong timestamp format, or if
-- the subset of entries used for validation had errors or missing values.
newFailures ::
  -- | 'exceptionMessage'
  Prelude.Text ->
  -- | 'remediation'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  Failures
newFailures
  pExceptionMessage_
  pRemediation_
  pTimestamp_ =
    Failures'
      { exceptionMessage = pExceptionMessage_,
        remediation = pRemediation_,
        timestamp = Data._Time Lens.# pTimestamp_
      }

-- | List of all exception messages.
failures_exceptionMessage :: Lens.Lens' Failures Prelude.Text
failures_exceptionMessage = Lens.lens (\Failures' {exceptionMessage} -> exceptionMessage) (\s@Failures' {} a -> s {exceptionMessage = a} :: Failures)

-- | List of all remediation steps for failures.
failures_remediation :: Lens.Lens' Failures Prelude.Text
failures_remediation = Lens.lens (\Failures' {remediation} -> remediation) (\s@Failures' {} a -> s {remediation = a} :: Failures)

-- | This error can occur if you configure the wrong timestamp format, or if
-- the subset of entries used for validation had errors or missing values.
failures_timestamp :: Lens.Lens' Failures Prelude.UTCTime
failures_timestamp = Lens.lens (\Failures' {timestamp} -> timestamp) (\s@Failures' {} a -> s {timestamp = a} :: Failures) Prelude.. Data._Time

instance Data.FromJSON Failures where
  parseJSON =
    Data.withObject
      "Failures"
      ( \x ->
          Failures'
            Prelude.<$> (x Data..: "exceptionMessage")
            Prelude.<*> (x Data..: "remediation")
            Prelude.<*> (x Data..: "timestamp")
      )

instance Prelude.Hashable Failures where
  hashWithSalt _salt Failures' {..} =
    _salt
      `Prelude.hashWithSalt` exceptionMessage
      `Prelude.hashWithSalt` remediation
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData Failures where
  rnf Failures' {..} =
    Prelude.rnf exceptionMessage `Prelude.seq`
      Prelude.rnf remediation `Prelude.seq`
        Prelude.rnf timestamp

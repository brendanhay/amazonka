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
-- Module      : Amazonka.EMR.Types.FailureDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.FailureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the step failure. The service attempts to detect the root
-- cause for many common failures.
--
-- /See:/ 'newFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The path to the log file where the step failure root cause was
    -- originally recorded.
    logFile :: Prelude.Maybe Prelude.Text,
    -- | The descriptive message including the error the Amazon EMR service has
    -- identified as the cause of step failure. This is text from an error log
    -- that describes the root cause of the failure.
    message :: Prelude.Maybe Prelude.Text,
    -- | The reason for the step failure. In the case where the service cannot
    -- successfully determine the root cause of the failure, it returns
    -- \"Unknown Error\" as a reason.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logFile', 'failureDetails_logFile' - The path to the log file where the step failure root cause was
-- originally recorded.
--
-- 'message', 'failureDetails_message' - The descriptive message including the error the Amazon EMR service has
-- identified as the cause of step failure. This is text from an error log
-- that describes the root cause of the failure.
--
-- 'reason', 'failureDetails_reason' - The reason for the step failure. In the case where the service cannot
-- successfully determine the root cause of the failure, it returns
-- \"Unknown Error\" as a reason.
newFailureDetails ::
  FailureDetails
newFailureDetails =
  FailureDetails'
    { logFile = Prelude.Nothing,
      message = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The path to the log file where the step failure root cause was
-- originally recorded.
failureDetails_logFile :: Lens.Lens' FailureDetails (Prelude.Maybe Prelude.Text)
failureDetails_logFile = Lens.lens (\FailureDetails' {logFile} -> logFile) (\s@FailureDetails' {} a -> s {logFile = a} :: FailureDetails)

-- | The descriptive message including the error the Amazon EMR service has
-- identified as the cause of step failure. This is text from an error log
-- that describes the root cause of the failure.
failureDetails_message :: Lens.Lens' FailureDetails (Prelude.Maybe Prelude.Text)
failureDetails_message = Lens.lens (\FailureDetails' {message} -> message) (\s@FailureDetails' {} a -> s {message = a} :: FailureDetails)

-- | The reason for the step failure. In the case where the service cannot
-- successfully determine the root cause of the failure, it returns
-- \"Unknown Error\" as a reason.
failureDetails_reason :: Lens.Lens' FailureDetails (Prelude.Maybe Prelude.Text)
failureDetails_reason = Lens.lens (\FailureDetails' {reason} -> reason) (\s@FailureDetails' {} a -> s {reason = a} :: FailureDetails)

instance Data.FromJSON FailureDetails where
  parseJSON =
    Data.withObject
      "FailureDetails"
      ( \x ->
          FailureDetails'
            Prelude.<$> (x Data..:? "LogFile")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Reason")
      )

instance Prelude.Hashable FailureDetails where
  hashWithSalt _salt FailureDetails' {..} =
    _salt
      `Prelude.hashWithSalt` logFile
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` reason

instance Prelude.NFData FailureDetails where
  rnf FailureDetails' {..} =
    Prelude.rnf logFile
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf reason

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
-- Module      : Network.AWS.EMR.Types.FailureDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.FailureDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details of the step failure. The service attempts to detect the root
-- cause for many common failures.
--
-- /See:/ 'newFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The descriptive message including the error the Amazon EMR service has
    -- identified as the cause of step failure. This is text from an error log
    -- that describes the root cause of the failure.
    message :: Core.Maybe Core.Text,
    -- | The reason for the step failure. In the case where the service cannot
    -- successfully determine the root cause of the failure, it returns
    -- \"Unknown Error\" as a reason.
    reason :: Core.Maybe Core.Text,
    -- | The path to the log file where the step failure root cause was
    -- originally recorded.
    logFile :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'failureDetails_message' - The descriptive message including the error the Amazon EMR service has
-- identified as the cause of step failure. This is text from an error log
-- that describes the root cause of the failure.
--
-- 'reason', 'failureDetails_reason' - The reason for the step failure. In the case where the service cannot
-- successfully determine the root cause of the failure, it returns
-- \"Unknown Error\" as a reason.
--
-- 'logFile', 'failureDetails_logFile' - The path to the log file where the step failure root cause was
-- originally recorded.
newFailureDetails ::
  FailureDetails
newFailureDetails =
  FailureDetails'
    { message = Core.Nothing,
      reason = Core.Nothing,
      logFile = Core.Nothing
    }

-- | The descriptive message including the error the Amazon EMR service has
-- identified as the cause of step failure. This is text from an error log
-- that describes the root cause of the failure.
failureDetails_message :: Lens.Lens' FailureDetails (Core.Maybe Core.Text)
failureDetails_message = Lens.lens (\FailureDetails' {message} -> message) (\s@FailureDetails' {} a -> s {message = a} :: FailureDetails)

-- | The reason for the step failure. In the case where the service cannot
-- successfully determine the root cause of the failure, it returns
-- \"Unknown Error\" as a reason.
failureDetails_reason :: Lens.Lens' FailureDetails (Core.Maybe Core.Text)
failureDetails_reason = Lens.lens (\FailureDetails' {reason} -> reason) (\s@FailureDetails' {} a -> s {reason = a} :: FailureDetails)

-- | The path to the log file where the step failure root cause was
-- originally recorded.
failureDetails_logFile :: Lens.Lens' FailureDetails (Core.Maybe Core.Text)
failureDetails_logFile = Lens.lens (\FailureDetails' {logFile} -> logFile) (\s@FailureDetails' {} a -> s {logFile = a} :: FailureDetails)

instance Core.FromJSON FailureDetails where
  parseJSON =
    Core.withObject
      "FailureDetails"
      ( \x ->
          FailureDetails'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "Reason")
            Core.<*> (x Core..:? "LogFile")
      )

instance Core.Hashable FailureDetails

instance Core.NFData FailureDetails

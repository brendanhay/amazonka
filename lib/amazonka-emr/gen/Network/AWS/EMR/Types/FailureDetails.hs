{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.FailureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.FailureDetails
  ( FailureDetails (..),

    -- * Smart constructor
    mkFailureDetails,

    -- * Lenses
    fdLogFile,
    fdMessage,
    fdReason,
  )
where

import qualified Network.AWS.EMR.Types.LogFile as Types
import qualified Network.AWS.EMR.Types.Message as Types
import qualified Network.AWS.EMR.Types.Reason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the step failure. The service attempts to detect the root cause for many common failures.
--
-- /See:/ 'mkFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The path to the log file where the step failure root cause was originally recorded.
    logFile :: Core.Maybe Types.LogFile,
    -- | The descriptive message including the error the Amazon EMR service has identified as the cause of step failure. This is text from an error log that describes the root cause of the failure.
    message :: Core.Maybe Types.Message,
    -- | The reason for the step failure. In the case where the service cannot successfully determine the root cause of the failure, it returns "Unknown Error" as a reason.
    reason :: Core.Maybe Types.Reason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailureDetails' value with any optional fields omitted.
mkFailureDetails ::
  FailureDetails
mkFailureDetails =
  FailureDetails'
    { logFile = Core.Nothing,
      message = Core.Nothing,
      reason = Core.Nothing
    }

-- | The path to the log file where the step failure root cause was originally recorded.
--
-- /Note:/ Consider using 'logFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdLogFile :: Lens.Lens' FailureDetails (Core.Maybe Types.LogFile)
fdLogFile = Lens.field @"logFile"
{-# DEPRECATED fdLogFile "Use generic-lens or generic-optics with 'logFile' instead." #-}

-- | The descriptive message including the error the Amazon EMR service has identified as the cause of step failure. This is text from an error log that describes the root cause of the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMessage :: Lens.Lens' FailureDetails (Core.Maybe Types.Message)
fdMessage = Lens.field @"message"
{-# DEPRECATED fdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The reason for the step failure. In the case where the service cannot successfully determine the root cause of the failure, it returns "Unknown Error" as a reason.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdReason :: Lens.Lens' FailureDetails (Core.Maybe Types.Reason)
fdReason = Lens.field @"reason"
{-# DEPRECATED fdReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON FailureDetails where
  parseJSON =
    Core.withObject "FailureDetails" Core.$
      \x ->
        FailureDetails'
          Core.<$> (x Core..:? "LogFile")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "Reason")

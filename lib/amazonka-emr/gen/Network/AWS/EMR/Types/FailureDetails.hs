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
    fdReason,
    fdMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the step failure. The service attempts to detect the root cause for many common failures.
--
-- /See:/ 'mkFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { logFile ::
      Lude.Maybe Lude.Text,
    reason :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailureDetails' with the minimum fields required to make a request.
--
-- * 'logFile' - The path to the log file where the step failure root cause was originally recorded.
-- * 'message' - The descriptive message including the error the Amazon EMR service has identified as the cause of step failure. This is text from an error log that describes the root cause of the failure.
-- * 'reason' - The reason for the step failure. In the case where the service cannot successfully determine the root cause of the failure, it returns "Unknown Error" as a reason.
mkFailureDetails ::
  FailureDetails
mkFailureDetails =
  FailureDetails'
    { logFile = Lude.Nothing,
      reason = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The path to the log file where the step failure root cause was originally recorded.
--
-- /Note:/ Consider using 'logFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdLogFile :: Lens.Lens' FailureDetails (Lude.Maybe Lude.Text)
fdLogFile = Lens.lens (logFile :: FailureDetails -> Lude.Maybe Lude.Text) (\s a -> s {logFile = a} :: FailureDetails)
{-# DEPRECATED fdLogFile "Use generic-lens or generic-optics with 'logFile' instead." #-}

-- | The reason for the step failure. In the case where the service cannot successfully determine the root cause of the failure, it returns "Unknown Error" as a reason.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdReason :: Lens.Lens' FailureDetails (Lude.Maybe Lude.Text)
fdReason = Lens.lens (reason :: FailureDetails -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: FailureDetails)
{-# DEPRECATED fdReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The descriptive message including the error the Amazon EMR service has identified as the cause of step failure. This is text from an error log that describes the root cause of the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMessage :: Lens.Lens' FailureDetails (Lude.Maybe Lude.Text)
fdMessage = Lens.lens (message :: FailureDetails -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: FailureDetails)
{-# DEPRECATED fdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON FailureDetails where
  parseJSON =
    Lude.withObject
      "FailureDetails"
      ( \x ->
          FailureDetails'
            Lude.<$> (x Lude..:? "LogFile")
            Lude.<*> (x Lude..:? "Reason")
            Lude.<*> (x Lude..:? "Message")
      )

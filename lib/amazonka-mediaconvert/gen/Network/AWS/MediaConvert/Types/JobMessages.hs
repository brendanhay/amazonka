{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobMessages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobMessages
  ( JobMessages (..),

    -- * Smart constructor
    mkJobMessages,

    -- * Lenses
    jmInfo,
    jmWarning,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides messages from the service about jobs that you have already successfully submitted.
--
-- /See:/ 'mkJobMessages' smart constructor.
data JobMessages = JobMessages'
  { -- | List of messages that are informational only and don't indicate a problem with your job.
    info :: Core.Maybe [Core.Text],
    -- | List of messages that warn about conditions that might cause your job not to run or to fail.
    warning :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobMessages' value with any optional fields omitted.
mkJobMessages ::
  JobMessages
mkJobMessages =
  JobMessages' {info = Core.Nothing, warning = Core.Nothing}

-- | List of messages that are informational only and don't indicate a problem with your job.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmInfo :: Lens.Lens' JobMessages (Core.Maybe [Core.Text])
jmInfo = Lens.field @"info"
{-# DEPRECATED jmInfo "Use generic-lens or generic-optics with 'info' instead." #-}

-- | List of messages that warn about conditions that might cause your job not to run or to fail.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmWarning :: Lens.Lens' JobMessages (Core.Maybe [Core.Text])
jmWarning = Lens.field @"warning"
{-# DEPRECATED jmWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

instance Core.FromJSON JobMessages where
  parseJSON =
    Core.withObject "JobMessages" Core.$
      \x ->
        JobMessages'
          Core.<$> (x Core..:? "info") Core.<*> (x Core..:? "warning")

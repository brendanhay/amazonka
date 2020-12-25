{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LastCrawlInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LastCrawlInfo
  ( LastCrawlInfo (..),

    -- * Smart constructor
    mkLastCrawlInfo,

    -- * Lenses
    lciErrorMessage,
    lciLogGroup,
    lciLogStream,
    lciMessagePrefix,
    lciStartTime,
    lciStatus,
  )
where

import qualified Network.AWS.Glue.Types.ErrorMessage as Types
import qualified Network.AWS.Glue.Types.LastCrawlStatus as Types
import qualified Network.AWS.Glue.Types.LogGroup as Types
import qualified Network.AWS.Glue.Types.LogStream as Types
import qualified Network.AWS.Glue.Types.MessagePrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status and error information about the most recent crawl.
--
-- /See:/ 'mkLastCrawlInfo' smart constructor.
data LastCrawlInfo = LastCrawlInfo'
  { -- | If an error occurred, the error information about the last crawl.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The log group for the last crawl.
    logGroup :: Core.Maybe Types.LogGroup,
    -- | The log stream for the last crawl.
    logStream :: Core.Maybe Types.LogStream,
    -- | The prefix for a message about this crawl.
    messagePrefix :: Core.Maybe Types.MessagePrefix,
    -- | The time at which the crawl started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | Status of the last crawl.
    status :: Core.Maybe Types.LastCrawlStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LastCrawlInfo' value with any optional fields omitted.
mkLastCrawlInfo ::
  LastCrawlInfo
mkLastCrawlInfo =
  LastCrawlInfo'
    { errorMessage = Core.Nothing,
      logGroup = Core.Nothing,
      logStream = Core.Nothing,
      messagePrefix = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing
    }

-- | If an error occurred, the error information about the last crawl.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciErrorMessage :: Lens.Lens' LastCrawlInfo (Core.Maybe Types.ErrorMessage)
lciErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED lciErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The log group for the last crawl.
--
-- /Note:/ Consider using 'logGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciLogGroup :: Lens.Lens' LastCrawlInfo (Core.Maybe Types.LogGroup)
lciLogGroup = Lens.field @"logGroup"
{-# DEPRECATED lciLogGroup "Use generic-lens or generic-optics with 'logGroup' instead." #-}

-- | The log stream for the last crawl.
--
-- /Note:/ Consider using 'logStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciLogStream :: Lens.Lens' LastCrawlInfo (Core.Maybe Types.LogStream)
lciLogStream = Lens.field @"logStream"
{-# DEPRECATED lciLogStream "Use generic-lens or generic-optics with 'logStream' instead." #-}

-- | The prefix for a message about this crawl.
--
-- /Note:/ Consider using 'messagePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMessagePrefix :: Lens.Lens' LastCrawlInfo (Core.Maybe Types.MessagePrefix)
lciMessagePrefix = Lens.field @"messagePrefix"
{-# DEPRECATED lciMessagePrefix "Use generic-lens or generic-optics with 'messagePrefix' instead." #-}

-- | The time at which the crawl started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciStartTime :: Lens.Lens' LastCrawlInfo (Core.Maybe Core.NominalDiffTime)
lciStartTime = Lens.field @"startTime"
{-# DEPRECATED lciStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Status of the last crawl.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciStatus :: Lens.Lens' LastCrawlInfo (Core.Maybe Types.LastCrawlStatus)
lciStatus = Lens.field @"status"
{-# DEPRECATED lciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON LastCrawlInfo where
  parseJSON =
    Core.withObject "LastCrawlInfo" Core.$
      \x ->
        LastCrawlInfo'
          Core.<$> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "LogGroup")
          Core.<*> (x Core..:? "LogStream")
          Core.<*> (x Core..:? "MessagePrefix")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Status")

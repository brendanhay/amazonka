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
    lciStatus,
    lciStartTime,
    lciLogStream,
    lciLogGroup,
    lciMessagePrefix,
    lciErrorMessage,
  )
where

import Network.AWS.Glue.Types.LastCrawlStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status and error information about the most recent crawl.
--
-- /See:/ 'mkLastCrawlInfo' smart constructor.
data LastCrawlInfo = LastCrawlInfo'
  { -- | Status of the last crawl.
    status :: Lude.Maybe LastCrawlStatus,
    -- | The time at which the crawl started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The log stream for the last crawl.
    logStream :: Lude.Maybe Lude.Text,
    -- | The log group for the last crawl.
    logGroup :: Lude.Maybe Lude.Text,
    -- | The prefix for a message about this crawl.
    messagePrefix :: Lude.Maybe Lude.Text,
    -- | If an error occurred, the error information about the last crawl.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LastCrawlInfo' with the minimum fields required to make a request.
--
-- * 'status' - Status of the last crawl.
-- * 'startTime' - The time at which the crawl started.
-- * 'logStream' - The log stream for the last crawl.
-- * 'logGroup' - The log group for the last crawl.
-- * 'messagePrefix' - The prefix for a message about this crawl.
-- * 'errorMessage' - If an error occurred, the error information about the last crawl.
mkLastCrawlInfo ::
  LastCrawlInfo
mkLastCrawlInfo =
  LastCrawlInfo'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      logStream = Lude.Nothing,
      logGroup = Lude.Nothing,
      messagePrefix = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Status of the last crawl.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciStatus :: Lens.Lens' LastCrawlInfo (Lude.Maybe LastCrawlStatus)
lciStatus = Lens.lens (status :: LastCrawlInfo -> Lude.Maybe LastCrawlStatus) (\s a -> s {status = a} :: LastCrawlInfo)
{-# DEPRECATED lciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time at which the crawl started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciStartTime :: Lens.Lens' LastCrawlInfo (Lude.Maybe Lude.Timestamp)
lciStartTime = Lens.lens (startTime :: LastCrawlInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: LastCrawlInfo)
{-# DEPRECATED lciStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The log stream for the last crawl.
--
-- /Note:/ Consider using 'logStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciLogStream :: Lens.Lens' LastCrawlInfo (Lude.Maybe Lude.Text)
lciLogStream = Lens.lens (logStream :: LastCrawlInfo -> Lude.Maybe Lude.Text) (\s a -> s {logStream = a} :: LastCrawlInfo)
{-# DEPRECATED lciLogStream "Use generic-lens or generic-optics with 'logStream' instead." #-}

-- | The log group for the last crawl.
--
-- /Note:/ Consider using 'logGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciLogGroup :: Lens.Lens' LastCrawlInfo (Lude.Maybe Lude.Text)
lciLogGroup = Lens.lens (logGroup :: LastCrawlInfo -> Lude.Maybe Lude.Text) (\s a -> s {logGroup = a} :: LastCrawlInfo)
{-# DEPRECATED lciLogGroup "Use generic-lens or generic-optics with 'logGroup' instead." #-}

-- | The prefix for a message about this crawl.
--
-- /Note:/ Consider using 'messagePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMessagePrefix :: Lens.Lens' LastCrawlInfo (Lude.Maybe Lude.Text)
lciMessagePrefix = Lens.lens (messagePrefix :: LastCrawlInfo -> Lude.Maybe Lude.Text) (\s a -> s {messagePrefix = a} :: LastCrawlInfo)
{-# DEPRECATED lciMessagePrefix "Use generic-lens or generic-optics with 'messagePrefix' instead." #-}

-- | If an error occurred, the error information about the last crawl.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciErrorMessage :: Lens.Lens' LastCrawlInfo (Lude.Maybe Lude.Text)
lciErrorMessage = Lens.lens (errorMessage :: LastCrawlInfo -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: LastCrawlInfo)
{-# DEPRECATED lciErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON LastCrawlInfo where
  parseJSON =
    Lude.withObject
      "LastCrawlInfo"
      ( \x ->
          LastCrawlInfo'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "LogStream")
            Lude.<*> (x Lude..:? "LogGroup")
            Lude.<*> (x Lude..:? "MessagePrefix")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )

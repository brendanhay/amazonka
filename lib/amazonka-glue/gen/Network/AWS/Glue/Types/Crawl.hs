{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Crawl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Crawl
  ( Crawl (..),

    -- * Smart constructor
    mkCrawl,

    -- * Lenses
    cfCompletedOn,
    cfState,
    cfStartedOn,
    cfLogStream,
    cfLogGroup,
    cfErrorMessage,
  )
where

import Network.AWS.Glue.Types.CrawlState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a crawl in the workflow.
--
-- /See:/ 'mkCrawl' smart constructor.
data Crawl = Crawl'
  { -- | The date and time on which the crawl completed.
    completedOn :: Lude.Maybe Lude.Timestamp,
    -- | The state of the crawler.
    state :: Lude.Maybe CrawlState,
    -- | The date and time on which the crawl started.
    startedOn :: Lude.Maybe Lude.Timestamp,
    -- | The log stream associated with the crawl.
    logStream :: Lude.Maybe Lude.Text,
    -- | The log group associated with the crawl.
    logGroup :: Lude.Maybe Lude.Text,
    -- | The error message associated with the crawl.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Crawl' with the minimum fields required to make a request.
--
-- * 'completedOn' - The date and time on which the crawl completed.
-- * 'state' - The state of the crawler.
-- * 'startedOn' - The date and time on which the crawl started.
-- * 'logStream' - The log stream associated with the crawl.
-- * 'logGroup' - The log group associated with the crawl.
-- * 'errorMessage' - The error message associated with the crawl.
mkCrawl ::
  Crawl
mkCrawl =
  Crawl'
    { completedOn = Lude.Nothing,
      state = Lude.Nothing,
      startedOn = Lude.Nothing,
      logStream = Lude.Nothing,
      logGroup = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The date and time on which the crawl completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCompletedOn :: Lens.Lens' Crawl (Lude.Maybe Lude.Timestamp)
cfCompletedOn = Lens.lens (completedOn :: Crawl -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedOn = a} :: Crawl)
{-# DEPRECATED cfCompletedOn "Use generic-lens or generic-optics with 'completedOn' instead." #-}

-- | The state of the crawler.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfState :: Lens.Lens' Crawl (Lude.Maybe CrawlState)
cfState = Lens.lens (state :: Crawl -> Lude.Maybe CrawlState) (\s a -> s {state = a} :: Crawl)
{-# DEPRECATED cfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date and time on which the crawl started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStartedOn :: Lens.Lens' Crawl (Lude.Maybe Lude.Timestamp)
cfStartedOn = Lens.lens (startedOn :: Crawl -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedOn = a} :: Crawl)
{-# DEPRECATED cfStartedOn "Use generic-lens or generic-optics with 'startedOn' instead." #-}

-- | The log stream associated with the crawl.
--
-- /Note:/ Consider using 'logStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogStream :: Lens.Lens' Crawl (Lude.Maybe Lude.Text)
cfLogStream = Lens.lens (logStream :: Crawl -> Lude.Maybe Lude.Text) (\s a -> s {logStream = a} :: Crawl)
{-# DEPRECATED cfLogStream "Use generic-lens or generic-optics with 'logStream' instead." #-}

-- | The log group associated with the crawl.
--
-- /Note:/ Consider using 'logGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogGroup :: Lens.Lens' Crawl (Lude.Maybe Lude.Text)
cfLogGroup = Lens.lens (logGroup :: Crawl -> Lude.Maybe Lude.Text) (\s a -> s {logGroup = a} :: Crawl)
{-# DEPRECATED cfLogGroup "Use generic-lens or generic-optics with 'logGroup' instead." #-}

-- | The error message associated with the crawl.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfErrorMessage :: Lens.Lens' Crawl (Lude.Maybe Lude.Text)
cfErrorMessage = Lens.lens (errorMessage :: Crawl -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: Crawl)
{-# DEPRECATED cfErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON Crawl where
  parseJSON =
    Lude.withObject
      "Crawl"
      ( \x ->
          Crawl'
            Lude.<$> (x Lude..:? "CompletedOn")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StartedOn")
            Lude.<*> (x Lude..:? "LogStream")
            Lude.<*> (x Lude..:? "LogGroup")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )

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
    craCompletedOn,
    craState,
    craStartedOn,
    craLogStream,
    craLogGroup,
    craErrorMessage,
  )
where

import Network.AWS.Glue.Types.CrawlState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a crawl in the workflow.
--
-- /See:/ 'mkCrawl' smart constructor.
data Crawl = Crawl'
  { completedOn :: Lude.Maybe Lude.Timestamp,
    state :: Lude.Maybe CrawlState,
    startedOn :: Lude.Maybe Lude.Timestamp,
    logStream :: Lude.Maybe Lude.Text,
    logGroup :: Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Crawl' with the minimum fields required to make a request.
--
-- * 'completedOn' - The date and time on which the crawl completed.
-- * 'errorMessage' - The error message associated with the crawl.
-- * 'logGroup' - The log group associated with the crawl.
-- * 'logStream' - The log stream associated with the crawl.
-- * 'startedOn' - The date and time on which the crawl started.
-- * 'state' - The state of the crawler.
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
craCompletedOn :: Lens.Lens' Crawl (Lude.Maybe Lude.Timestamp)
craCompletedOn = Lens.lens (completedOn :: Crawl -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedOn = a} :: Crawl)
{-# DEPRECATED craCompletedOn "Use generic-lens or generic-optics with 'completedOn' instead." #-}

-- | The state of the crawler.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craState :: Lens.Lens' Crawl (Lude.Maybe CrawlState)
craState = Lens.lens (state :: Crawl -> Lude.Maybe CrawlState) (\s a -> s {state = a} :: Crawl)
{-# DEPRECATED craState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date and time on which the crawl started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craStartedOn :: Lens.Lens' Crawl (Lude.Maybe Lude.Timestamp)
craStartedOn = Lens.lens (startedOn :: Crawl -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedOn = a} :: Crawl)
{-# DEPRECATED craStartedOn "Use generic-lens or generic-optics with 'startedOn' instead." #-}

-- | The log stream associated with the crawl.
--
-- /Note:/ Consider using 'logStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craLogStream :: Lens.Lens' Crawl (Lude.Maybe Lude.Text)
craLogStream = Lens.lens (logStream :: Crawl -> Lude.Maybe Lude.Text) (\s a -> s {logStream = a} :: Crawl)
{-# DEPRECATED craLogStream "Use generic-lens or generic-optics with 'logStream' instead." #-}

-- | The log group associated with the crawl.
--
-- /Note:/ Consider using 'logGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craLogGroup :: Lens.Lens' Crawl (Lude.Maybe Lude.Text)
craLogGroup = Lens.lens (logGroup :: Crawl -> Lude.Maybe Lude.Text) (\s a -> s {logGroup = a} :: Crawl)
{-# DEPRECATED craLogGroup "Use generic-lens or generic-optics with 'logGroup' instead." #-}

-- | The error message associated with the crawl.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craErrorMessage :: Lens.Lens' Crawl (Lude.Maybe Lude.Text)
craErrorMessage = Lens.lens (errorMessage :: Crawl -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: Crawl)
{-# DEPRECATED craErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

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

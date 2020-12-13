{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.SearchedLogStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.SearchedLogStream
  ( SearchedLogStream (..),

    -- * Smart constructor
    mkSearchedLogStream,

    -- * Lenses
    slsLogStreamName,
    slsSearchedCompletely,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the search status of a log stream.
--
-- /See:/ 'mkSearchedLogStream' smart constructor.
data SearchedLogStream = SearchedLogStream'
  { -- | The name of the log stream.
    logStreamName :: Lude.Maybe Lude.Text,
    -- | Indicates whether all the events in this log stream were searched.
    searchedCompletely :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchedLogStream' with the minimum fields required to make a request.
--
-- * 'logStreamName' - The name of the log stream.
-- * 'searchedCompletely' - Indicates whether all the events in this log stream were searched.
mkSearchedLogStream ::
  SearchedLogStream
mkSearchedLogStream =
  SearchedLogStream'
    { logStreamName = Lude.Nothing,
      searchedCompletely = Lude.Nothing
    }

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slsLogStreamName :: Lens.Lens' SearchedLogStream (Lude.Maybe Lude.Text)
slsLogStreamName = Lens.lens (logStreamName :: SearchedLogStream -> Lude.Maybe Lude.Text) (\s a -> s {logStreamName = a} :: SearchedLogStream)
{-# DEPRECATED slsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | Indicates whether all the events in this log stream were searched.
--
-- /Note:/ Consider using 'searchedCompletely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slsSearchedCompletely :: Lens.Lens' SearchedLogStream (Lude.Maybe Lude.Bool)
slsSearchedCompletely = Lens.lens (searchedCompletely :: SearchedLogStream -> Lude.Maybe Lude.Bool) (\s a -> s {searchedCompletely = a} :: SearchedLogStream)
{-# DEPRECATED slsSearchedCompletely "Use generic-lens or generic-optics with 'searchedCompletely' instead." #-}

instance Lude.FromJSON SearchedLogStream where
  parseJSON =
    Lude.withObject
      "SearchedLogStream"
      ( \x ->
          SearchedLogStream'
            Lude.<$> (x Lude..:? "logStreamName")
            Lude.<*> (x Lude..:? "searchedCompletely")
      )

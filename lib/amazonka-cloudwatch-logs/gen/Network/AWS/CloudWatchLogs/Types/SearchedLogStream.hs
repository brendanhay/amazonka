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

import qualified Network.AWS.CloudWatchLogs.Types.LogStreamName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the search status of a log stream.
--
-- /See:/ 'mkSearchedLogStream' smart constructor.
data SearchedLogStream = SearchedLogStream'
  { -- | The name of the log stream.
    logStreamName :: Core.Maybe Types.LogStreamName,
    -- | Indicates whether all the events in this log stream were searched.
    searchedCompletely :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchedLogStream' value with any optional fields omitted.
mkSearchedLogStream ::
  SearchedLogStream
mkSearchedLogStream =
  SearchedLogStream'
    { logStreamName = Core.Nothing,
      searchedCompletely = Core.Nothing
    }

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slsLogStreamName :: Lens.Lens' SearchedLogStream (Core.Maybe Types.LogStreamName)
slsLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED slsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | Indicates whether all the events in this log stream were searched.
--
-- /Note:/ Consider using 'searchedCompletely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slsSearchedCompletely :: Lens.Lens' SearchedLogStream (Core.Maybe Core.Bool)
slsSearchedCompletely = Lens.field @"searchedCompletely"
{-# DEPRECATED slsSearchedCompletely "Use generic-lens or generic-optics with 'searchedCompletely' instead." #-}

instance Core.FromJSON SearchedLogStream where
  parseJSON =
    Core.withObject "SearchedLogStream" Core.$
      \x ->
        SearchedLogStream'
          Core.<$> (x Core..:? "logStreamName")
          Core.<*> (x Core..:? "searchedCompletely")

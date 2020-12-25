{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SearchStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SearchStatus
  ( SearchStatus (..),

    -- * Smart constructor
    mkSearchStatus,

    -- * Lenses
    sRid,
    sTimems,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.Rid as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
-- /See:/ 'mkSearchStatus' smart constructor.
data SearchStatus = SearchStatus'
  { -- | The encrypted resource ID for the request.
    rid :: Core.Maybe Types.Rid,
    -- | How long it took to process the request, in milliseconds.
    timems :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchStatus' value with any optional fields omitted.
mkSearchStatus ::
  SearchStatus
mkSearchStatus =
  SearchStatus' {rid = Core.Nothing, timems = Core.Nothing}

-- | The encrypted resource ID for the request.
--
-- /Note:/ Consider using 'rid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRid :: Lens.Lens' SearchStatus (Core.Maybe Types.Rid)
sRid = Lens.field @"rid"
{-# DEPRECATED sRid "Use generic-lens or generic-optics with 'rid' instead." #-}

-- | How long it took to process the request, in milliseconds.
--
-- /Note:/ Consider using 'timems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimems :: Lens.Lens' SearchStatus (Core.Maybe Core.Integer)
sTimems = Lens.field @"timems"
{-# DEPRECATED sTimems "Use generic-lens or generic-optics with 'timems' instead." #-}

instance Core.FromJSON SearchStatus where
  parseJSON =
    Core.withObject "SearchStatus" Core.$
      \x ->
        SearchStatus'
          Core.<$> (x Core..:? "rid") Core.<*> (x Core..:? "timems")

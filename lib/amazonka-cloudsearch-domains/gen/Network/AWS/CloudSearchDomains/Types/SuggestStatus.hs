{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestStatus
  ( SuggestStatus (..),

    -- * Smart constructor
    mkSuggestStatus,

    -- * Lenses
    ssRid,
    ssTimems,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
-- /See:/ 'mkSuggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
  { -- | The encrypted resource ID for the request.
    rid :: Core.Maybe Types.String,
    -- | How long it took to process the request, in milliseconds.
    timems :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuggestStatus' value with any optional fields omitted.
mkSuggestStatus ::
  SuggestStatus
mkSuggestStatus =
  SuggestStatus' {rid = Core.Nothing, timems = Core.Nothing}

-- | The encrypted resource ID for the request.
--
-- /Note:/ Consider using 'rid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRid :: Lens.Lens' SuggestStatus (Core.Maybe Types.String)
ssRid = Lens.field @"rid"
{-# DEPRECATED ssRid "Use generic-lens or generic-optics with 'rid' instead." #-}

-- | How long it took to process the request, in milliseconds.
--
-- /Note:/ Consider using 'timems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTimems :: Lens.Lens' SuggestStatus (Core.Maybe Core.Integer)
ssTimems = Lens.field @"timems"
{-# DEPRECATED ssTimems "Use generic-lens or generic-optics with 'timems' instead." #-}

instance Core.FromJSON SuggestStatus where
  parseJSON =
    Core.withObject "SuggestStatus" Core.$
      \x ->
        SuggestStatus'
          Core.<$> (x Core..:? "rid") Core.<*> (x Core..:? "timems")

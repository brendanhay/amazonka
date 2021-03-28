{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.SuggestStatus
  ( SuggestStatus (..)
  -- * Smart constructor
  , mkSuggestStatus
  -- * Lenses
  , ssRid
  , ssTimems
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
-- /See:/ 'mkSuggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
  { rid :: Core.Maybe Core.Text
    -- ^ The encrypted resource ID for the request.
  , timems :: Core.Maybe Core.Integer
    -- ^ How long it took to process the request, in milliseconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuggestStatus' value with any optional fields omitted.
mkSuggestStatus
    :: SuggestStatus
mkSuggestStatus
  = SuggestStatus'{rid = Core.Nothing, timems = Core.Nothing}

-- | The encrypted resource ID for the request.
--
-- /Note:/ Consider using 'rid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRid :: Lens.Lens' SuggestStatus (Core.Maybe Core.Text)
ssRid = Lens.field @"rid"
{-# INLINEABLE ssRid #-}
{-# DEPRECATED rid "Use generic-lens or generic-optics with 'rid' instead"  #-}

-- | How long it took to process the request, in milliseconds.
--
-- /Note:/ Consider using 'timems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTimems :: Lens.Lens' SuggestStatus (Core.Maybe Core.Integer)
ssTimems = Lens.field @"timems"
{-# INLINEABLE ssTimems #-}
{-# DEPRECATED timems "Use generic-lens or generic-optics with 'timems' instead"  #-}

instance Core.FromJSON SuggestStatus where
        parseJSON
          = Core.withObject "SuggestStatus" Core.$
              \ x ->
                SuggestStatus' Core.<$>
                  (x Core..:? "rid") Core.<*> x Core..:? "timems"

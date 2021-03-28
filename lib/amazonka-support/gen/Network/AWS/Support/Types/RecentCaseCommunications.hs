{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.RecentCaseCommunications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.RecentCaseCommunications
  ( RecentCaseCommunications (..)
  -- * Smart constructor
  , mkRecentCaseCommunications
  -- * Lenses
  , rccCommunications
  , rccNextToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.Communication as Types
import qualified Network.AWS.Support.Types.NextToken as Types

-- | The five most recent communications associated with the case.
--
-- /See:/ 'mkRecentCaseCommunications' smart constructor.
data RecentCaseCommunications = RecentCaseCommunications'
  { communications :: Core.Maybe [Types.Communication]
    -- ^ The five most recent communications associated with the case.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A resumption point for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecentCaseCommunications' value with any optional fields omitted.
mkRecentCaseCommunications
    :: RecentCaseCommunications
mkRecentCaseCommunications
  = RecentCaseCommunications'{communications = Core.Nothing,
                              nextToken = Core.Nothing}

-- | The five most recent communications associated with the case.
--
-- /Note:/ Consider using 'communications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccCommunications :: Lens.Lens' RecentCaseCommunications (Core.Maybe [Types.Communication])
rccCommunications = Lens.field @"communications"
{-# INLINEABLE rccCommunications #-}
{-# DEPRECATED communications "Use generic-lens or generic-optics with 'communications' instead"  #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccNextToken :: Lens.Lens' RecentCaseCommunications (Core.Maybe Types.NextToken)
rccNextToken = Lens.field @"nextToken"
{-# INLINEABLE rccNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON RecentCaseCommunications where
        parseJSON
          = Core.withObject "RecentCaseCommunications" Core.$
              \ x ->
                RecentCaseCommunications' Core.<$>
                  (x Core..:? "communications") Core.<*> x Core..:? "nextToken"

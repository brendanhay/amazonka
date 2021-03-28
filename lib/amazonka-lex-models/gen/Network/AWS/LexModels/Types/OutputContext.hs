{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.OutputContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.OutputContext
  ( OutputContext (..)
  -- * Smart constructor
  , mkOutputContext
  -- * Lenses
  , ocName
  , ocTimeToLiveInSeconds
  , ocTurnsToLive
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.OutputContextName as Types
import qualified Network.AWS.Prelude as Core

-- | The specification of an output context that is set when an intent is fulfilled.
--
-- /See:/ 'mkOutputContext' smart constructor.
data OutputContext = OutputContext'
  { name :: Types.OutputContextName
    -- ^ The name of the context.
  , timeToLiveInSeconds :: Core.Natural
    -- ^ The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
  , turnsToLive :: Core.Natural
    -- ^ The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputContext' value with any optional fields omitted.
mkOutputContext
    :: Types.OutputContextName -- ^ 'name'
    -> Core.Natural -- ^ 'timeToLiveInSeconds'
    -> Core.Natural -- ^ 'turnsToLive'
    -> OutputContext
mkOutputContext name timeToLiveInSeconds turnsToLive
  = OutputContext'{name, timeToLiveInSeconds, turnsToLive}

-- | The name of the context.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocName :: Lens.Lens' OutputContext Types.OutputContextName
ocName = Lens.field @"name"
{-# INLINEABLE ocName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
--
-- /Note:/ Consider using 'timeToLiveInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTimeToLiveInSeconds :: Lens.Lens' OutputContext Core.Natural
ocTimeToLiveInSeconds = Lens.field @"timeToLiveInSeconds"
{-# INLINEABLE ocTimeToLiveInSeconds #-}
{-# DEPRECATED timeToLiveInSeconds "Use generic-lens or generic-optics with 'timeToLiveInSeconds' instead"  #-}

-- | The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
--
-- /Note:/ Consider using 'turnsToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTurnsToLive :: Lens.Lens' OutputContext Core.Natural
ocTurnsToLive = Lens.field @"turnsToLive"
{-# INLINEABLE ocTurnsToLive #-}
{-# DEPRECATED turnsToLive "Use generic-lens or generic-optics with 'turnsToLive' instead"  #-}

instance Core.FromJSON OutputContext where
        toJSON OutputContext{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("timeToLiveInSeconds" Core..= timeToLiveInSeconds),
                  Core.Just ("turnsToLive" Core..= turnsToLive)])

instance Core.FromJSON OutputContext where
        parseJSON
          = Core.withObject "OutputContext" Core.$
              \ x ->
                OutputContext' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "timeToLiveInSeconds"
                    Core.<*> x Core..: "turnsToLive"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.UtteranceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.UtteranceData
  ( UtteranceData (..)
  -- * Smart constructor
  , mkUtteranceData
  -- * Lenses
  , udCount
  , udDistinctUsers
  , udFirstUtteredDate
  , udLastUtteredDate
  , udUtteranceString
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.UtteranceString as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about a single utterance that was made to your bot. 
--
-- /See:/ 'mkUtteranceData' smart constructor.
data UtteranceData = UtteranceData'
  { count :: Core.Maybe Core.Int
    -- ^ The number of times that the utterance was processed.
  , distinctUsers :: Core.Maybe Core.Int
    -- ^ The total number of individuals that used the utterance.
  , firstUtteredDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the utterance was first recorded.
  , lastUtteredDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the utterance was last recorded.
  , utteranceString :: Core.Maybe Types.UtteranceString
    -- ^ The text that was entered by the user or the text representation of an audio clip.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UtteranceData' value with any optional fields omitted.
mkUtteranceData
    :: UtteranceData
mkUtteranceData
  = UtteranceData'{count = Core.Nothing,
                   distinctUsers = Core.Nothing, firstUtteredDate = Core.Nothing,
                   lastUtteredDate = Core.Nothing, utteranceString = Core.Nothing}

-- | The number of times that the utterance was processed.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCount :: Lens.Lens' UtteranceData (Core.Maybe Core.Int)
udCount = Lens.field @"count"
{-# INLINEABLE udCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The total number of individuals that used the utterance.
--
-- /Note:/ Consider using 'distinctUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDistinctUsers :: Lens.Lens' UtteranceData (Core.Maybe Core.Int)
udDistinctUsers = Lens.field @"distinctUsers"
{-# INLINEABLE udDistinctUsers #-}
{-# DEPRECATED distinctUsers "Use generic-lens or generic-optics with 'distinctUsers' instead"  #-}

-- | The date that the utterance was first recorded.
--
-- /Note:/ Consider using 'firstUtteredDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udFirstUtteredDate :: Lens.Lens' UtteranceData (Core.Maybe Core.NominalDiffTime)
udFirstUtteredDate = Lens.field @"firstUtteredDate"
{-# INLINEABLE udFirstUtteredDate #-}
{-# DEPRECATED firstUtteredDate "Use generic-lens or generic-optics with 'firstUtteredDate' instead"  #-}

-- | The date that the utterance was last recorded.
--
-- /Note:/ Consider using 'lastUtteredDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udLastUtteredDate :: Lens.Lens' UtteranceData (Core.Maybe Core.NominalDiffTime)
udLastUtteredDate = Lens.field @"lastUtteredDate"
{-# INLINEABLE udLastUtteredDate #-}
{-# DEPRECATED lastUtteredDate "Use generic-lens or generic-optics with 'lastUtteredDate' instead"  #-}

-- | The text that was entered by the user or the text representation of an audio clip.
--
-- /Note:/ Consider using 'utteranceString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUtteranceString :: Lens.Lens' UtteranceData (Core.Maybe Types.UtteranceString)
udUtteranceString = Lens.field @"utteranceString"
{-# INLINEABLE udUtteranceString #-}
{-# DEPRECATED utteranceString "Use generic-lens or generic-optics with 'utteranceString' instead"  #-}

instance Core.FromJSON UtteranceData where
        parseJSON
          = Core.withObject "UtteranceData" Core.$
              \ x ->
                UtteranceData' Core.<$>
                  (x Core..:? "count") Core.<*> x Core..:? "distinctUsers" Core.<*>
                    x Core..:? "firstUtteredDate"
                    Core.<*> x Core..:? "lastUtteredDate"
                    Core.<*> x Core..:? "utteranceString"

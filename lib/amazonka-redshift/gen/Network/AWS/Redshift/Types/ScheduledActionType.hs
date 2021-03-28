{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ScheduledActionType
  ( ScheduledActionType (..)
  -- * Smart constructor
  , mkScheduledActionType
  -- * Lenses
  , satPauseCluster
  , satResizeCluster
  , satResumeCluster
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.PauseClusterMessage as Types
import qualified Network.AWS.Redshift.Types.ResizeClusterMessage as Types
import qualified Network.AWS.Redshift.Types.ResumeClusterMessage as Types

-- | The action type that specifies an Amazon Redshift API operation that is supported by the Amazon Redshift scheduler. 
--
-- /See:/ 'mkScheduledActionType' smart constructor.
data ScheduledActionType = ScheduledActionType'
  { pauseCluster :: Core.Maybe Types.PauseClusterMessage
    -- ^ An action that runs a @PauseCluster@ API operation. 
  , resizeCluster :: Core.Maybe Types.ResizeClusterMessage
    -- ^ An action that runs a @ResizeCluster@ API operation. 
  , resumeCluster :: Core.Maybe Types.ResumeClusterMessage
    -- ^ An action that runs a @ResumeCluster@ API operation. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledActionType' value with any optional fields omitted.
mkScheduledActionType
    :: ScheduledActionType
mkScheduledActionType
  = ScheduledActionType'{pauseCluster = Core.Nothing,
                         resizeCluster = Core.Nothing, resumeCluster = Core.Nothing}

-- | An action that runs a @PauseCluster@ API operation. 
--
-- /Note:/ Consider using 'pauseCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satPauseCluster :: Lens.Lens' ScheduledActionType (Core.Maybe Types.PauseClusterMessage)
satPauseCluster = Lens.field @"pauseCluster"
{-# INLINEABLE satPauseCluster #-}
{-# DEPRECATED pauseCluster "Use generic-lens or generic-optics with 'pauseCluster' instead"  #-}

-- | An action that runs a @ResizeCluster@ API operation. 
--
-- /Note:/ Consider using 'resizeCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satResizeCluster :: Lens.Lens' ScheduledActionType (Core.Maybe Types.ResizeClusterMessage)
satResizeCluster = Lens.field @"resizeCluster"
{-# INLINEABLE satResizeCluster #-}
{-# DEPRECATED resizeCluster "Use generic-lens or generic-optics with 'resizeCluster' instead"  #-}

-- | An action that runs a @ResumeCluster@ API operation. 
--
-- /Note:/ Consider using 'resumeCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satResumeCluster :: Lens.Lens' ScheduledActionType (Core.Maybe Types.ResumeClusterMessage)
satResumeCluster = Lens.field @"resumeCluster"
{-# INLINEABLE satResumeCluster #-}
{-# DEPRECATED resumeCluster "Use generic-lens or generic-optics with 'resumeCluster' instead"  #-}

instance Core.ToQuery ScheduledActionType where
        toQuery ScheduledActionType{..}
          = Core.maybe Core.mempty (Core.toQueryPair "PauseCluster")
              pauseCluster
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResizeCluster")
                resizeCluster
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResumeCluster")
                resumeCluster

instance Core.FromXML ScheduledActionType where
        parseXML x
          = ScheduledActionType' Core.<$>
              (x Core..@? "PauseCluster") Core.<*> x Core..@? "ResizeCluster"
                Core.<*> x Core..@? "ResumeCluster"

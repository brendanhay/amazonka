{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
  ( ClusterAssociatedToSchedule (..)
  -- * Smart constructor
  , mkClusterAssociatedToSchedule
  -- * Lenses
  , catsClusterIdentifier
  , catsScheduleAssociationState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ScheduleState as Types

-- | 
--
-- /See:/ 'mkClusterAssociatedToSchedule' smart constructor.
data ClusterAssociatedToSchedule = ClusterAssociatedToSchedule'
  { clusterIdentifier :: Core.Maybe Core.Text
    -- ^ 
  , scheduleAssociationState :: Core.Maybe Types.ScheduleState
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterAssociatedToSchedule' value with any optional fields omitted.
mkClusterAssociatedToSchedule
    :: ClusterAssociatedToSchedule
mkClusterAssociatedToSchedule
  = ClusterAssociatedToSchedule'{clusterIdentifier = Core.Nothing,
                                 scheduleAssociationState = Core.Nothing}

-- | 
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catsClusterIdentifier :: Lens.Lens' ClusterAssociatedToSchedule (Core.Maybe Core.Text)
catsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE catsClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'scheduleAssociationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catsScheduleAssociationState :: Lens.Lens' ClusterAssociatedToSchedule (Core.Maybe Types.ScheduleState)
catsScheduleAssociationState = Lens.field @"scheduleAssociationState"
{-# INLINEABLE catsScheduleAssociationState #-}
{-# DEPRECATED scheduleAssociationState "Use generic-lens or generic-optics with 'scheduleAssociationState' instead"  #-}

instance Core.FromXML ClusterAssociatedToSchedule where
        parseXML x
          = ClusterAssociatedToSchedule' Core.<$>
              (x Core..@? "ClusterIdentifier") Core.<*>
                x Core..@? "ScheduleAssociationState"

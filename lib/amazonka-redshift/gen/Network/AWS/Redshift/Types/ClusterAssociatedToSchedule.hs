{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
  ( ClusterAssociatedToSchedule (..),

    -- * Smart constructor
    mkClusterAssociatedToSchedule,

    -- * Lenses
    catsScheduleAssociationState,
    catsClusterIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduleState

-- |
--
-- /See:/ 'mkClusterAssociatedToSchedule' smart constructor.
data ClusterAssociatedToSchedule = ClusterAssociatedToSchedule'
  { -- |
    scheduleAssociationState :: Lude.Maybe ScheduleState,
    -- |
    clusterIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterAssociatedToSchedule' with the minimum fields required to make a request.
--
-- * 'scheduleAssociationState' -
-- * 'clusterIdentifier' -
mkClusterAssociatedToSchedule ::
  ClusterAssociatedToSchedule
mkClusterAssociatedToSchedule =
  ClusterAssociatedToSchedule'
    { scheduleAssociationState =
        Lude.Nothing,
      clusterIdentifier = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'scheduleAssociationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catsScheduleAssociationState :: Lens.Lens' ClusterAssociatedToSchedule (Lude.Maybe ScheduleState)
catsScheduleAssociationState = Lens.lens (scheduleAssociationState :: ClusterAssociatedToSchedule -> Lude.Maybe ScheduleState) (\s a -> s {scheduleAssociationState = a} :: ClusterAssociatedToSchedule)
{-# DEPRECATED catsScheduleAssociationState "Use generic-lens or generic-optics with 'scheduleAssociationState' instead." #-}

-- |
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catsClusterIdentifier :: Lens.Lens' ClusterAssociatedToSchedule (Lude.Maybe Lude.Text)
catsClusterIdentifier = Lens.lens (clusterIdentifier :: ClusterAssociatedToSchedule -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: ClusterAssociatedToSchedule)
{-# DEPRECATED catsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.FromXML ClusterAssociatedToSchedule where
  parseXML x =
    ClusterAssociatedToSchedule'
      Lude.<$> (x Lude..@? "ScheduleAssociationState")
      Lude.<*> (x Lude..@? "ClusterIdentifier")

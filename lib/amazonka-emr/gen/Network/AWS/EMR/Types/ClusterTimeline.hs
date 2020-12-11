-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterTimeline
  ( ClusterTimeline (..),

    -- * Smart constructor
    mkClusterTimeline,

    -- * Lenses
    ctReadyDateTime,
    ctCreationDateTime,
    ctEndDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the timeline of the cluster's lifecycle.
--
-- /See:/ 'mkClusterTimeline' smart constructor.
data ClusterTimeline = ClusterTimeline'
  { readyDateTime ::
      Lude.Maybe Lude.Timestamp,
    creationDateTime :: Lude.Maybe Lude.Timestamp,
    endDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterTimeline' with the minimum fields required to make a request.
--
-- * 'creationDateTime' - The creation date and time of the cluster.
-- * 'endDateTime' - The date and time when the cluster was terminated.
-- * 'readyDateTime' - The date and time when the cluster was ready to run steps.
mkClusterTimeline ::
  ClusterTimeline
mkClusterTimeline =
  ClusterTimeline'
    { readyDateTime = Lude.Nothing,
      creationDateTime = Lude.Nothing,
      endDateTime = Lude.Nothing
    }

-- | The date and time when the cluster was ready to run steps.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctReadyDateTime :: Lens.Lens' ClusterTimeline (Lude.Maybe Lude.Timestamp)
ctReadyDateTime = Lens.lens (readyDateTime :: ClusterTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {readyDateTime = a} :: ClusterTimeline)
{-# DEPRECATED ctReadyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead." #-}

-- | The creation date and time of the cluster.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCreationDateTime :: Lens.Lens' ClusterTimeline (Lude.Maybe Lude.Timestamp)
ctCreationDateTime = Lens.lens (creationDateTime :: ClusterTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: ClusterTimeline)
{-# DEPRECATED ctCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The date and time when the cluster was terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctEndDateTime :: Lens.Lens' ClusterTimeline (Lude.Maybe Lude.Timestamp)
ctEndDateTime = Lens.lens (endDateTime :: ClusterTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDateTime = a} :: ClusterTimeline)
{-# DEPRECATED ctEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

instance Lude.FromJSON ClusterTimeline where
  parseJSON =
    Lude.withObject
      "ClusterTimeline"
      ( \x ->
          ClusterTimeline'
            Lude.<$> (x Lude..:? "ReadyDateTime")
            Lude.<*> (x Lude..:? "CreationDateTime")
            Lude.<*> (x Lude..:? "EndDateTime")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetTimeline
  ( InstanceFleetTimeline (..),

    -- * Smart constructor
    mkInstanceFleetTimeline,

    -- * Lenses
    iftReadyDateTime,
    iftCreationDateTime,
    iftEndDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
--
-- /See:/ 'mkInstanceFleetTimeline' smart constructor.
data InstanceFleetTimeline = InstanceFleetTimeline'
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

-- | Creates a value of 'InstanceFleetTimeline' with the minimum fields required to make a request.
--
-- * 'creationDateTime' - The time and date the instance fleet was created.
-- * 'endDateTime' - The time and date the instance fleet terminated.
-- * 'readyDateTime' - The time and date the instance fleet was ready to run jobs.
mkInstanceFleetTimeline ::
  InstanceFleetTimeline
mkInstanceFleetTimeline =
  InstanceFleetTimeline'
    { readyDateTime = Lude.Nothing,
      creationDateTime = Lude.Nothing,
      endDateTime = Lude.Nothing
    }

-- | The time and date the instance fleet was ready to run jobs.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iftReadyDateTime :: Lens.Lens' InstanceFleetTimeline (Lude.Maybe Lude.Timestamp)
iftReadyDateTime = Lens.lens (readyDateTime :: InstanceFleetTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {readyDateTime = a} :: InstanceFleetTimeline)
{-# DEPRECATED iftReadyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead." #-}

-- | The time and date the instance fleet was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iftCreationDateTime :: Lens.Lens' InstanceFleetTimeline (Lude.Maybe Lude.Timestamp)
iftCreationDateTime = Lens.lens (creationDateTime :: InstanceFleetTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: InstanceFleetTimeline)
{-# DEPRECATED iftCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The time and date the instance fleet terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iftEndDateTime :: Lens.Lens' InstanceFleetTimeline (Lude.Maybe Lude.Timestamp)
iftEndDateTime = Lens.lens (endDateTime :: InstanceFleetTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDateTime = a} :: InstanceFleetTimeline)
{-# DEPRECATED iftEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

instance Lude.FromJSON InstanceFleetTimeline where
  parseJSON =
    Lude.withObject
      "InstanceFleetTimeline"
      ( \x ->
          InstanceFleetTimeline'
            Lude.<$> (x Lude..:? "ReadyDateTime")
            Lude.<*> (x Lude..:? "CreationDateTime")
            Lude.<*> (x Lude..:? "EndDateTime")
      )

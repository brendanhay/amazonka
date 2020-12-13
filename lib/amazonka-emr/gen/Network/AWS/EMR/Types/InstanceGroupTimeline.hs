{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupTimeline
  ( InstanceGroupTimeline (..),

    -- * Smart constructor
    mkInstanceGroupTimeline,

    -- * Lenses
    igtReadyDateTime,
    igtCreationDateTime,
    igtEndDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The timeline of the instance group lifecycle.
--
-- /See:/ 'mkInstanceGroupTimeline' smart constructor.
data InstanceGroupTimeline = InstanceGroupTimeline'
  { -- | The date and time when the instance group became ready to perform tasks.
    readyDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The creation date and time of the instance group.
    creationDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The date and time when the instance group terminated.
    endDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceGroupTimeline' with the minimum fields required to make a request.
--
-- * 'readyDateTime' - The date and time when the instance group became ready to perform tasks.
-- * 'creationDateTime' - The creation date and time of the instance group.
-- * 'endDateTime' - The date and time when the instance group terminated.
mkInstanceGroupTimeline ::
  InstanceGroupTimeline
mkInstanceGroupTimeline =
  InstanceGroupTimeline'
    { readyDateTime = Lude.Nothing,
      creationDateTime = Lude.Nothing,
      endDateTime = Lude.Nothing
    }

-- | The date and time when the instance group became ready to perform tasks.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igtReadyDateTime :: Lens.Lens' InstanceGroupTimeline (Lude.Maybe Lude.Timestamp)
igtReadyDateTime = Lens.lens (readyDateTime :: InstanceGroupTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {readyDateTime = a} :: InstanceGroupTimeline)
{-# DEPRECATED igtReadyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead." #-}

-- | The creation date and time of the instance group.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igtCreationDateTime :: Lens.Lens' InstanceGroupTimeline (Lude.Maybe Lude.Timestamp)
igtCreationDateTime = Lens.lens (creationDateTime :: InstanceGroupTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: InstanceGroupTimeline)
{-# DEPRECATED igtCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The date and time when the instance group terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igtEndDateTime :: Lens.Lens' InstanceGroupTimeline (Lude.Maybe Lude.Timestamp)
igtEndDateTime = Lens.lens (endDateTime :: InstanceGroupTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDateTime = a} :: InstanceGroupTimeline)
{-# DEPRECATED igtEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

instance Lude.FromJSON InstanceGroupTimeline where
  parseJSON =
    Lude.withObject
      "InstanceGroupTimeline"
      ( \x ->
          InstanceGroupTimeline'
            Lude.<$> (x Lude..:? "ReadyDateTime")
            Lude.<*> (x Lude..:? "CreationDateTime")
            Lude.<*> (x Lude..:? "EndDateTime")
      )

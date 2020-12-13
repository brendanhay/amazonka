{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTimeline
  ( InstanceTimeline (..),

    -- * Smart constructor
    mkInstanceTimeline,

    -- * Lenses
    itReadyDateTime,
    itCreationDateTime,
    itEndDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The timeline of the instance lifecycle.
--
-- /See:/ 'mkInstanceTimeline' smart constructor.
data InstanceTimeline = InstanceTimeline'
  { -- | The date and time when the instance was ready to perform tasks.
    readyDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The creation date and time of the instance.
    creationDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The date and time when the instance was terminated.
    endDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceTimeline' with the minimum fields required to make a request.
--
-- * 'readyDateTime' - The date and time when the instance was ready to perform tasks.
-- * 'creationDateTime' - The creation date and time of the instance.
-- * 'endDateTime' - The date and time when the instance was terminated.
mkInstanceTimeline ::
  InstanceTimeline
mkInstanceTimeline =
  InstanceTimeline'
    { readyDateTime = Lude.Nothing,
      creationDateTime = Lude.Nothing,
      endDateTime = Lude.Nothing
    }

-- | The date and time when the instance was ready to perform tasks.
--
-- /Note:/ Consider using 'readyDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itReadyDateTime :: Lens.Lens' InstanceTimeline (Lude.Maybe Lude.Timestamp)
itReadyDateTime = Lens.lens (readyDateTime :: InstanceTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {readyDateTime = a} :: InstanceTimeline)
{-# DEPRECATED itReadyDateTime "Use generic-lens or generic-optics with 'readyDateTime' instead." #-}

-- | The creation date and time of the instance.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itCreationDateTime :: Lens.Lens' InstanceTimeline (Lude.Maybe Lude.Timestamp)
itCreationDateTime = Lens.lens (creationDateTime :: InstanceTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: InstanceTimeline)
{-# DEPRECATED itCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The date and time when the instance was terminated.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itEndDateTime :: Lens.Lens' InstanceTimeline (Lude.Maybe Lude.Timestamp)
itEndDateTime = Lens.lens (endDateTime :: InstanceTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDateTime = a} :: InstanceTimeline)
{-# DEPRECATED itEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

instance Lude.FromJSON InstanceTimeline where
  parseJSON =
    Lude.withObject
      "InstanceTimeline"
      ( \x ->
          InstanceTimeline'
            Lude.<$> (x Lude..:? "ReadyDateTime")
            Lude.<*> (x Lude..:? "CreationDateTime")
            Lude.<*> (x Lude..:? "EndDateTime")
      )

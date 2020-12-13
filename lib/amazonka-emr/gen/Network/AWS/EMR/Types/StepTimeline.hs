{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepTimeline
  ( StepTimeline (..),

    -- * Smart constructor
    mkStepTimeline,

    -- * Lenses
    stCreationDateTime,
    stEndDateTime,
    stStartDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The timeline of the cluster step lifecycle.
--
-- /See:/ 'mkStepTimeline' smart constructor.
data StepTimeline = StepTimeline'
  { -- | The date and time when the cluster step was created.
    creationDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The date and time when the cluster step execution completed or failed.
    endDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The date and time when the cluster step execution started.
    startDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepTimeline' with the minimum fields required to make a request.
--
-- * 'creationDateTime' - The date and time when the cluster step was created.
-- * 'endDateTime' - The date and time when the cluster step execution completed or failed.
-- * 'startDateTime' - The date and time when the cluster step execution started.
mkStepTimeline ::
  StepTimeline
mkStepTimeline =
  StepTimeline'
    { creationDateTime = Lude.Nothing,
      endDateTime = Lude.Nothing,
      startDateTime = Lude.Nothing
    }

-- | The date and time when the cluster step was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCreationDateTime :: Lens.Lens' StepTimeline (Lude.Maybe Lude.Timestamp)
stCreationDateTime = Lens.lens (creationDateTime :: StepTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: StepTimeline)
{-# DEPRECATED stCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The date and time when the cluster step execution completed or failed.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stEndDateTime :: Lens.Lens' StepTimeline (Lude.Maybe Lude.Timestamp)
stEndDateTime = Lens.lens (endDateTime :: StepTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDateTime = a} :: StepTimeline)
{-# DEPRECATED stEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

-- | The date and time when the cluster step execution started.
--
-- /Note:/ Consider using 'startDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStartDateTime :: Lens.Lens' StepTimeline (Lude.Maybe Lude.Timestamp)
stStartDateTime = Lens.lens (startDateTime :: StepTimeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {startDateTime = a} :: StepTimeline)
{-# DEPRECATED stStartDateTime "Use generic-lens or generic-optics with 'startDateTime' instead." #-}

instance Lude.FromJSON StepTimeline where
  parseJSON =
    Lude.withObject
      "StepTimeline"
      ( \x ->
          StepTimeline'
            Lude.<$> (x Lude..:? "CreationDateTime")
            Lude.<*> (x Lude..:? "EndDateTime")
            Lude.<*> (x Lude..:? "StartDateTime")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Schedule
  ( Schedule (..),

    -- * Smart constructor
    mkSchedule,

    -- * Lenses
    sExpression,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The schedule for when to trigger an update.
--
-- /See:/ 'mkSchedule' smart constructor.
newtype Schedule = Schedule' {expression :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- * 'expression' - The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the /Amazon CloudWatch Events User Guide/ .
mkSchedule ::
  Schedule
mkSchedule = Schedule' {expression = Lude.Nothing}

-- | The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the /Amazon CloudWatch Events User Guide/ .
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExpression :: Lens.Lens' Schedule (Lude.Maybe Lude.Text)
sExpression = Lens.lens (expression :: Schedule -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: Schedule)
{-# DEPRECATED sExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Lude.FromJSON Schedule where
  parseJSON =
    Lude.withObject
      "Schedule"
      (\x -> Schedule' Lude.<$> (x Lude..:? "expression"))

instance Lude.ToJSON Schedule where
  toJSON Schedule' {..} =
    Lude.object
      (Lude.catMaybes [("expression" Lude..=) Lude.<$> expression])

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.RetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.RetentionPeriod
  ( RetentionPeriod (..),

    -- * Smart constructor
    mkRetentionPeriod,

    -- * Lenses
    rpUnlimited,
    rpNumberOfDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | How long, in days, message data is kept.
--
-- /See:/ 'mkRetentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { unlimited ::
      Lude.Maybe Lude.Bool,
    numberOfDays :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetentionPeriod' with the minimum fields required to make a request.
--
-- * 'numberOfDays' - The number of days that message data is kept. The @unlimited@ parameter must be false.
-- * 'unlimited' - If true, message data is kept indefinitely.
mkRetentionPeriod ::
  RetentionPeriod
mkRetentionPeriod =
  RetentionPeriod'
    { unlimited = Lude.Nothing,
      numberOfDays = Lude.Nothing
    }

-- | If true, message data is kept indefinitely.
--
-- /Note:/ Consider using 'unlimited' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpUnlimited :: Lens.Lens' RetentionPeriod (Lude.Maybe Lude.Bool)
rpUnlimited = Lens.lens (unlimited :: RetentionPeriod -> Lude.Maybe Lude.Bool) (\s a -> s {unlimited = a} :: RetentionPeriod)
{-# DEPRECATED rpUnlimited "Use generic-lens or generic-optics with 'unlimited' instead." #-}

-- | The number of days that message data is kept. The @unlimited@ parameter must be false.
--
-- /Note:/ Consider using 'numberOfDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpNumberOfDays :: Lens.Lens' RetentionPeriod (Lude.Maybe Lude.Natural)
rpNumberOfDays = Lens.lens (numberOfDays :: RetentionPeriod -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfDays = a} :: RetentionPeriod)
{-# DEPRECATED rpNumberOfDays "Use generic-lens or generic-optics with 'numberOfDays' instead." #-}

instance Lude.FromJSON RetentionPeriod where
  parseJSON =
    Lude.withObject
      "RetentionPeriod"
      ( \x ->
          RetentionPeriod'
            Lude.<$> (x Lude..:? "unlimited") Lude.<*> (x Lude..:? "numberOfDays")
      )

instance Lude.ToJSON RetentionPeriod where
  toJSON RetentionPeriod' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("unlimited" Lude..=) Lude.<$> unlimited,
            ("numberOfDays" Lude..=) Lude.<$> numberOfDays
          ]
      )

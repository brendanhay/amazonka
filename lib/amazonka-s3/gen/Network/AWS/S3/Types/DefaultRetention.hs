{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DefaultRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DefaultRetention
  ( DefaultRetention (..),

    -- * Smart constructor
    mkDefaultRetention,

    -- * Lenses
    drDays,
    drMode,
    drYears,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockRetentionMode

-- | The container element for specifying the default Object Lock retention settings for new objects placed in the specified bucket.
--
-- /See:/ 'mkDefaultRetention' smart constructor.
data DefaultRetention = DefaultRetention'
  { -- | The number of days that you want to specify for the default retention period.
    days :: Lude.Maybe Lude.Int,
    -- | The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
    mode :: Lude.Maybe ObjectLockRetentionMode,
    -- | The number of years that you want to specify for the default retention period.
    years :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefaultRetention' with the minimum fields required to make a request.
--
-- * 'days' - The number of days that you want to specify for the default retention period.
-- * 'mode' - The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
-- * 'years' - The number of years that you want to specify for the default retention period.
mkDefaultRetention ::
  DefaultRetention
mkDefaultRetention =
  DefaultRetention'
    { days = Lude.Nothing,
      mode = Lude.Nothing,
      years = Lude.Nothing
    }

-- | The number of days that you want to specify for the default retention period.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDays :: Lens.Lens' DefaultRetention (Lude.Maybe Lude.Int)
drDays = Lens.lens (days :: DefaultRetention -> Lude.Maybe Lude.Int) (\s a -> s {days = a} :: DefaultRetention)
{-# DEPRECATED drDays "Use generic-lens or generic-optics with 'days' instead." #-}

-- | The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMode :: Lens.Lens' DefaultRetention (Lude.Maybe ObjectLockRetentionMode)
drMode = Lens.lens (mode :: DefaultRetention -> Lude.Maybe ObjectLockRetentionMode) (\s a -> s {mode = a} :: DefaultRetention)
{-# DEPRECATED drMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The number of years that you want to specify for the default retention period.
--
-- /Note:/ Consider using 'years' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drYears :: Lens.Lens' DefaultRetention (Lude.Maybe Lude.Int)
drYears = Lens.lens (years :: DefaultRetention -> Lude.Maybe Lude.Int) (\s a -> s {years = a} :: DefaultRetention)
{-# DEPRECATED drYears "Use generic-lens or generic-optics with 'years' instead." #-}

instance Lude.FromXML DefaultRetention where
  parseXML x =
    DefaultRetention'
      Lude.<$> (x Lude..@? "Days")
      Lude.<*> (x Lude..@? "Mode")
      Lude.<*> (x Lude..@? "Years")

instance Lude.ToXML DefaultRetention where
  toXML DefaultRetention' {..} =
    Lude.mconcat
      ["Days" Lude.@= days, "Mode" Lude.@= mode, "Years" Lude.@= years]

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RequireCheckIn
  ( RequireCheckIn (..),

    -- * Smart constructor
    mkRequireCheckIn,

    -- * Lenses
    rciEnabled,
    rciReleaseAfterMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released.
--
-- /See:/ 'mkRequireCheckIn' smart constructor.
data RequireCheckIn = RequireCheckIn'
  { -- | Whether require check in is enabled or not.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
    releaseAfterMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequireCheckIn' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether require check in is enabled or not.
-- * 'releaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
mkRequireCheckIn ::
  RequireCheckIn
mkRequireCheckIn =
  RequireCheckIn'
    { enabled = Lude.Nothing,
      releaseAfterMinutes = Lude.Nothing
    }

-- | Whether require check in is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciEnabled :: Lens.Lens' RequireCheckIn (Lude.Maybe Lude.Bool)
rciEnabled = Lens.lens (enabled :: RequireCheckIn -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: RequireCheckIn)
{-# DEPRECATED rciEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
--
-- /Note:/ Consider using 'releaseAfterMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciReleaseAfterMinutes :: Lens.Lens' RequireCheckIn (Lude.Maybe Lude.Int)
rciReleaseAfterMinutes = Lens.lens (releaseAfterMinutes :: RequireCheckIn -> Lude.Maybe Lude.Int) (\s a -> s {releaseAfterMinutes = a} :: RequireCheckIn)
{-# DEPRECATED rciReleaseAfterMinutes "Use generic-lens or generic-optics with 'releaseAfterMinutes' instead." #-}

instance Lude.FromJSON RequireCheckIn where
  parseJSON =
    Lude.withObject
      "RequireCheckIn"
      ( \x ->
          RequireCheckIn'
            Lude.<$> (x Lude..:? "Enabled") Lude.<*> (x Lude..:? "ReleaseAfterMinutes")
      )

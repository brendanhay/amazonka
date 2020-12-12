{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
  ( CreateRequireCheckIn (..),

    -- * Smart constructor
    mkCreateRequireCheckIn,

    -- * Lenses
    crciReleaseAfterMinutes,
    crciEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Creates settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released.
--
-- /See:/ 'mkCreateRequireCheckIn' smart constructor.
data CreateRequireCheckIn = CreateRequireCheckIn'
  { releaseAfterMinutes ::
      Lude.Int,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRequireCheckIn' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether require check in is enabled or not.
-- * 'releaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
mkCreateRequireCheckIn ::
  -- | 'releaseAfterMinutes'
  Lude.Int ->
  -- | 'enabled'
  Lude.Bool ->
  CreateRequireCheckIn
mkCreateRequireCheckIn pReleaseAfterMinutes_ pEnabled_ =
  CreateRequireCheckIn'
    { releaseAfterMinutes =
        pReleaseAfterMinutes_,
      enabled = pEnabled_
    }

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
--
-- /Note:/ Consider using 'releaseAfterMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crciReleaseAfterMinutes :: Lens.Lens' CreateRequireCheckIn Lude.Int
crciReleaseAfterMinutes = Lens.lens (releaseAfterMinutes :: CreateRequireCheckIn -> Lude.Int) (\s a -> s {releaseAfterMinutes = a} :: CreateRequireCheckIn)
{-# DEPRECATED crciReleaseAfterMinutes "Use generic-lens or generic-optics with 'releaseAfterMinutes' instead." #-}

-- | Whether require check in is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crciEnabled :: Lens.Lens' CreateRequireCheckIn Lude.Bool
crciEnabled = Lens.lens (enabled :: CreateRequireCheckIn -> Lude.Bool) (\s a -> s {enabled = a} :: CreateRequireCheckIn)
{-# DEPRECATED crciEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToJSON CreateRequireCheckIn where
  toJSON CreateRequireCheckIn' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ReleaseAfterMinutes" Lude..= releaseAfterMinutes),
            Lude.Just ("Enabled" Lude..= enabled)
          ]
      )

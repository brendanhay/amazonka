{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossBehavior
  ( InputLossBehavior (..),

    -- * Smart constructor
    mkInputLossBehavior,

    -- * Lenses
    ilbInputLossImageColor,
    ilbBlackFrameMsec,
    ilbRepeatFrameMsec,
    ilbInputLossImageType,
    ilbInputLossImageSlate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.MediaLive.Types.InputLossImageType
import qualified Network.AWS.Prelude as Lude

-- | Input Loss Behavior
--
-- /See:/ 'mkInputLossBehavior' smart constructor.
data InputLossBehavior = InputLossBehavior'
  { -- | When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
    inputLossImageColor :: Lude.Maybe Lude.Text,
    -- | Documentation update needed
    blackFrameMsec :: Lude.Maybe Lude.Natural,
    -- | Documentation update needed
    repeatFrameMsec :: Lude.Maybe Lude.Natural,
    -- | Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
    inputLossImageType :: Lude.Maybe InputLossImageType,
    -- | When input loss image type is "slate" these fields specify the parameters for accessing the slate.
    inputLossImageSlate :: Lude.Maybe InputLocation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputLossBehavior' with the minimum fields required to make a request.
--
-- * 'inputLossImageColor' - When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
-- * 'blackFrameMsec' - Documentation update needed
-- * 'repeatFrameMsec' - Documentation update needed
-- * 'inputLossImageType' - Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
-- * 'inputLossImageSlate' - When input loss image type is "slate" these fields specify the parameters for accessing the slate.
mkInputLossBehavior ::
  InputLossBehavior
mkInputLossBehavior =
  InputLossBehavior'
    { inputLossImageColor = Lude.Nothing,
      blackFrameMsec = Lude.Nothing,
      repeatFrameMsec = Lude.Nothing,
      inputLossImageType = Lude.Nothing,
      inputLossImageSlate = Lude.Nothing
    }

-- | When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
--
-- /Note:/ Consider using 'inputLossImageColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbInputLossImageColor :: Lens.Lens' InputLossBehavior (Lude.Maybe Lude.Text)
ilbInputLossImageColor = Lens.lens (inputLossImageColor :: InputLossBehavior -> Lude.Maybe Lude.Text) (\s a -> s {inputLossImageColor = a} :: InputLossBehavior)
{-# DEPRECATED ilbInputLossImageColor "Use generic-lens or generic-optics with 'inputLossImageColor' instead." #-}

-- | Documentation update needed
--
-- /Note:/ Consider using 'blackFrameMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbBlackFrameMsec :: Lens.Lens' InputLossBehavior (Lude.Maybe Lude.Natural)
ilbBlackFrameMsec = Lens.lens (blackFrameMsec :: InputLossBehavior -> Lude.Maybe Lude.Natural) (\s a -> s {blackFrameMsec = a} :: InputLossBehavior)
{-# DEPRECATED ilbBlackFrameMsec "Use generic-lens or generic-optics with 'blackFrameMsec' instead." #-}

-- | Documentation update needed
--
-- /Note:/ Consider using 'repeatFrameMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbRepeatFrameMsec :: Lens.Lens' InputLossBehavior (Lude.Maybe Lude.Natural)
ilbRepeatFrameMsec = Lens.lens (repeatFrameMsec :: InputLossBehavior -> Lude.Maybe Lude.Natural) (\s a -> s {repeatFrameMsec = a} :: InputLossBehavior)
{-# DEPRECATED ilbRepeatFrameMsec "Use generic-lens or generic-optics with 'repeatFrameMsec' instead." #-}

-- | Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
--
-- /Note:/ Consider using 'inputLossImageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbInputLossImageType :: Lens.Lens' InputLossBehavior (Lude.Maybe InputLossImageType)
ilbInputLossImageType = Lens.lens (inputLossImageType :: InputLossBehavior -> Lude.Maybe InputLossImageType) (\s a -> s {inputLossImageType = a} :: InputLossBehavior)
{-# DEPRECATED ilbInputLossImageType "Use generic-lens or generic-optics with 'inputLossImageType' instead." #-}

-- | When input loss image type is "slate" these fields specify the parameters for accessing the slate.
--
-- /Note:/ Consider using 'inputLossImageSlate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbInputLossImageSlate :: Lens.Lens' InputLossBehavior (Lude.Maybe InputLocation)
ilbInputLossImageSlate = Lens.lens (inputLossImageSlate :: InputLossBehavior -> Lude.Maybe InputLocation) (\s a -> s {inputLossImageSlate = a} :: InputLossBehavior)
{-# DEPRECATED ilbInputLossImageSlate "Use generic-lens or generic-optics with 'inputLossImageSlate' instead." #-}

instance Lude.FromJSON InputLossBehavior where
  parseJSON =
    Lude.withObject
      "InputLossBehavior"
      ( \x ->
          InputLossBehavior'
            Lude.<$> (x Lude..:? "inputLossImageColor")
            Lude.<*> (x Lude..:? "blackFrameMsec")
            Lude.<*> (x Lude..:? "repeatFrameMsec")
            Lude.<*> (x Lude..:? "inputLossImageType")
            Lude.<*> (x Lude..:? "inputLossImageSlate")
      )

instance Lude.ToJSON InputLossBehavior where
  toJSON InputLossBehavior' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputLossImageColor" Lude..=) Lude.<$> inputLossImageColor,
            ("blackFrameMsec" Lude..=) Lude.<$> blackFrameMsec,
            ("repeatFrameMsec" Lude..=) Lude.<$> repeatFrameMsec,
            ("inputLossImageType" Lude..=) Lude.<$> inputLossImageType,
            ("inputLossImageSlate" Lude..=) Lude.<$> inputLossImageSlate
          ]
      )

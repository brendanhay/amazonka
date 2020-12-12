{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothOutputSettings
  ( MsSmoothOutputSettings (..),

    -- * Smart constructor
    mkMsSmoothOutputSettings,

    -- * Lenses
    msosH265PackagingType,
    msosNameModifier,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
import qualified Network.AWS.Prelude as Lude

-- | Ms Smooth Output Settings
--
-- /See:/ 'mkMsSmoothOutputSettings' smart constructor.
data MsSmoothOutputSettings = MsSmoothOutputSettings'
  { h265PackagingType ::
      Lude.Maybe MsSmoothH265PackagingType,
    nameModifier :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MsSmoothOutputSettings' with the minimum fields required to make a request.
--
-- * 'h265PackagingType' - Only applicable when this output is referencing an H.265 video description.
--
-- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
-- * 'nameModifier' - String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
mkMsSmoothOutputSettings ::
  MsSmoothOutputSettings
mkMsSmoothOutputSettings =
  MsSmoothOutputSettings'
    { h265PackagingType = Lude.Nothing,
      nameModifier = Lude.Nothing
    }

-- | Only applicable when this output is referencing an H.265 video description.
--
-- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
--
-- /Note:/ Consider using 'h265PackagingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msosH265PackagingType :: Lens.Lens' MsSmoothOutputSettings (Lude.Maybe MsSmoothH265PackagingType)
msosH265PackagingType = Lens.lens (h265PackagingType :: MsSmoothOutputSettings -> Lude.Maybe MsSmoothH265PackagingType) (\s a -> s {h265PackagingType = a} :: MsSmoothOutputSettings)
{-# DEPRECATED msosH265PackagingType "Use generic-lens or generic-optics with 'h265PackagingType' instead." #-}

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msosNameModifier :: Lens.Lens' MsSmoothOutputSettings (Lude.Maybe Lude.Text)
msosNameModifier = Lens.lens (nameModifier :: MsSmoothOutputSettings -> Lude.Maybe Lude.Text) (\s a -> s {nameModifier = a} :: MsSmoothOutputSettings)
{-# DEPRECATED msosNameModifier "Use generic-lens or generic-optics with 'nameModifier' instead." #-}

instance Lude.FromJSON MsSmoothOutputSettings where
  parseJSON =
    Lude.withObject
      "MsSmoothOutputSettings"
      ( \x ->
          MsSmoothOutputSettings'
            Lude.<$> (x Lude..:? "h265PackagingType")
            Lude.<*> (x Lude..:? "nameModifier")
      )

instance Lude.ToJSON MsSmoothOutputSettings where
  toJSON MsSmoothOutputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("h265PackagingType" Lude..=) Lude.<$> h265PackagingType,
            ("nameModifier" Lude..=) Lude.<$> nameModifier
          ]
      )

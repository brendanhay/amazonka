{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
  ( FrameCaptureOutputSettings (..),

    -- * Smart constructor
    mkFrameCaptureOutputSettings,

    -- * Lenses
    fcosNameModifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Frame Capture Output Settings
--
-- /See:/ 'mkFrameCaptureOutputSettings' smart constructor.
newtype FrameCaptureOutputSettings = FrameCaptureOutputSettings'
  { -- | Required if the output group contains more than one output. This modifier forms part of the output file name.
    nameModifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FrameCaptureOutputSettings' with the minimum fields required to make a request.
--
-- * 'nameModifier' - Required if the output group contains more than one output. This modifier forms part of the output file name.
mkFrameCaptureOutputSettings ::
  FrameCaptureOutputSettings
mkFrameCaptureOutputSettings =
  FrameCaptureOutputSettings' {nameModifier = Lude.Nothing}

-- | Required if the output group contains more than one output. This modifier forms part of the output file name.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcosNameModifier :: Lens.Lens' FrameCaptureOutputSettings (Lude.Maybe Lude.Text)
fcosNameModifier = Lens.lens (nameModifier :: FrameCaptureOutputSettings -> Lude.Maybe Lude.Text) (\s a -> s {nameModifier = a} :: FrameCaptureOutputSettings)
{-# DEPRECATED fcosNameModifier "Use generic-lens or generic-optics with 'nameModifier' instead." #-}

instance Lude.FromJSON FrameCaptureOutputSettings where
  parseJSON =
    Lude.withObject
      "FrameCaptureOutputSettings"
      ( \x ->
          FrameCaptureOutputSettings' Lude.<$> (x Lude..:? "nameModifier")
      )

instance Lude.ToJSON FrameCaptureOutputSettings where
  toJSON FrameCaptureOutputSettings' {..} =
    Lude.object
      (Lude.catMaybes [("nameModifier" Lude..=) Lude.<$> nameModifier])

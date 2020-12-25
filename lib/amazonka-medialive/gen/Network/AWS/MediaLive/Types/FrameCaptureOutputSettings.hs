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
import qualified Network.AWS.Prelude as Core

-- | Frame Capture Output Settings
--
-- /See:/ 'mkFrameCaptureOutputSettings' smart constructor.
newtype FrameCaptureOutputSettings = FrameCaptureOutputSettings'
  { -- | Required if the output group contains more than one output. This modifier forms part of the output file name.
    nameModifier :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FrameCaptureOutputSettings' value with any optional fields omitted.
mkFrameCaptureOutputSettings ::
  FrameCaptureOutputSettings
mkFrameCaptureOutputSettings =
  FrameCaptureOutputSettings' {nameModifier = Core.Nothing}

-- | Required if the output group contains more than one output. This modifier forms part of the output file name.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcosNameModifier :: Lens.Lens' FrameCaptureOutputSettings (Core.Maybe Core.Text)
fcosNameModifier = Lens.field @"nameModifier"
{-# DEPRECATED fcosNameModifier "Use generic-lens or generic-optics with 'nameModifier' instead." #-}

instance Core.FromJSON FrameCaptureOutputSettings where
  toJSON FrameCaptureOutputSettings {..} =
    Core.object
      (Core.catMaybes [("nameModifier" Core..=) Core.<$> nameModifier])

instance Core.FromJSON FrameCaptureOutputSettings where
  parseJSON =
    Core.withObject "FrameCaptureOutputSettings" Core.$
      \x ->
        FrameCaptureOutputSettings' Core.<$> (x Core..:? "nameModifier")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
  ( InputSwitchScheduleActionSettings (..),

    -- * Smart constructor
    mkInputSwitchScheduleActionSettings,

    -- * Lenses
    issasInputAttachmentNameReference,
    issasInputClippingSettings,
    issasUrlPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputClippingSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for the "switch input" action: to switch from ingesting one input to ingesting another input.
--
-- /See:/ 'mkInputSwitchScheduleActionSettings' smart constructor.
data InputSwitchScheduleActionSettings = InputSwitchScheduleActionSettings'
  { -- | The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
    inputAttachmentNameReference :: Core.Text,
    -- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
    inputClippingSettings :: Core.Maybe Types.InputClippingSettings,
    -- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
    urlPath :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputSwitchScheduleActionSettings' value with any optional fields omitted.
mkInputSwitchScheduleActionSettings ::
  -- | 'inputAttachmentNameReference'
  Core.Text ->
  InputSwitchScheduleActionSettings
mkInputSwitchScheduleActionSettings inputAttachmentNameReference =
  InputSwitchScheduleActionSettings'
    { inputAttachmentNameReference,
      inputClippingSettings = Core.Nothing,
      urlPath = Core.Nothing
    }

-- | The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
--
-- /Note:/ Consider using 'inputAttachmentNameReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issasInputAttachmentNameReference :: Lens.Lens' InputSwitchScheduleActionSettings Core.Text
issasInputAttachmentNameReference = Lens.field @"inputAttachmentNameReference"
{-# DEPRECATED issasInputAttachmentNameReference "Use generic-lens or generic-optics with 'inputAttachmentNameReference' instead." #-}

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /Note:/ Consider using 'inputClippingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issasInputClippingSettings :: Lens.Lens' InputSwitchScheduleActionSettings (Core.Maybe Types.InputClippingSettings)
issasInputClippingSettings = Lens.field @"inputClippingSettings"
{-# DEPRECATED issasInputClippingSettings "Use generic-lens or generic-optics with 'inputClippingSettings' instead." #-}

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
--
-- /Note:/ Consider using 'urlPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issasUrlPath :: Lens.Lens' InputSwitchScheduleActionSettings (Core.Maybe [Core.Text])
issasUrlPath = Lens.field @"urlPath"
{-# DEPRECATED issasUrlPath "Use generic-lens or generic-optics with 'urlPath' instead." #-}

instance Core.FromJSON InputSwitchScheduleActionSettings where
  toJSON InputSwitchScheduleActionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "inputAttachmentNameReference"
                  Core..= inputAttachmentNameReference
              ),
            ("inputClippingSettings" Core..=) Core.<$> inputClippingSettings,
            ("urlPath" Core..=) Core.<$> urlPath
          ]
      )

instance Core.FromJSON InputSwitchScheduleActionSettings where
  parseJSON =
    Core.withObject "InputSwitchScheduleActionSettings" Core.$
      \x ->
        InputSwitchScheduleActionSettings'
          Core.<$> (x Core..: "inputAttachmentNameReference")
          Core.<*> (x Core..:? "inputClippingSettings")
          Core.<*> (x Core..:? "urlPath")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputAttachment
  ( InputAttachment (..),

    -- * Smart constructor
    mkInputAttachment,

    -- * Lenses
    iaAutomaticInputFailoverSettings,
    iaInputAttachmentName,
    iaInputId,
    iaInputSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings as Types
import qualified Network.AWS.MediaLive.Types.InputSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for InputAttachment
--
-- /See:/ 'mkInputAttachment' smart constructor.
data InputAttachment = InputAttachment'
  { -- | User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
    automaticInputFailoverSettings :: Core.Maybe Types.AutomaticInputFailoverSettings,
    -- | User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
    inputAttachmentName :: Core.Maybe Core.Text,
    -- | The ID of the input
    inputId :: Core.Maybe Core.Text,
    -- | Settings of an input (caption selector, etc.)
    inputSettings :: Core.Maybe Types.InputSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputAttachment' value with any optional fields omitted.
mkInputAttachment ::
  InputAttachment
mkInputAttachment =
  InputAttachment'
    { automaticInputFailoverSettings = Core.Nothing,
      inputAttachmentName = Core.Nothing,
      inputId = Core.Nothing,
      inputSettings = Core.Nothing
    }

-- | User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
--
-- /Note:/ Consider using 'automaticInputFailoverSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAutomaticInputFailoverSettings :: Lens.Lens' InputAttachment (Core.Maybe Types.AutomaticInputFailoverSettings)
iaAutomaticInputFailoverSettings = Lens.field @"automaticInputFailoverSettings"
{-# DEPRECATED iaAutomaticInputFailoverSettings "Use generic-lens or generic-optics with 'automaticInputFailoverSettings' instead." #-}

-- | User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
--
-- /Note:/ Consider using 'inputAttachmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInputAttachmentName :: Lens.Lens' InputAttachment (Core.Maybe Core.Text)
iaInputAttachmentName = Lens.field @"inputAttachmentName"
{-# DEPRECATED iaInputAttachmentName "Use generic-lens or generic-optics with 'inputAttachmentName' instead." #-}

-- | The ID of the input
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInputId :: Lens.Lens' InputAttachment (Core.Maybe Core.Text)
iaInputId = Lens.field @"inputId"
{-# DEPRECATED iaInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

-- | Settings of an input (caption selector, etc.)
--
-- /Note:/ Consider using 'inputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInputSettings :: Lens.Lens' InputAttachment (Core.Maybe Types.InputSettings)
iaInputSettings = Lens.field @"inputSettings"
{-# DEPRECATED iaInputSettings "Use generic-lens or generic-optics with 'inputSettings' instead." #-}

instance Core.FromJSON InputAttachment where
  toJSON InputAttachment {..} =
    Core.object
      ( Core.catMaybes
          [ ("automaticInputFailoverSettings" Core..=)
              Core.<$> automaticInputFailoverSettings,
            ("inputAttachmentName" Core..=) Core.<$> inputAttachmentName,
            ("inputId" Core..=) Core.<$> inputId,
            ("inputSettings" Core..=) Core.<$> inputSettings
          ]
      )

instance Core.FromJSON InputAttachment where
  parseJSON =
    Core.withObject "InputAttachment" Core.$
      \x ->
        InputAttachment'
          Core.<$> (x Core..:? "automaticInputFailoverSettings")
          Core.<*> (x Core..:? "inputAttachmentName")
          Core.<*> (x Core..:? "inputId")
          Core.<*> (x Core..:? "inputSettings")

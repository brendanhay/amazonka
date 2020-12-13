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
    iaInputAttachmentName,
    iaInputId,
    iaAutomaticInputFailoverSettings,
    iaInputSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
import Network.AWS.MediaLive.Types.InputSettings
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for InputAttachment
--
-- /See:/ 'mkInputAttachment' smart constructor.
data InputAttachment = InputAttachment'
  { -- | User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
    inputAttachmentName :: Lude.Maybe Lude.Text,
    -- | The ID of the input
    inputId :: Lude.Maybe Lude.Text,
    -- | User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
    automaticInputFailoverSettings :: Lude.Maybe AutomaticInputFailoverSettings,
    -- | Settings of an input (caption selector, etc.)
    inputSettings :: Lude.Maybe InputSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputAttachment' with the minimum fields required to make a request.
--
-- * 'inputAttachmentName' - User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
-- * 'inputId' - The ID of the input
-- * 'automaticInputFailoverSettings' - User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
-- * 'inputSettings' - Settings of an input (caption selector, etc.)
mkInputAttachment ::
  InputAttachment
mkInputAttachment =
  InputAttachment'
    { inputAttachmentName = Lude.Nothing,
      inputId = Lude.Nothing,
      automaticInputFailoverSettings = Lude.Nothing,
      inputSettings = Lude.Nothing
    }

-- | User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
--
-- /Note:/ Consider using 'inputAttachmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInputAttachmentName :: Lens.Lens' InputAttachment (Lude.Maybe Lude.Text)
iaInputAttachmentName = Lens.lens (inputAttachmentName :: InputAttachment -> Lude.Maybe Lude.Text) (\s a -> s {inputAttachmentName = a} :: InputAttachment)
{-# DEPRECATED iaInputAttachmentName "Use generic-lens or generic-optics with 'inputAttachmentName' instead." #-}

-- | The ID of the input
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInputId :: Lens.Lens' InputAttachment (Lude.Maybe Lude.Text)
iaInputId = Lens.lens (inputId :: InputAttachment -> Lude.Maybe Lude.Text) (\s a -> s {inputId = a} :: InputAttachment)
{-# DEPRECATED iaInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

-- | User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
--
-- /Note:/ Consider using 'automaticInputFailoverSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAutomaticInputFailoverSettings :: Lens.Lens' InputAttachment (Lude.Maybe AutomaticInputFailoverSettings)
iaAutomaticInputFailoverSettings = Lens.lens (automaticInputFailoverSettings :: InputAttachment -> Lude.Maybe AutomaticInputFailoverSettings) (\s a -> s {automaticInputFailoverSettings = a} :: InputAttachment)
{-# DEPRECATED iaAutomaticInputFailoverSettings "Use generic-lens or generic-optics with 'automaticInputFailoverSettings' instead." #-}

-- | Settings of an input (caption selector, etc.)
--
-- /Note:/ Consider using 'inputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInputSettings :: Lens.Lens' InputAttachment (Lude.Maybe InputSettings)
iaInputSettings = Lens.lens (inputSettings :: InputAttachment -> Lude.Maybe InputSettings) (\s a -> s {inputSettings = a} :: InputAttachment)
{-# DEPRECATED iaInputSettings "Use generic-lens or generic-optics with 'inputSettings' instead." #-}

instance Lude.FromJSON InputAttachment where
  parseJSON =
    Lude.withObject
      "InputAttachment"
      ( \x ->
          InputAttachment'
            Lude.<$> (x Lude..:? "inputAttachmentName")
            Lude.<*> (x Lude..:? "inputId")
            Lude.<*> (x Lude..:? "automaticInputFailoverSettings")
            Lude.<*> (x Lude..:? "inputSettings")
      )

instance Lude.ToJSON InputAttachment where
  toJSON InputAttachment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputAttachmentName" Lude..=) Lude.<$> inputAttachmentName,
            ("inputId" Lude..=) Lude.<$> inputId,
            ("automaticInputFailoverSettings" Lude..=)
              Lude.<$> automaticInputFailoverSettings,
            ("inputSettings" Lude..=) Lude.<$> inputSettings
          ]
      )

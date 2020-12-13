{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateConfiguration
  ( TemplateConfiguration (..),

    -- * Smart constructor
    mkTemplateConfiguration,

    -- * Lenses
    tcSMSTemplate,
    tcVoiceTemplate,
    tcPushTemplate,
    tcEmailTemplate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Template
import qualified Network.AWS.Prelude as Lude

-- | Specifies the message template to use for the message, for each type of channel.
--
-- /See:/ 'mkTemplateConfiguration' smart constructor.
data TemplateConfiguration = TemplateConfiguration'
  { -- | The SMS template to use for the message.
    sMSTemplate :: Lude.Maybe Template,
    -- | The voice template to use for the message. This object isn't supported for campaigns.
    voiceTemplate :: Lude.Maybe Template,
    -- | The push notification template to use for the message.
    pushTemplate :: Lude.Maybe Template,
    -- | The email template to use for the message.
    emailTemplate :: Lude.Maybe Template
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplateConfiguration' with the minimum fields required to make a request.
--
-- * 'sMSTemplate' - The SMS template to use for the message.
-- * 'voiceTemplate' - The voice template to use for the message. This object isn't supported for campaigns.
-- * 'pushTemplate' - The push notification template to use for the message.
-- * 'emailTemplate' - The email template to use for the message.
mkTemplateConfiguration ::
  TemplateConfiguration
mkTemplateConfiguration =
  TemplateConfiguration'
    { sMSTemplate = Lude.Nothing,
      voiceTemplate = Lude.Nothing,
      pushTemplate = Lude.Nothing,
      emailTemplate = Lude.Nothing
    }

-- | The SMS template to use for the message.
--
-- /Note:/ Consider using 'sMSTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSMSTemplate :: Lens.Lens' TemplateConfiguration (Lude.Maybe Template)
tcSMSTemplate = Lens.lens (sMSTemplate :: TemplateConfiguration -> Lude.Maybe Template) (\s a -> s {sMSTemplate = a} :: TemplateConfiguration)
{-# DEPRECATED tcSMSTemplate "Use generic-lens or generic-optics with 'sMSTemplate' instead." #-}

-- | The voice template to use for the message. This object isn't supported for campaigns.
--
-- /Note:/ Consider using 'voiceTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcVoiceTemplate :: Lens.Lens' TemplateConfiguration (Lude.Maybe Template)
tcVoiceTemplate = Lens.lens (voiceTemplate :: TemplateConfiguration -> Lude.Maybe Template) (\s a -> s {voiceTemplate = a} :: TemplateConfiguration)
{-# DEPRECATED tcVoiceTemplate "Use generic-lens or generic-optics with 'voiceTemplate' instead." #-}

-- | The push notification template to use for the message.
--
-- /Note:/ Consider using 'pushTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcPushTemplate :: Lens.Lens' TemplateConfiguration (Lude.Maybe Template)
tcPushTemplate = Lens.lens (pushTemplate :: TemplateConfiguration -> Lude.Maybe Template) (\s a -> s {pushTemplate = a} :: TemplateConfiguration)
{-# DEPRECATED tcPushTemplate "Use generic-lens or generic-optics with 'pushTemplate' instead." #-}

-- | The email template to use for the message.
--
-- /Note:/ Consider using 'emailTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEmailTemplate :: Lens.Lens' TemplateConfiguration (Lude.Maybe Template)
tcEmailTemplate = Lens.lens (emailTemplate :: TemplateConfiguration -> Lude.Maybe Template) (\s a -> s {emailTemplate = a} :: TemplateConfiguration)
{-# DEPRECATED tcEmailTemplate "Use generic-lens or generic-optics with 'emailTemplate' instead." #-}

instance Lude.FromJSON TemplateConfiguration where
  parseJSON =
    Lude.withObject
      "TemplateConfiguration"
      ( \x ->
          TemplateConfiguration'
            Lude.<$> (x Lude..:? "SMSTemplate")
            Lude.<*> (x Lude..:? "VoiceTemplate")
            Lude.<*> (x Lude..:? "PushTemplate")
            Lude.<*> (x Lude..:? "EmailTemplate")
      )

instance Lude.ToJSON TemplateConfiguration where
  toJSON TemplateConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SMSTemplate" Lude..=) Lude.<$> sMSTemplate,
            ("VoiceTemplate" Lude..=) Lude.<$> voiceTemplate,
            ("PushTemplate" Lude..=) Lude.<$> pushTemplate,
            ("EmailTemplate" Lude..=) Lude.<$> emailTemplate
          ]
      )

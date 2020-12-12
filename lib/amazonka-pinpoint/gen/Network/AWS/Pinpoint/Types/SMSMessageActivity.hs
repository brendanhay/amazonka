{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSMessageActivity
  ( SMSMessageActivity (..),

    -- * Smart constructor
    mkSMSMessageActivity,

    -- * Lenses
    smsmaTemplateName,
    smsmaTemplateVersion,
    smsmaNextActivity,
    smsmaMessageConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.JourneySMSMessage
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for an SMS activity in a journey. This type of activity sends a text message to participants.
--
-- /See:/ 'mkSMSMessageActivity' smart constructor.
data SMSMessageActivity = SMSMessageActivity'
  { templateName ::
      Lude.Maybe Lude.Text,
    templateVersion :: Lude.Maybe Lude.Text,
    nextActivity :: Lude.Maybe Lude.Text,
    messageConfig :: Lude.Maybe JourneySMSMessage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMSMessageActivity' with the minimum fields required to make a request.
--
-- * 'messageConfig' - Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
-- * 'nextActivity' - The unique identifier for the next activity to perform, after the message is sent.
-- * 'templateName' - The name of the SMS message template to use for the message. If specified, this value must match the name of an existing message template.
-- * 'templateVersion' - The unique identifier for the version of the SMS template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
mkSMSMessageActivity ::
  SMSMessageActivity
mkSMSMessageActivity =
  SMSMessageActivity'
    { templateName = Lude.Nothing,
      templateVersion = Lude.Nothing,
      nextActivity = Lude.Nothing,
      messageConfig = Lude.Nothing
    }

-- | The name of the SMS message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaTemplateName :: Lens.Lens' SMSMessageActivity (Lude.Maybe Lude.Text)
smsmaTemplateName = Lens.lens (templateName :: SMSMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: SMSMessageActivity)
{-# DEPRECATED smsmaTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The unique identifier for the version of the SMS template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaTemplateVersion :: Lens.Lens' SMSMessageActivity (Lude.Maybe Lude.Text)
smsmaTemplateVersion = Lens.lens (templateVersion :: SMSMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateVersion = a} :: SMSMessageActivity)
{-# DEPRECATED smsmaTemplateVersion "Use generic-lens or generic-optics with 'templateVersion' instead." #-}

-- | The unique identifier for the next activity to perform, after the message is sent.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaNextActivity :: Lens.Lens' SMSMessageActivity (Lude.Maybe Lude.Text)
smsmaNextActivity = Lens.lens (nextActivity :: SMSMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: SMSMessageActivity)
{-# DEPRECATED smsmaNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaMessageConfig :: Lens.Lens' SMSMessageActivity (Lude.Maybe JourneySMSMessage)
smsmaMessageConfig = Lens.lens (messageConfig :: SMSMessageActivity -> Lude.Maybe JourneySMSMessage) (\s a -> s {messageConfig = a} :: SMSMessageActivity)
{-# DEPRECATED smsmaMessageConfig "Use generic-lens or generic-optics with 'messageConfig' instead." #-}

instance Lude.FromJSON SMSMessageActivity where
  parseJSON =
    Lude.withObject
      "SMSMessageActivity"
      ( \x ->
          SMSMessageActivity'
            Lude.<$> (x Lude..:? "TemplateName")
            Lude.<*> (x Lude..:? "TemplateVersion")
            Lude.<*> (x Lude..:? "NextActivity")
            Lude.<*> (x Lude..:? "MessageConfig")
      )

instance Lude.ToJSON SMSMessageActivity where
  toJSON SMSMessageActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TemplateName" Lude..=) Lude.<$> templateName,
            ("TemplateVersion" Lude..=) Lude.<$> templateVersion,
            ("NextActivity" Lude..=) Lude.<$> nextActivity,
            ("MessageConfig" Lude..=) Lude.<$> messageConfig
          ]
      )

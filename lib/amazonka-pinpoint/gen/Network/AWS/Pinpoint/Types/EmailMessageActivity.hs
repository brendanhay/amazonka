{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailMessageActivity
  ( EmailMessageActivity (..),

    -- * Smart constructor
    mkEmailMessageActivity,

    -- * Lenses
    emaTemplateName,
    emaTemplateVersion,
    emaNextActivity,
    emaMessageConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.JourneyEmailMessage
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for an email activity in a journey. This type of activity sends an email message to participants.
--
-- /See:/ 'mkEmailMessageActivity' smart constructor.
data EmailMessageActivity = EmailMessageActivity'
  { templateName ::
      Lude.Maybe Lude.Text,
    templateVersion :: Lude.Maybe Lude.Text,
    nextActivity :: Lude.Maybe Lude.Text,
    messageConfig :: Lude.Maybe JourneyEmailMessage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmailMessageActivity' with the minimum fields required to make a request.
--
-- * 'messageConfig' - Specifies the sender address for an email message that's sent to participants in the journey.
-- * 'nextActivity' - The unique identifier for the next activity to perform, after the message is sent.
-- * 'templateName' - The name of the email message template to use for the message. If specified, this value must match the name of an existing message template.
-- * 'templateVersion' - The unique identifier for the version of the email template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
mkEmailMessageActivity ::
  EmailMessageActivity
mkEmailMessageActivity =
  EmailMessageActivity'
    { templateName = Lude.Nothing,
      templateVersion = Lude.Nothing,
      nextActivity = Lude.Nothing,
      messageConfig = Lude.Nothing
    }

-- | The name of the email message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaTemplateName :: Lens.Lens' EmailMessageActivity (Lude.Maybe Lude.Text)
emaTemplateName = Lens.lens (templateName :: EmailMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: EmailMessageActivity)
{-# DEPRECATED emaTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The unique identifier for the version of the email template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaTemplateVersion :: Lens.Lens' EmailMessageActivity (Lude.Maybe Lude.Text)
emaTemplateVersion = Lens.lens (templateVersion :: EmailMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateVersion = a} :: EmailMessageActivity)
{-# DEPRECATED emaTemplateVersion "Use generic-lens or generic-optics with 'templateVersion' instead." #-}

-- | The unique identifier for the next activity to perform, after the message is sent.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaNextActivity :: Lens.Lens' EmailMessageActivity (Lude.Maybe Lude.Text)
emaNextActivity = Lens.lens (nextActivity :: EmailMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: EmailMessageActivity)
{-# DEPRECATED emaNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | Specifies the sender address for an email message that's sent to participants in the journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaMessageConfig :: Lens.Lens' EmailMessageActivity (Lude.Maybe JourneyEmailMessage)
emaMessageConfig = Lens.lens (messageConfig :: EmailMessageActivity -> Lude.Maybe JourneyEmailMessage) (\s a -> s {messageConfig = a} :: EmailMessageActivity)
{-# DEPRECATED emaMessageConfig "Use generic-lens or generic-optics with 'messageConfig' instead." #-}

instance Lude.FromJSON EmailMessageActivity where
  parseJSON =
    Lude.withObject
      "EmailMessageActivity"
      ( \x ->
          EmailMessageActivity'
            Lude.<$> (x Lude..:? "TemplateName")
            Lude.<*> (x Lude..:? "TemplateVersion")
            Lude.<*> (x Lude..:? "NextActivity")
            Lude.<*> (x Lude..:? "MessageConfig")
      )

instance Lude.ToJSON EmailMessageActivity where
  toJSON EmailMessageActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TemplateName" Lude..=) Lude.<$> templateName,
            ("TemplateVersion" Lude..=) Lude.<$> templateVersion,
            ("NextActivity" Lude..=) Lude.<$> nextActivity,
            ("MessageConfig" Lude..=) Lude.<$> messageConfig
          ]
      )

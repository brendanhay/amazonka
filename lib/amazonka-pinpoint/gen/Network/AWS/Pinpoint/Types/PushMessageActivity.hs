{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PushMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushMessageActivity
  ( PushMessageActivity (..),

    -- * Smart constructor
    mkPushMessageActivity,

    -- * Lenses
    pmaTemplateName,
    pmaTemplateVersion,
    pmaNextActivity,
    pmaMessageConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.JourneyPushMessage
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a push notification activity in a journey. This type of activity sends a push notification to participants.
--
-- /See:/ 'mkPushMessageActivity' smart constructor.
data PushMessageActivity = PushMessageActivity'
  { templateName ::
      Lude.Maybe Lude.Text,
    templateVersion :: Lude.Maybe Lude.Text,
    nextActivity :: Lude.Maybe Lude.Text,
    messageConfig :: Lude.Maybe JourneyPushMessage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PushMessageActivity' with the minimum fields required to make a request.
--
-- * 'messageConfig' - Specifies the time to live (TTL) value for push notifications that are sent to participants in a journey.
-- * 'nextActivity' - The unique identifier for the next activity to perform, after the message is sent.
-- * 'templateName' - The name of the push notification template to use for the message. If specified, this value must match the name of an existing message template.
-- * 'templateVersion' - The unique identifier for the version of the push notification template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
mkPushMessageActivity ::
  PushMessageActivity
mkPushMessageActivity =
  PushMessageActivity'
    { templateName = Lude.Nothing,
      templateVersion = Lude.Nothing,
      nextActivity = Lude.Nothing,
      messageConfig = Lude.Nothing
    }

-- | The name of the push notification template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTemplateName :: Lens.Lens' PushMessageActivity (Lude.Maybe Lude.Text)
pmaTemplateName = Lens.lens (templateName :: PushMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: PushMessageActivity)
{-# DEPRECATED pmaTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The unique identifier for the version of the push notification template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTemplateVersion :: Lens.Lens' PushMessageActivity (Lude.Maybe Lude.Text)
pmaTemplateVersion = Lens.lens (templateVersion :: PushMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateVersion = a} :: PushMessageActivity)
{-# DEPRECATED pmaTemplateVersion "Use generic-lens or generic-optics with 'templateVersion' instead." #-}

-- | The unique identifier for the next activity to perform, after the message is sent.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaNextActivity :: Lens.Lens' PushMessageActivity (Lude.Maybe Lude.Text)
pmaNextActivity = Lens.lens (nextActivity :: PushMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: PushMessageActivity)
{-# DEPRECATED pmaNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | Specifies the time to live (TTL) value for push notifications that are sent to participants in a journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaMessageConfig :: Lens.Lens' PushMessageActivity (Lude.Maybe JourneyPushMessage)
pmaMessageConfig = Lens.lens (messageConfig :: PushMessageActivity -> Lude.Maybe JourneyPushMessage) (\s a -> s {messageConfig = a} :: PushMessageActivity)
{-# DEPRECATED pmaMessageConfig "Use generic-lens or generic-optics with 'messageConfig' instead." #-}

instance Lude.FromJSON PushMessageActivity where
  parseJSON =
    Lude.withObject
      "PushMessageActivity"
      ( \x ->
          PushMessageActivity'
            Lude.<$> (x Lude..:? "TemplateName")
            Lude.<*> (x Lude..:? "TemplateVersion")
            Lude.<*> (x Lude..:? "NextActivity")
            Lude.<*> (x Lude..:? "MessageConfig")
      )

instance Lude.ToJSON PushMessageActivity where
  toJSON PushMessageActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TemplateName" Lude..=) Lude.<$> templateName,
            ("TemplateVersion" Lude..=) Lude.<$> templateVersion,
            ("NextActivity" Lude..=) Lude.<$> nextActivity,
            ("MessageConfig" Lude..=) Lude.<$> messageConfig
          ]
      )

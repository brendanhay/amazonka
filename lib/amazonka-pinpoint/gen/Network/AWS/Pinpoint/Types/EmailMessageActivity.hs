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
    emaMessageConfig,
    emaNextActivity,
    emaTemplateName,
    emaTemplateVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.JourneyEmailMessage as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for an email activity in a journey. This type of activity sends an email message to participants.
--
-- /See:/ 'mkEmailMessageActivity' smart constructor.
data EmailMessageActivity = EmailMessageActivity'
  { -- | Specifies the sender address for an email message that's sent to participants in the journey.
    messageConfig :: Core.Maybe Types.JourneyEmailMessage,
    -- | The unique identifier for the next activity to perform, after the message is sent.
    nextActivity :: Core.Maybe Core.Text,
    -- | The name of the email message template to use for the message. If specified, this value must match the name of an existing message template.
    templateName :: Core.Maybe Core.Text,
    -- | The unique identifier for the version of the email template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
    --
    -- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
    templateVersion :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmailMessageActivity' value with any optional fields omitted.
mkEmailMessageActivity ::
  EmailMessageActivity
mkEmailMessageActivity =
  EmailMessageActivity'
    { messageConfig = Core.Nothing,
      nextActivity = Core.Nothing,
      templateName = Core.Nothing,
      templateVersion = Core.Nothing
    }

-- | Specifies the sender address for an email message that's sent to participants in the journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaMessageConfig :: Lens.Lens' EmailMessageActivity (Core.Maybe Types.JourneyEmailMessage)
emaMessageConfig = Lens.field @"messageConfig"
{-# DEPRECATED emaMessageConfig "Use generic-lens or generic-optics with 'messageConfig' instead." #-}

-- | The unique identifier for the next activity to perform, after the message is sent.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaNextActivity :: Lens.Lens' EmailMessageActivity (Core.Maybe Core.Text)
emaNextActivity = Lens.field @"nextActivity"
{-# DEPRECATED emaNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The name of the email message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaTemplateName :: Lens.Lens' EmailMessageActivity (Core.Maybe Core.Text)
emaTemplateName = Lens.field @"templateName"
{-# DEPRECATED emaTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The unique identifier for the version of the email template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emaTemplateVersion :: Lens.Lens' EmailMessageActivity (Core.Maybe Core.Text)
emaTemplateVersion = Lens.field @"templateVersion"
{-# DEPRECATED emaTemplateVersion "Use generic-lens or generic-optics with 'templateVersion' instead." #-}

instance Core.FromJSON EmailMessageActivity where
  toJSON EmailMessageActivity {..} =
    Core.object
      ( Core.catMaybes
          [ ("MessageConfig" Core..=) Core.<$> messageConfig,
            ("NextActivity" Core..=) Core.<$> nextActivity,
            ("TemplateName" Core..=) Core.<$> templateName,
            ("TemplateVersion" Core..=) Core.<$> templateVersion
          ]
      )

instance Core.FromJSON EmailMessageActivity where
  parseJSON =
    Core.withObject "EmailMessageActivity" Core.$
      \x ->
        EmailMessageActivity'
          Core.<$> (x Core..:? "MessageConfig")
          Core.<*> (x Core..:? "NextActivity")
          Core.<*> (x Core..:? "TemplateName")
          Core.<*> (x Core..:? "TemplateVersion")

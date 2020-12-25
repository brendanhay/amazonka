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
    pmaMessageConfig,
    pmaNextActivity,
    pmaTemplateName,
    pmaTemplateVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.JourneyPushMessage as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a push notification activity in a journey. This type of activity sends a push notification to participants.
--
-- /See:/ 'mkPushMessageActivity' smart constructor.
data PushMessageActivity = PushMessageActivity'
  { -- | Specifies the time to live (TTL) value for push notifications that are sent to participants in a journey.
    messageConfig :: Core.Maybe Types.JourneyPushMessage,
    -- | The unique identifier for the next activity to perform, after the message is sent.
    nextActivity :: Core.Maybe Core.Text,
    -- | The name of the push notification template to use for the message. If specified, this value must match the name of an existing message template.
    templateName :: Core.Maybe Core.Text,
    -- | The unique identifier for the version of the push notification template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
    --
    -- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
    templateVersion :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PushMessageActivity' value with any optional fields omitted.
mkPushMessageActivity ::
  PushMessageActivity
mkPushMessageActivity =
  PushMessageActivity'
    { messageConfig = Core.Nothing,
      nextActivity = Core.Nothing,
      templateName = Core.Nothing,
      templateVersion = Core.Nothing
    }

-- | Specifies the time to live (TTL) value for push notifications that are sent to participants in a journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaMessageConfig :: Lens.Lens' PushMessageActivity (Core.Maybe Types.JourneyPushMessage)
pmaMessageConfig = Lens.field @"messageConfig"
{-# DEPRECATED pmaMessageConfig "Use generic-lens or generic-optics with 'messageConfig' instead." #-}

-- | The unique identifier for the next activity to perform, after the message is sent.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaNextActivity :: Lens.Lens' PushMessageActivity (Core.Maybe Core.Text)
pmaNextActivity = Lens.field @"nextActivity"
{-# DEPRECATED pmaNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The name of the push notification template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTemplateName :: Lens.Lens' PushMessageActivity (Core.Maybe Core.Text)
pmaTemplateName = Lens.field @"templateName"
{-# DEPRECATED pmaTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The unique identifier for the version of the push notification template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTemplateVersion :: Lens.Lens' PushMessageActivity (Core.Maybe Core.Text)
pmaTemplateVersion = Lens.field @"templateVersion"
{-# DEPRECATED pmaTemplateVersion "Use generic-lens or generic-optics with 'templateVersion' instead." #-}

instance Core.FromJSON PushMessageActivity where
  toJSON PushMessageActivity {..} =
    Core.object
      ( Core.catMaybes
          [ ("MessageConfig" Core..=) Core.<$> messageConfig,
            ("NextActivity" Core..=) Core.<$> nextActivity,
            ("TemplateName" Core..=) Core.<$> templateName,
            ("TemplateVersion" Core..=) Core.<$> templateVersion
          ]
      )

instance Core.FromJSON PushMessageActivity where
  parseJSON =
    Core.withObject "PushMessageActivity" Core.$
      \x ->
        PushMessageActivity'
          Core.<$> (x Core..:? "MessageConfig")
          Core.<*> (x Core..:? "NextActivity")
          Core.<*> (x Core..:? "TemplateName")
          Core.<*> (x Core..:? "TemplateVersion")

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SMSMessageActivity
  ( SMSMessageActivity (..)
  -- * Smart constructor
  , mkSMSMessageActivity
  -- * Lenses
  , smsmaMessageConfig
  , smsmaNextActivity
  , smsmaTemplateName
  , smsmaTemplateVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.JourneySMSMessage as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for an SMS activity in a journey. This type of activity sends a text message to participants.
--
-- /See:/ 'mkSMSMessageActivity' smart constructor.
data SMSMessageActivity = SMSMessageActivity'
  { messageConfig :: Core.Maybe Types.JourneySMSMessage
    -- ^ Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
  , nextActivity :: Core.Maybe Core.Text
    -- ^ The unique identifier for the next activity to perform, after the message is sent.
  , templateName :: Core.Maybe Core.Text
    -- ^ The name of the SMS message template to use for the message. If specified, this value must match the name of an existing message template.
  , templateVersion :: Core.Maybe Core.Text
    -- ^ The unique identifier for the version of the SMS template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SMSMessageActivity' value with any optional fields omitted.
mkSMSMessageActivity
    :: SMSMessageActivity
mkSMSMessageActivity
  = SMSMessageActivity'{messageConfig = Core.Nothing,
                        nextActivity = Core.Nothing, templateName = Core.Nothing,
                        templateVersion = Core.Nothing}

-- | Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaMessageConfig :: Lens.Lens' SMSMessageActivity (Core.Maybe Types.JourneySMSMessage)
smsmaMessageConfig = Lens.field @"messageConfig"
{-# INLINEABLE smsmaMessageConfig #-}
{-# DEPRECATED messageConfig "Use generic-lens or generic-optics with 'messageConfig' instead"  #-}

-- | The unique identifier for the next activity to perform, after the message is sent.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaNextActivity :: Lens.Lens' SMSMessageActivity (Core.Maybe Core.Text)
smsmaNextActivity = Lens.field @"nextActivity"
{-# INLINEABLE smsmaNextActivity #-}
{-# DEPRECATED nextActivity "Use generic-lens or generic-optics with 'nextActivity' instead"  #-}

-- | The name of the SMS message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaTemplateName :: Lens.Lens' SMSMessageActivity (Core.Maybe Core.Text)
smsmaTemplateName = Lens.field @"templateName"
{-# INLINEABLE smsmaTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The unique identifier for the version of the SMS template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmaTemplateVersion :: Lens.Lens' SMSMessageActivity (Core.Maybe Core.Text)
smsmaTemplateVersion = Lens.field @"templateVersion"
{-# INLINEABLE smsmaTemplateVersion #-}
{-# DEPRECATED templateVersion "Use generic-lens or generic-optics with 'templateVersion' instead"  #-}

instance Core.FromJSON SMSMessageActivity where
        toJSON SMSMessageActivity{..}
          = Core.object
              (Core.catMaybes
                 [("MessageConfig" Core..=) Core.<$> messageConfig,
                  ("NextActivity" Core..=) Core.<$> nextActivity,
                  ("TemplateName" Core..=) Core.<$> templateName,
                  ("TemplateVersion" Core..=) Core.<$> templateVersion])

instance Core.FromJSON SMSMessageActivity where
        parseJSON
          = Core.withObject "SMSMessageActivity" Core.$
              \ x ->
                SMSMessageActivity' Core.<$>
                  (x Core..:? "MessageConfig") Core.<*> x Core..:? "NextActivity"
                    Core.<*> x Core..:? "TemplateName"
                    Core.<*> x Core..:? "TemplateVersion"

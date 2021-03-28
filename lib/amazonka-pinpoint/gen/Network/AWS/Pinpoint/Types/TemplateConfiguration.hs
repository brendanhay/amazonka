{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.TemplateConfiguration
  ( TemplateConfiguration (..)
  -- * Smart constructor
  , mkTemplateConfiguration
  -- * Lenses
  , tcEmailTemplate
  , tcPushTemplate
  , tcSMSTemplate
  , tcVoiceTemplate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Template as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the message template to use for the message, for each type of channel.
--
-- /See:/ 'mkTemplateConfiguration' smart constructor.
data TemplateConfiguration = TemplateConfiguration'
  { emailTemplate :: Core.Maybe Types.Template
    -- ^ The email template to use for the message.
  , pushTemplate :: Core.Maybe Types.Template
    -- ^ The push notification template to use for the message.
  , sMSTemplate :: Core.Maybe Types.Template
    -- ^ The SMS template to use for the message.
  , voiceTemplate :: Core.Maybe Types.Template
    -- ^ The voice template to use for the message. This object isn't supported for campaigns.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemplateConfiguration' value with any optional fields omitted.
mkTemplateConfiguration
    :: TemplateConfiguration
mkTemplateConfiguration
  = TemplateConfiguration'{emailTemplate = Core.Nothing,
                           pushTemplate = Core.Nothing, sMSTemplate = Core.Nothing,
                           voiceTemplate = Core.Nothing}

-- | The email template to use for the message.
--
-- /Note:/ Consider using 'emailTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEmailTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Types.Template)
tcEmailTemplate = Lens.field @"emailTemplate"
{-# INLINEABLE tcEmailTemplate #-}
{-# DEPRECATED emailTemplate "Use generic-lens or generic-optics with 'emailTemplate' instead"  #-}

-- | The push notification template to use for the message.
--
-- /Note:/ Consider using 'pushTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcPushTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Types.Template)
tcPushTemplate = Lens.field @"pushTemplate"
{-# INLINEABLE tcPushTemplate #-}
{-# DEPRECATED pushTemplate "Use generic-lens or generic-optics with 'pushTemplate' instead"  #-}

-- | The SMS template to use for the message.
--
-- /Note:/ Consider using 'sMSTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSMSTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Types.Template)
tcSMSTemplate = Lens.field @"sMSTemplate"
{-# INLINEABLE tcSMSTemplate #-}
{-# DEPRECATED sMSTemplate "Use generic-lens or generic-optics with 'sMSTemplate' instead"  #-}

-- | The voice template to use for the message. This object isn't supported for campaigns.
--
-- /Note:/ Consider using 'voiceTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcVoiceTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Types.Template)
tcVoiceTemplate = Lens.field @"voiceTemplate"
{-# INLINEABLE tcVoiceTemplate #-}
{-# DEPRECATED voiceTemplate "Use generic-lens or generic-optics with 'voiceTemplate' instead"  #-}

instance Core.FromJSON TemplateConfiguration where
        toJSON TemplateConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("EmailTemplate" Core..=) Core.<$> emailTemplate,
                  ("PushTemplate" Core..=) Core.<$> pushTemplate,
                  ("SMSTemplate" Core..=) Core.<$> sMSTemplate,
                  ("VoiceTemplate" Core..=) Core.<$> voiceTemplate])

instance Core.FromJSON TemplateConfiguration where
        parseJSON
          = Core.withObject "TemplateConfiguration" Core.$
              \ x ->
                TemplateConfiguration' Core.<$>
                  (x Core..:? "EmailTemplate") Core.<*> x Core..:? "PushTemplate"
                    Core.<*> x Core..:? "SMSTemplate"
                    Core.<*> x Core..:? "VoiceTemplate"

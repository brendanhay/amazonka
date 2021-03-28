{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.TemplateResponse
  ( TemplateResponse (..)
  -- * Smart constructor
  , mkTemplateResponse
  -- * Lenses
  , trLastModifiedDate
  , trCreationDate
  , trTemplateName
  , trTemplateType
  , trArn
  , trDefaultSubstitutions
  , trTemplateDescription
  , trVersion
  , trTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.TemplateType as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about a message template that's associated with your Amazon Pinpoint account.
--
-- /See:/ 'mkTemplateResponse' smart constructor.
data TemplateResponse = TemplateResponse'
  { lastModifiedDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the message template was last modified.
  , creationDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the message template was created.
  , templateName :: Core.Text
    -- ^ The name of the message template.
  , templateType :: Types.TemplateType
    -- ^ The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
  , arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
  , defaultSubstitutions :: Core.Maybe Core.Text
    -- ^ The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
  , templateDescription :: Core.Maybe Core.Text
    -- ^ The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
  , version :: Core.Maybe Core.Text
    -- ^ The unique identifier, as an integer, for the active version of the message template.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemplateResponse' value with any optional fields omitted.
mkTemplateResponse
    :: Core.Text -- ^ 'lastModifiedDate'
    -> Core.Text -- ^ 'creationDate'
    -> Core.Text -- ^ 'templateName'
    -> Types.TemplateType -- ^ 'templateType'
    -> TemplateResponse
mkTemplateResponse lastModifiedDate creationDate templateName
  templateType
  = TemplateResponse'{lastModifiedDate, creationDate, templateName,
                      templateType, arn = Core.Nothing,
                      defaultSubstitutions = Core.Nothing,
                      templateDescription = Core.Nothing, version = Core.Nothing,
                      tags = Core.Nothing}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trLastModifiedDate :: Lens.Lens' TemplateResponse Core.Text
trLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE trLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trCreationDate :: Lens.Lens' TemplateResponse Core.Text
trCreationDate = Lens.field @"creationDate"
{-# INLINEABLE trCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTemplateName :: Lens.Lens' TemplateResponse Core.Text
trTemplateName = Lens.field @"templateName"
{-# INLINEABLE trTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTemplateType :: Lens.Lens' TemplateResponse Types.TemplateType
trTemplateType = Lens.field @"templateType"
{-# INLINEABLE trTemplateType #-}
{-# DEPRECATED templateType "Use generic-lens or generic-optics with 'templateType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trArn :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
trArn = Lens.field @"arn"
{-# INLINEABLE trArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trDefaultSubstitutions :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
trDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# INLINEABLE trDefaultSubstitutions #-}
{-# DEPRECATED defaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead"  #-}

-- | The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTemplateDescription :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
trTemplateDescription = Lens.field @"templateDescription"
{-# INLINEABLE trTemplateDescription #-}
{-# DEPRECATED templateDescription "Use generic-lens or generic-optics with 'templateDescription' instead"  #-}

-- | The unique identifier, as an integer, for the active version of the message template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trVersion :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
trVersion = Lens.field @"version"
{-# INLINEABLE trVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
trTags = Lens.field @"tags"
{-# INLINEABLE trTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON TemplateResponse where
        parseJSON
          = Core.withObject "TemplateResponse" Core.$
              \ x ->
                TemplateResponse' Core.<$>
                  (x Core..: "LastModifiedDate") Core.<*> x Core..: "CreationDate"
                    Core.<*> x Core..: "TemplateName"
                    Core.<*> x Core..: "TemplateType"
                    Core.<*> x Core..:? "Arn"
                    Core.<*> x Core..:? "DefaultSubstitutions"
                    Core.<*> x Core..:? "TemplateDescription"
                    Core.<*> x Core..:? "Version"
                    Core.<*> x Core..:? "tags"

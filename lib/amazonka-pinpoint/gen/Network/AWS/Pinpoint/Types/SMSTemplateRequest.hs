{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSTemplateRequest
  ( SMSTemplateRequest (..),

    -- * Smart constructor
    mkSMSTemplateRequest,

    -- * Lenses
    smstrBody,
    smstrDefaultSubstitutions,
    smstrRecommenderId,
    smstrTemplateDescription,
    smstrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the content and settings for a message template that can be used in text messages that are sent through the SMS channel.
--
-- /See:/ 'mkSMSTemplateRequest' smart constructor.
data SMSTemplateRequest = SMSTemplateRequest'
  { -- | The message body to use in text messages that are based on the message template.
    body :: Core.Maybe Core.Text,
    -- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
    recommenderId :: Core.Maybe Core.Text,
    -- | A custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SMSTemplateRequest' value with any optional fields omitted.
mkSMSTemplateRequest ::
  SMSTemplateRequest
mkSMSTemplateRequest =
  SMSTemplateRequest'
    { body = Core.Nothing,
      defaultSubstitutions = Core.Nothing,
      recommenderId = Core.Nothing,
      templateDescription = Core.Nothing,
      tags = Core.Nothing
    }

-- | The message body to use in text messages that are based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrBody :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
smstrBody = Lens.field @"body"
{-# DEPRECATED smstrBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrDefaultSubstitutions :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
smstrDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# DEPRECATED smstrDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrRecommenderId :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
smstrRecommenderId = Lens.field @"recommenderId"
{-# DEPRECATED smstrRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrTemplateDescription :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
smstrTemplateDescription = Lens.field @"templateDescription"
{-# DEPRECATED smstrTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrTags :: Lens.Lens' SMSTemplateRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
smstrTags = Lens.field @"tags"
{-# DEPRECATED smstrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON SMSTemplateRequest where
  toJSON SMSTemplateRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("Body" Core..=) Core.<$> body,
            ("DefaultSubstitutions" Core..=) Core.<$> defaultSubstitutions,
            ("RecommenderId" Core..=) Core.<$> recommenderId,
            ("TemplateDescription" Core..=) Core.<$> templateDescription,
            ("tags" Core..=) Core.<$> tags
          ]
      )

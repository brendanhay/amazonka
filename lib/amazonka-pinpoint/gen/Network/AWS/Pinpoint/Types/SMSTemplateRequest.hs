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
    smstrTemplateDescription,
    smstrDefaultSubstitutions,
    smstrRecommenderId,
    smstrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content and settings for a message template that can be used in text messages that are sent through the SMS channel.
--
-- /See:/ 'mkSMSTemplateRequest' smart constructor.
data SMSTemplateRequest = SMSTemplateRequest'
  { body ::
      Lude.Maybe Lude.Text,
    templateDescription :: Lude.Maybe Lude.Text,
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    recommenderId :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMSTemplateRequest' with the minimum fields required to make a request.
--
-- * 'body' - The message body to use in text messages that are based on the message template.
-- * 'defaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
-- * 'recommenderId' - The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
-- * 'templateDescription' - A custom description of the message template.
mkSMSTemplateRequest ::
  SMSTemplateRequest
mkSMSTemplateRequest =
  SMSTemplateRequest'
    { body = Lude.Nothing,
      templateDescription = Lude.Nothing,
      defaultSubstitutions = Lude.Nothing,
      recommenderId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The message body to use in text messages that are based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrBody :: Lens.Lens' SMSTemplateRequest (Lude.Maybe Lude.Text)
smstrBody = Lens.lens (body :: SMSTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: SMSTemplateRequest)
{-# DEPRECATED smstrBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrTemplateDescription :: Lens.Lens' SMSTemplateRequest (Lude.Maybe Lude.Text)
smstrTemplateDescription = Lens.lens (templateDescription :: SMSTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: SMSTemplateRequest)
{-# DEPRECATED smstrTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrDefaultSubstitutions :: Lens.Lens' SMSTemplateRequest (Lude.Maybe Lude.Text)
smstrDefaultSubstitutions = Lens.lens (defaultSubstitutions :: SMSTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: SMSTemplateRequest)
{-# DEPRECATED smstrDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrRecommenderId :: Lens.Lens' SMSTemplateRequest (Lude.Maybe Lude.Text)
smstrRecommenderId = Lens.lens (recommenderId :: SMSTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {recommenderId = a} :: SMSTemplateRequest)
{-# DEPRECATED smstrRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstrTags :: Lens.Lens' SMSTemplateRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
smstrTags = Lens.lens (tags :: SMSTemplateRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: SMSTemplateRequest)
{-# DEPRECATED smstrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON SMSTemplateRequest where
  toJSON SMSTemplateRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Body" Lude..=) Lude.<$> body,
            ("TemplateDescription" Lude..=) Lude.<$> templateDescription,
            ("DefaultSubstitutions" Lude..=) Lude.<$> defaultSubstitutions,
            ("RecommenderId" Lude..=) Lude.<$> recommenderId,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

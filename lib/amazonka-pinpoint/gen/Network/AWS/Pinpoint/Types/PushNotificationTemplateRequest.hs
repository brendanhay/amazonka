{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PushNotificationTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushNotificationTemplateRequest
  ( PushNotificationTemplateRequest (..),

    -- * Smart constructor
    mkPushNotificationTemplateRequest,

    -- * Lenses
    pADM,
    pAPNS,
    pBaidu,
    pDefault,
    pDefaultSubstitutions,
    pGCM,
    pRecommenderId,
    pTemplateDescription,
    pTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate as Types
import qualified Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate as Types
import qualified Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the content and settings for a message template that can be used in messages that are sent through a push notification channel.
--
-- /See:/ 'mkPushNotificationTemplateRequest' smart constructor.
data PushNotificationTemplateRequest = PushNotificationTemplateRequest'
  { -- | The message template to use for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    adm :: Core.Maybe Types.AndroidPushNotificationTemplate,
    -- | The message template to use for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    apns :: Core.Maybe Types.APNSPushNotificationTemplate,
    -- | The message template to use for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    baidu :: Core.Maybe Types.AndroidPushNotificationTemplate,
    -- | The default message template to use for push notification channels.
    default' :: Core.Maybe Types.DefaultPushNotificationTemplate,
    -- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | The message template to use for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    gcm :: Core.Maybe Types.AndroidPushNotificationTemplate,
    -- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
    recommenderId :: Core.Maybe Core.Text,
    -- | A custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PushNotificationTemplateRequest' value with any optional fields omitted.
mkPushNotificationTemplateRequest ::
  PushNotificationTemplateRequest
mkPushNotificationTemplateRequest =
  PushNotificationTemplateRequest'
    { adm = Core.Nothing,
      apns = Core.Nothing,
      baidu = Core.Nothing,
      default' = Core.Nothing,
      defaultSubstitutions = Core.Nothing,
      gcm = Core.Nothing,
      recommenderId = Core.Nothing,
      templateDescription = Core.Nothing,
      tags = Core.Nothing
    }

-- | The message template to use for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'adm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pADM :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Types.AndroidPushNotificationTemplate)
pADM = Lens.field @"adm"
{-# DEPRECATED pADM "Use generic-lens or generic-optics with 'adm' instead." #-}

-- | The message template to use for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'apns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAPNS :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Types.APNSPushNotificationTemplate)
pAPNS = Lens.field @"apns"
{-# DEPRECATED pAPNS "Use generic-lens or generic-optics with 'apns' instead." #-}

-- | The message template to use for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'baidu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBaidu :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Types.AndroidPushNotificationTemplate)
pBaidu = Lens.field @"baidu"
{-# DEPRECATED pBaidu "Use generic-lens or generic-optics with 'baidu' instead." #-}

-- | The default message template to use for push notification channels.
--
-- /Note:/ Consider using 'default'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDefault :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Types.DefaultPushNotificationTemplate)
pDefault = Lens.field @"default'"
{-# DEPRECATED pDefault "Use generic-lens or generic-optics with 'default'' instead." #-}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDefaultSubstitutions :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Core.Text)
pDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# DEPRECATED pDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The message template to use for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'gcm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGCM :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Types.AndroidPushNotificationTemplate)
pGCM = Lens.field @"gcm"
{-# DEPRECATED pGCM "Use generic-lens or generic-optics with 'gcm' instead." #-}

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRecommenderId :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Core.Text)
pRecommenderId = Lens.field @"recommenderId"
{-# DEPRECATED pRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTemplateDescription :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Core.Text)
pTemplateDescription = Lens.field @"templateDescription"
{-# DEPRECATED pTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTags :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
pTags = Lens.field @"tags"
{-# DEPRECATED pTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON PushNotificationTemplateRequest where
  toJSON PushNotificationTemplateRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("ADM" Core..=) Core.<$> adm,
            ("APNS" Core..=) Core.<$> apns,
            ("Baidu" Core..=) Core.<$> baidu,
            ("Default" Core..=) Core.<$> default',
            ("DefaultSubstitutions" Core..=) Core.<$> defaultSubstitutions,
            ("GCM" Core..=) Core.<$> gcm,
            ("RecommenderId" Core..=) Core.<$> recommenderId,
            ("TemplateDescription" Core..=) Core.<$> templateDescription,
            ("tags" Core..=) Core.<$> tags
          ]
      )

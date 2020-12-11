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
    pntrDefault,
    pntrTemplateDescription,
    pntrGCM,
    pntrAPNS,
    pntrDefaultSubstitutions,
    pntrADM,
    pntrBaidu,
    pntrRecommenderId,
    pntrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
import Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
import Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content and settings for a message template that can be used in messages that are sent through a push notification channel.
--
-- /See:/ 'mkPushNotificationTemplateRequest' smart constructor.
data PushNotificationTemplateRequest = PushNotificationTemplateRequest'
  { default' ::
      Lude.Maybe
        DefaultPushNotificationTemplate,
    templateDescription ::
      Lude.Maybe Lude.Text,
    gcm ::
      Lude.Maybe
        AndroidPushNotificationTemplate,
    apns ::
      Lude.Maybe
        APNSPushNotificationTemplate,
    defaultSubstitutions ::
      Lude.Maybe Lude.Text,
    aDM ::
      Lude.Maybe
        AndroidPushNotificationTemplate,
    baidu ::
      Lude.Maybe
        AndroidPushNotificationTemplate,
    recommenderId ::
      Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PushNotificationTemplateRequest' with the minimum fields required to make a request.
--
-- * 'aDM' - The message template to use for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
-- * 'apns' - The message template to use for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
-- * 'baidu' - The message template to use for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
-- * 'default'' - The default message template to use for push notification channels.
-- * 'defaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
-- * 'gcm' - The message template to use for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
-- * 'recommenderId' - The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
-- * 'templateDescription' - A custom description of the message template.
mkPushNotificationTemplateRequest ::
  PushNotificationTemplateRequest
mkPushNotificationTemplateRequest =
  PushNotificationTemplateRequest'
    { default' = Lude.Nothing,
      templateDescription = Lude.Nothing,
      gcm = Lude.Nothing,
      apns = Lude.Nothing,
      defaultSubstitutions = Lude.Nothing,
      aDM = Lude.Nothing,
      baidu = Lude.Nothing,
      recommenderId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The default message template to use for push notification channels.
--
-- /Note:/ Consider using 'default'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrDefault :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe DefaultPushNotificationTemplate)
pntrDefault = Lens.lens (default' :: PushNotificationTemplateRequest -> Lude.Maybe DefaultPushNotificationTemplate) (\s a -> s {default' = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrDefault "Use generic-lens or generic-optics with 'default'' instead." #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrTemplateDescription :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe Lude.Text)
pntrTemplateDescription = Lens.lens (templateDescription :: PushNotificationTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The message template to use for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'gcm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrGCM :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe AndroidPushNotificationTemplate)
pntrGCM = Lens.lens (gcm :: PushNotificationTemplateRequest -> Lude.Maybe AndroidPushNotificationTemplate) (\s a -> s {gcm = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrGCM "Use generic-lens or generic-optics with 'gcm' instead." #-}

-- | The message template to use for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'apns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrAPNS :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe APNSPushNotificationTemplate)
pntrAPNS = Lens.lens (apns :: PushNotificationTemplateRequest -> Lude.Maybe APNSPushNotificationTemplate) (\s a -> s {apns = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrAPNS "Use generic-lens or generic-optics with 'apns' instead." #-}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrDefaultSubstitutions :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe Lude.Text)
pntrDefaultSubstitutions = Lens.lens (defaultSubstitutions :: PushNotificationTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The message template to use for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'aDM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrADM :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe AndroidPushNotificationTemplate)
pntrADM = Lens.lens (aDM :: PushNotificationTemplateRequest -> Lude.Maybe AndroidPushNotificationTemplate) (\s a -> s {aDM = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrADM "Use generic-lens or generic-optics with 'aDM' instead." #-}

-- | The message template to use for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'baidu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrBaidu :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe AndroidPushNotificationTemplate)
pntrBaidu = Lens.lens (baidu :: PushNotificationTemplateRequest -> Lude.Maybe AndroidPushNotificationTemplate) (\s a -> s {baidu = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrBaidu "Use generic-lens or generic-optics with 'baidu' instead." #-}

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrRecommenderId :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe Lude.Text)
pntrRecommenderId = Lens.lens (recommenderId :: PushNotificationTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {recommenderId = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrTags :: Lens.Lens' PushNotificationTemplateRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pntrTags = Lens.lens (tags :: PushNotificationTemplateRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: PushNotificationTemplateRequest)
{-# DEPRECATED pntrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON PushNotificationTemplateRequest where
  toJSON PushNotificationTemplateRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Default" Lude..=) Lude.<$> default',
            ("TemplateDescription" Lude..=) Lude.<$> templateDescription,
            ("GCM" Lude..=) Lude.<$> gcm,
            ("APNS" Lude..=) Lude.<$> apns,
            ("DefaultSubstitutions" Lude..=) Lude.<$> defaultSubstitutions,
            ("ADM" Lude..=) Lude.<$> aDM,
            ("Baidu" Lude..=) Lude.<$> baidu,
            ("RecommenderId" Lude..=) Lude.<$> recommenderId,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

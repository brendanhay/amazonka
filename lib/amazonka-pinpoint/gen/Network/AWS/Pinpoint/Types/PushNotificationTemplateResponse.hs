{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PushNotificationTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushNotificationTemplateResponse
  ( PushNotificationTemplateResponse (..),

    -- * Smart constructor
    mkPushNotificationTemplateResponse,

    -- * Lenses
    pntrLastModifiedDate,
    pntrCreationDate,
    pntrTemplateType,
    pntrTemplateName,
    pntrADM,
    pntrAPNS,
    pntrArn,
    pntrBaidu,
    pntrDefault,
    pntrDefaultSubstitutions,
    pntrGCM,
    pntrRecommenderId,
    pntrTemplateDescription,
    pntrVersion,
    pntrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate as Types
import qualified Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate as Types
import qualified Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate as Types
import qualified Network.AWS.Pinpoint.Types.TemplateType as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through a push notification channel.
--
-- /See:/ 'mkPushNotificationTemplateResponse' smart constructor.
data PushNotificationTemplateResponse = PushNotificationTemplateResponse'
  { -- | The date, in ISO 8601 format, when the message template was last modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Core.Text,
    -- | The type of channel that the message template is designed for. For a push notification template, this value is PUSH.
    templateType :: Types.TemplateType,
    -- | The name of the message template.
    templateName :: Core.Text,
    -- | The message template that's used for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    adm :: Core.Maybe Types.AndroidPushNotificationTemplate,
    -- | The message template that's used for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    apns :: Core.Maybe Types.APNSPushNotificationTemplate,
    -- | The Amazon Resource Name (ARN) of the message template.
    arn :: Core.Maybe Core.Text,
    -- | The message template that's used for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    baidu :: Core.Maybe Types.AndroidPushNotificationTemplate,
    -- | The default message template that's used for push notification channels.
    default' :: Core.Maybe Types.DefaultPushNotificationTemplate,
    -- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | The message template that's used for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
    gcm :: Core.Maybe Types.AndroidPushNotificationTemplate,
    -- | The unique identifier for the recommender model that's used by the message template.
    recommenderId :: Core.Maybe Core.Text,
    -- | The custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
    version :: Core.Maybe Core.Text,
    -- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PushNotificationTemplateResponse' value with any optional fields omitted.
mkPushNotificationTemplateResponse ::
  -- | 'lastModifiedDate'
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'templateType'
  Types.TemplateType ->
  -- | 'templateName'
  Core.Text ->
  PushNotificationTemplateResponse
mkPushNotificationTemplateResponse
  lastModifiedDate
  creationDate
  templateType
  templateName =
    PushNotificationTemplateResponse'
      { lastModifiedDate,
        creationDate,
        templateType,
        templateName,
        adm = Core.Nothing,
        apns = Core.Nothing,
        arn = Core.Nothing,
        baidu = Core.Nothing,
        default' = Core.Nothing,
        defaultSubstitutions = Core.Nothing,
        gcm = Core.Nothing,
        recommenderId = Core.Nothing,
        templateDescription = Core.Nothing,
        version = Core.Nothing,
        tags = Core.Nothing
      }

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrLastModifiedDate :: Lens.Lens' PushNotificationTemplateResponse Core.Text
pntrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED pntrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrCreationDate :: Lens.Lens' PushNotificationTemplateResponse Core.Text
pntrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED pntrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The type of channel that the message template is designed for. For a push notification template, this value is PUSH.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrTemplateType :: Lens.Lens' PushNotificationTemplateResponse Types.TemplateType
pntrTemplateType = Lens.field @"templateType"
{-# DEPRECATED pntrTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrTemplateName :: Lens.Lens' PushNotificationTemplateResponse Core.Text
pntrTemplateName = Lens.field @"templateName"
{-# DEPRECATED pntrTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The message template that's used for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'adm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrADM :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Types.AndroidPushNotificationTemplate)
pntrADM = Lens.field @"adm"
{-# DEPRECATED pntrADM "Use generic-lens or generic-optics with 'adm' instead." #-}

-- | The message template that's used for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'apns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrAPNS :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Types.APNSPushNotificationTemplate)
pntrAPNS = Lens.field @"apns"
{-# DEPRECATED pntrAPNS "Use generic-lens or generic-optics with 'apns' instead." #-}

-- | The Amazon Resource Name (ARN) of the message template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrArn :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pntrArn = Lens.field @"arn"
{-# DEPRECATED pntrArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The message template that's used for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'baidu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrBaidu :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Types.AndroidPushNotificationTemplate)
pntrBaidu = Lens.field @"baidu"
{-# DEPRECATED pntrBaidu "Use generic-lens or generic-optics with 'baidu' instead." #-}

-- | The default message template that's used for push notification channels.
--
-- /Note:/ Consider using 'default'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrDefault :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Types.DefaultPushNotificationTemplate)
pntrDefault = Lens.field @"default'"
{-# DEPRECATED pntrDefault "Use generic-lens or generic-optics with 'default'' instead." #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrDefaultSubstitutions :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pntrDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# DEPRECATED pntrDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The message template that's used for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- /Note:/ Consider using 'gcm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrGCM :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Types.AndroidPushNotificationTemplate)
pntrGCM = Lens.field @"gcm"
{-# DEPRECATED pntrGCM "Use generic-lens or generic-optics with 'gcm' instead." #-}

-- | The unique identifier for the recommender model that's used by the message template.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrRecommenderId :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pntrRecommenderId = Lens.field @"recommenderId"
{-# DEPRECATED pntrRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | The custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrTemplateDescription :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pntrTemplateDescription = Lens.field @"templateDescription"
{-# DEPRECATED pntrTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrVersion :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pntrVersion = Lens.field @"version"
{-# DEPRECATED pntrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pntrTags :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
pntrTags = Lens.field @"tags"
{-# DEPRECATED pntrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON PushNotificationTemplateResponse where
  parseJSON =
    Core.withObject "PushNotificationTemplateResponse" Core.$
      \x ->
        PushNotificationTemplateResponse'
          Core.<$> (x Core..: "LastModifiedDate")
          Core.<*> (x Core..: "CreationDate")
          Core.<*> (x Core..: "TemplateType")
          Core.<*> (x Core..: "TemplateName")
          Core.<*> (x Core..:? "ADM")
          Core.<*> (x Core..:? "APNS")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Baidu")
          Core.<*> (x Core..:? "Default")
          Core.<*> (x Core..:? "DefaultSubstitutions")
          Core.<*> (x Core..:? "GCM")
          Core.<*> (x Core..:? "RecommenderId")
          Core.<*> (x Core..:? "TemplateDescription")
          Core.<*> (x Core..:? "Version")
          Core.<*> (x Core..:? "tags")

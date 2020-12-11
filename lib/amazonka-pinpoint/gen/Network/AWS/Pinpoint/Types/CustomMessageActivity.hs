-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CustomMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CustomMessageActivity
  ( CustomMessageActivity (..),

    -- * Smart constructor
    mkCustomMessageActivity,

    -- * Lenses
    cmaTemplateName,
    cmaTemplateVersion,
    cmaEndpointTypes,
    cmaNextActivity,
    cmaDeliveryURI,
    cmaMessageConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointTypesElement
import Network.AWS.Pinpoint.Types.JourneyCustomMessage
import qualified Network.AWS.Prelude as Lude

-- | The settings for a custom message activity. This type of activity calls an AWS Lambda function or web hook that sends messages to participants.
--
-- /See:/ 'mkCustomMessageActivity' smart constructor.
data CustomMessageActivity = CustomMessageActivity'
  { templateName ::
      Lude.Maybe Lude.Text,
    templateVersion :: Lude.Maybe Lude.Text,
    endpointTypes ::
      Lude.Maybe [EndpointTypesElement],
    nextActivity :: Lude.Maybe Lude.Text,
    deliveryURI :: Lude.Maybe Lude.Text,
    messageConfig ::
      Lude.Maybe JourneyCustomMessage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomMessageActivity' with the minimum fields required to make a request.
--
-- * 'deliveryURI' - The destination to send the campaign or treatment to. This value can be one of the following:
--
--
--     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.
--
--
--     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
--
--
-- * 'endpointTypes' - The types of endpoints to send the custom message to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
-- * 'messageConfig' - Specifies the message data included in a custom channel message that's sent to participants in a journey.
-- * 'nextActivity' - The unique identifier for the next activity to perform, after Amazon Pinpoint calls the AWS Lambda function or web hook.
-- * 'templateName' - The name of the custom message template to use for the message. If specified, this value must match the name of an existing message template.
-- * 'templateVersion' - The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
mkCustomMessageActivity ::
  CustomMessageActivity
mkCustomMessageActivity =
  CustomMessageActivity'
    { templateName = Lude.Nothing,
      templateVersion = Lude.Nothing,
      endpointTypes = Lude.Nothing,
      nextActivity = Lude.Nothing,
      deliveryURI = Lude.Nothing,
      messageConfig = Lude.Nothing
    }

-- | The name of the custom message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaTemplateName :: Lens.Lens' CustomMessageActivity (Lude.Maybe Lude.Text)
cmaTemplateName = Lens.lens (templateName :: CustomMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: CustomMessageActivity)
{-# DEPRECATED cmaTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaTemplateVersion :: Lens.Lens' CustomMessageActivity (Lude.Maybe Lude.Text)
cmaTemplateVersion = Lens.lens (templateVersion :: CustomMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {templateVersion = a} :: CustomMessageActivity)
{-# DEPRECATED cmaTemplateVersion "Use generic-lens or generic-optics with 'templateVersion' instead." #-}

-- | The types of endpoints to send the custom message to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
--
-- /Note:/ Consider using 'endpointTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaEndpointTypes :: Lens.Lens' CustomMessageActivity (Lude.Maybe [EndpointTypesElement])
cmaEndpointTypes = Lens.lens (endpointTypes :: CustomMessageActivity -> Lude.Maybe [EndpointTypesElement]) (\s a -> s {endpointTypes = a} :: CustomMessageActivity)
{-# DEPRECATED cmaEndpointTypes "Use generic-lens or generic-optics with 'endpointTypes' instead." #-}

-- | The unique identifier for the next activity to perform, after Amazon Pinpoint calls the AWS Lambda function or web hook.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaNextActivity :: Lens.Lens' CustomMessageActivity (Lude.Maybe Lude.Text)
cmaNextActivity = Lens.lens (nextActivity :: CustomMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: CustomMessageActivity)
{-# DEPRECATED cmaNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The destination to send the campaign or treatment to. This value can be one of the following:
--
--
--     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.
--
--
--     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
--
--
--
-- /Note:/ Consider using 'deliveryURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaDeliveryURI :: Lens.Lens' CustomMessageActivity (Lude.Maybe Lude.Text)
cmaDeliveryURI = Lens.lens (deliveryURI :: CustomMessageActivity -> Lude.Maybe Lude.Text) (\s a -> s {deliveryURI = a} :: CustomMessageActivity)
{-# DEPRECATED cmaDeliveryURI "Use generic-lens or generic-optics with 'deliveryURI' instead." #-}

-- | Specifies the message data included in a custom channel message that's sent to participants in a journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMessageConfig :: Lens.Lens' CustomMessageActivity (Lude.Maybe JourneyCustomMessage)
cmaMessageConfig = Lens.lens (messageConfig :: CustomMessageActivity -> Lude.Maybe JourneyCustomMessage) (\s a -> s {messageConfig = a} :: CustomMessageActivity)
{-# DEPRECATED cmaMessageConfig "Use generic-lens or generic-optics with 'messageConfig' instead." #-}

instance Lude.FromJSON CustomMessageActivity where
  parseJSON =
    Lude.withObject
      "CustomMessageActivity"
      ( \x ->
          CustomMessageActivity'
            Lude.<$> (x Lude..:? "TemplateName")
            Lude.<*> (x Lude..:? "TemplateVersion")
            Lude.<*> (x Lude..:? "EndpointTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextActivity")
            Lude.<*> (x Lude..:? "DeliveryUri")
            Lude.<*> (x Lude..:? "MessageConfig")
      )

instance Lude.ToJSON CustomMessageActivity where
  toJSON CustomMessageActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TemplateName" Lude..=) Lude.<$> templateName,
            ("TemplateVersion" Lude..=) Lude.<$> templateVersion,
            ("EndpointTypes" Lude..=) Lude.<$> endpointTypes,
            ("NextActivity" Lude..=) Lude.<$> nextActivity,
            ("DeliveryUri" Lude..=) Lude.<$> deliveryURI,
            ("MessageConfig" Lude..=) Lude.<$> messageConfig
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cmaDeliveryUri,
    cmaEndpointTypes,
    cmaMessageConfig,
    cmaNextActivity,
    cmaTemplateName,
    cmaTemplateVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EndpointTypesElement as Types
import qualified Network.AWS.Pinpoint.Types.JourneyCustomMessage as Types
import qualified Network.AWS.Prelude as Core

-- | The settings for a custom message activity. This type of activity calls an AWS Lambda function or web hook that sends messages to participants.
--
-- /See:/ 'mkCustomMessageActivity' smart constructor.
data CustomMessageActivity = CustomMessageActivity'
  { -- | The destination to send the campaign or treatment to. This value can be one of the following:
    --
    --
    --     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.
    --
    --
    --     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
    deliveryUri :: Core.Maybe Core.Text,
    -- | The types of endpoints to send the custom message to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
    endpointTypes :: Core.Maybe [Types.EndpointTypesElement],
    -- | Specifies the message data included in a custom channel message that's sent to participants in a journey.
    messageConfig :: Core.Maybe Types.JourneyCustomMessage,
    -- | The unique identifier for the next activity to perform, after Amazon Pinpoint calls the AWS Lambda function or web hook.
    nextActivity :: Core.Maybe Core.Text,
    -- | The name of the custom message template to use for the message. If specified, this value must match the name of an existing message template.
    templateName :: Core.Maybe Core.Text,
    -- | The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
    --
    -- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
    templateVersion :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomMessageActivity' value with any optional fields omitted.
mkCustomMessageActivity ::
  CustomMessageActivity
mkCustomMessageActivity =
  CustomMessageActivity'
    { deliveryUri = Core.Nothing,
      endpointTypes = Core.Nothing,
      messageConfig = Core.Nothing,
      nextActivity = Core.Nothing,
      templateName = Core.Nothing,
      templateVersion = Core.Nothing
    }

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
-- /Note:/ Consider using 'deliveryUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaDeliveryUri :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
cmaDeliveryUri = Lens.field @"deliveryUri"
{-# DEPRECATED cmaDeliveryUri "Use generic-lens or generic-optics with 'deliveryUri' instead." #-}

-- | The types of endpoints to send the custom message to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
--
-- /Note:/ Consider using 'endpointTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaEndpointTypes :: Lens.Lens' CustomMessageActivity (Core.Maybe [Types.EndpointTypesElement])
cmaEndpointTypes = Lens.field @"endpointTypes"
{-# DEPRECATED cmaEndpointTypes "Use generic-lens or generic-optics with 'endpointTypes' instead." #-}

-- | Specifies the message data included in a custom channel message that's sent to participants in a journey.
--
-- /Note:/ Consider using 'messageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMessageConfig :: Lens.Lens' CustomMessageActivity (Core.Maybe Types.JourneyCustomMessage)
cmaMessageConfig = Lens.field @"messageConfig"
{-# DEPRECATED cmaMessageConfig "Use generic-lens or generic-optics with 'messageConfig' instead." #-}

-- | The unique identifier for the next activity to perform, after Amazon Pinpoint calls the AWS Lambda function or web hook.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaNextActivity :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
cmaNextActivity = Lens.field @"nextActivity"
{-# DEPRECATED cmaNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The name of the custom message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaTemplateName :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
cmaTemplateName = Lens.field @"templateName"
{-# DEPRECATED cmaTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'templateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaTemplateVersion :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
cmaTemplateVersion = Lens.field @"templateVersion"
{-# DEPRECATED cmaTemplateVersion "Use generic-lens or generic-optics with 'templateVersion' instead." #-}

instance Core.FromJSON CustomMessageActivity where
  toJSON CustomMessageActivity {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeliveryUri" Core..=) Core.<$> deliveryUri,
            ("EndpointTypes" Core..=) Core.<$> endpointTypes,
            ("MessageConfig" Core..=) Core.<$> messageConfig,
            ("NextActivity" Core..=) Core.<$> nextActivity,
            ("TemplateName" Core..=) Core.<$> templateName,
            ("TemplateVersion" Core..=) Core.<$> templateVersion
          ]
      )

instance Core.FromJSON CustomMessageActivity where
  parseJSON =
    Core.withObject "CustomMessageActivity" Core.$
      \x ->
        CustomMessageActivity'
          Core.<$> (x Core..:? "DeliveryUri")
          Core.<*> (x Core..:? "EndpointTypes")
          Core.<*> (x Core..:? "MessageConfig")
          Core.<*> (x Core..:? "NextActivity")
          Core.<*> (x Core..:? "TemplateName")
          Core.<*> (x Core..:? "TemplateVersion")

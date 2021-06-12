{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CustomMessageActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CustomMessageActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointTypesElement
import Network.AWS.Pinpoint.Types.JourneyCustomMessage

-- | The settings for a custom message activity. This type of activity calls
-- an AWS Lambda function or web hook that sends messages to participants.
--
-- /See:/ 'newCustomMessageActivity' smart constructor.
data CustomMessageActivity = CustomMessageActivity'
  { -- | The name of the custom message template to use for the message. If
    -- specified, this value must match the name of an existing message
    -- template.
    templateName :: Core.Maybe Core.Text,
    -- | Specifies the message data included in a custom channel message that\'s
    -- sent to participants in a journey.
    messageConfig :: Core.Maybe JourneyCustomMessage,
    -- | The destination to send the campaign or treatment to. This value can be
    -- one of the following:
    --
    -- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
    --     invoke to handle delivery of the campaign or treatment.
    --
    -- -   The URL for a web application or service that supports HTTPS and can
    --     receive the message. The URL has to be a full URL, including the
    --     HTTPS protocol.
    deliveryUri :: Core.Maybe Core.Text,
    -- | The types of endpoints to send the custom message to. Each valid value
    -- maps to a type of channel that you can associate with an endpoint by
    -- using the ChannelType property of an endpoint.
    endpointTypes :: Core.Maybe [EndpointTypesElement],
    -- | The unique identifier for the next activity to perform, after Amazon
    -- Pinpoint calls the AWS Lambda function or web hook.
    nextActivity :: Core.Maybe Core.Text,
    -- | The unique identifier for the version of the message template to use for
    -- the message. If specified, this value must match the identifier for an
    -- existing template version. To retrieve a list of versions and version
    -- identifiers for a template, use the Template Versions resource.
    --
    -- If you don\'t specify a value for this property, Amazon Pinpoint uses
    -- the /active version/ of the template. The /active version/ is typically
    -- the version of a template that\'s been most recently reviewed and
    -- approved for use, depending on your workflow. It isn\'t necessarily the
    -- latest version of a template.
    templateVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomMessageActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'customMessageActivity_templateName' - The name of the custom message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
--
-- 'messageConfig', 'customMessageActivity_messageConfig' - Specifies the message data included in a custom channel message that\'s
-- sent to participants in a journey.
--
-- 'deliveryUri', 'customMessageActivity_deliveryUri' - The destination to send the campaign or treatment to. This value can be
-- one of the following:
--
-- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
--     invoke to handle delivery of the campaign or treatment.
--
-- -   The URL for a web application or service that supports HTTPS and can
--     receive the message. The URL has to be a full URL, including the
--     HTTPS protocol.
--
-- 'endpointTypes', 'customMessageActivity_endpointTypes' - The types of endpoints to send the custom message to. Each valid value
-- maps to a type of channel that you can associate with an endpoint by
-- using the ChannelType property of an endpoint.
--
-- 'nextActivity', 'customMessageActivity_nextActivity' - The unique identifier for the next activity to perform, after Amazon
-- Pinpoint calls the AWS Lambda function or web hook.
--
-- 'templateVersion', 'customMessageActivity_templateVersion' - The unique identifier for the version of the message template to use for
-- the message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
newCustomMessageActivity ::
  CustomMessageActivity
newCustomMessageActivity =
  CustomMessageActivity'
    { templateName = Core.Nothing,
      messageConfig = Core.Nothing,
      deliveryUri = Core.Nothing,
      endpointTypes = Core.Nothing,
      nextActivity = Core.Nothing,
      templateVersion = Core.Nothing
    }

-- | The name of the custom message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
customMessageActivity_templateName :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
customMessageActivity_templateName = Lens.lens (\CustomMessageActivity' {templateName} -> templateName) (\s@CustomMessageActivity' {} a -> s {templateName = a} :: CustomMessageActivity)

-- | Specifies the message data included in a custom channel message that\'s
-- sent to participants in a journey.
customMessageActivity_messageConfig :: Lens.Lens' CustomMessageActivity (Core.Maybe JourneyCustomMessage)
customMessageActivity_messageConfig = Lens.lens (\CustomMessageActivity' {messageConfig} -> messageConfig) (\s@CustomMessageActivity' {} a -> s {messageConfig = a} :: CustomMessageActivity)

-- | The destination to send the campaign or treatment to. This value can be
-- one of the following:
--
-- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
--     invoke to handle delivery of the campaign or treatment.
--
-- -   The URL for a web application or service that supports HTTPS and can
--     receive the message. The URL has to be a full URL, including the
--     HTTPS protocol.
customMessageActivity_deliveryUri :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
customMessageActivity_deliveryUri = Lens.lens (\CustomMessageActivity' {deliveryUri} -> deliveryUri) (\s@CustomMessageActivity' {} a -> s {deliveryUri = a} :: CustomMessageActivity)

-- | The types of endpoints to send the custom message to. Each valid value
-- maps to a type of channel that you can associate with an endpoint by
-- using the ChannelType property of an endpoint.
customMessageActivity_endpointTypes :: Lens.Lens' CustomMessageActivity (Core.Maybe [EndpointTypesElement])
customMessageActivity_endpointTypes = Lens.lens (\CustomMessageActivity' {endpointTypes} -> endpointTypes) (\s@CustomMessageActivity' {} a -> s {endpointTypes = a} :: CustomMessageActivity) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the next activity to perform, after Amazon
-- Pinpoint calls the AWS Lambda function or web hook.
customMessageActivity_nextActivity :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
customMessageActivity_nextActivity = Lens.lens (\CustomMessageActivity' {nextActivity} -> nextActivity) (\s@CustomMessageActivity' {} a -> s {nextActivity = a} :: CustomMessageActivity)

-- | The unique identifier for the version of the message template to use for
-- the message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
customMessageActivity_templateVersion :: Lens.Lens' CustomMessageActivity (Core.Maybe Core.Text)
customMessageActivity_templateVersion = Lens.lens (\CustomMessageActivity' {templateVersion} -> templateVersion) (\s@CustomMessageActivity' {} a -> s {templateVersion = a} :: CustomMessageActivity)

instance Core.FromJSON CustomMessageActivity where
  parseJSON =
    Core.withObject
      "CustomMessageActivity"
      ( \x ->
          CustomMessageActivity'
            Core.<$> (x Core..:? "TemplateName")
            Core.<*> (x Core..:? "MessageConfig")
            Core.<*> (x Core..:? "DeliveryUri")
            Core.<*> (x Core..:? "EndpointTypes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextActivity")
            Core.<*> (x Core..:? "TemplateVersion")
      )

instance Core.Hashable CustomMessageActivity

instance Core.NFData CustomMessageActivity

instance Core.ToJSON CustomMessageActivity where
  toJSON CustomMessageActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TemplateName" Core..=) Core.<$> templateName,
            ("MessageConfig" Core..=) Core.<$> messageConfig,
            ("DeliveryUri" Core..=) Core.<$> deliveryUri,
            ("EndpointTypes" Core..=) Core.<$> endpointTypes,
            ("NextActivity" Core..=) Core.<$> nextActivity,
            ("TemplateVersion" Core..=)
              Core.<$> templateVersion
          ]
      )

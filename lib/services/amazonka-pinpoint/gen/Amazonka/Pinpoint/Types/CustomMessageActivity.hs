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
-- Module      : Amazonka.Pinpoint.Types.CustomMessageActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CustomMessageActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EndpointTypesElement
import Amazonka.Pinpoint.Types.JourneyCustomMessage
import qualified Amazonka.Prelude as Prelude

-- | The settings for a custom message activity. This type of activity calls
-- an AWS Lambda function or web hook that sends messages to participants.
--
-- /See:/ 'newCustomMessageActivity' smart constructor.
data CustomMessageActivity = CustomMessageActivity'
  { -- | The destination to send the campaign or treatment to. This value can be
    -- one of the following:
    --
    -- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
    --     invoke to handle delivery of the campaign or treatment.
    --
    -- -   The URL for a web application or service that supports HTTPS and can
    --     receive the message. The URL has to be a full URL, including the
    --     HTTPS protocol.
    deliveryUri :: Prelude.Maybe Prelude.Text,
    -- | The types of endpoints to send the custom message to. Each valid value
    -- maps to a type of channel that you can associate with an endpoint by
    -- using the ChannelType property of an endpoint.
    endpointTypes :: Prelude.Maybe [EndpointTypesElement],
    -- | Specifies the message data included in a custom channel message that\'s
    -- sent to participants in a journey.
    messageConfig :: Prelude.Maybe JourneyCustomMessage,
    -- | The unique identifier for the next activity to perform, after Amazon
    -- Pinpoint calls the AWS Lambda function or web hook.
    nextActivity :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom message template to use for the message. If
    -- specified, this value must match the name of an existing message
    -- template.
    templateName :: Prelude.Maybe Prelude.Text,
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
    templateVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomMessageActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'messageConfig', 'customMessageActivity_messageConfig' - Specifies the message data included in a custom channel message that\'s
-- sent to participants in a journey.
--
-- 'nextActivity', 'customMessageActivity_nextActivity' - The unique identifier for the next activity to perform, after Amazon
-- Pinpoint calls the AWS Lambda function or web hook.
--
-- 'templateName', 'customMessageActivity_templateName' - The name of the custom message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
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
    { deliveryUri =
        Prelude.Nothing,
      endpointTypes = Prelude.Nothing,
      messageConfig = Prelude.Nothing,
      nextActivity = Prelude.Nothing,
      templateName = Prelude.Nothing,
      templateVersion = Prelude.Nothing
    }

-- | The destination to send the campaign or treatment to. This value can be
-- one of the following:
--
-- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
--     invoke to handle delivery of the campaign or treatment.
--
-- -   The URL for a web application or service that supports HTTPS and can
--     receive the message. The URL has to be a full URL, including the
--     HTTPS protocol.
customMessageActivity_deliveryUri :: Lens.Lens' CustomMessageActivity (Prelude.Maybe Prelude.Text)
customMessageActivity_deliveryUri = Lens.lens (\CustomMessageActivity' {deliveryUri} -> deliveryUri) (\s@CustomMessageActivity' {} a -> s {deliveryUri = a} :: CustomMessageActivity)

-- | The types of endpoints to send the custom message to. Each valid value
-- maps to a type of channel that you can associate with an endpoint by
-- using the ChannelType property of an endpoint.
customMessageActivity_endpointTypes :: Lens.Lens' CustomMessageActivity (Prelude.Maybe [EndpointTypesElement])
customMessageActivity_endpointTypes = Lens.lens (\CustomMessageActivity' {endpointTypes} -> endpointTypes) (\s@CustomMessageActivity' {} a -> s {endpointTypes = a} :: CustomMessageActivity) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the message data included in a custom channel message that\'s
-- sent to participants in a journey.
customMessageActivity_messageConfig :: Lens.Lens' CustomMessageActivity (Prelude.Maybe JourneyCustomMessage)
customMessageActivity_messageConfig = Lens.lens (\CustomMessageActivity' {messageConfig} -> messageConfig) (\s@CustomMessageActivity' {} a -> s {messageConfig = a} :: CustomMessageActivity)

-- | The unique identifier for the next activity to perform, after Amazon
-- Pinpoint calls the AWS Lambda function or web hook.
customMessageActivity_nextActivity :: Lens.Lens' CustomMessageActivity (Prelude.Maybe Prelude.Text)
customMessageActivity_nextActivity = Lens.lens (\CustomMessageActivity' {nextActivity} -> nextActivity) (\s@CustomMessageActivity' {} a -> s {nextActivity = a} :: CustomMessageActivity)

-- | The name of the custom message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
customMessageActivity_templateName :: Lens.Lens' CustomMessageActivity (Prelude.Maybe Prelude.Text)
customMessageActivity_templateName = Lens.lens (\CustomMessageActivity' {templateName} -> templateName) (\s@CustomMessageActivity' {} a -> s {templateName = a} :: CustomMessageActivity)

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
customMessageActivity_templateVersion :: Lens.Lens' CustomMessageActivity (Prelude.Maybe Prelude.Text)
customMessageActivity_templateVersion = Lens.lens (\CustomMessageActivity' {templateVersion} -> templateVersion) (\s@CustomMessageActivity' {} a -> s {templateVersion = a} :: CustomMessageActivity)

instance Data.FromJSON CustomMessageActivity where
  parseJSON =
    Data.withObject
      "CustomMessageActivity"
      ( \x ->
          CustomMessageActivity'
            Prelude.<$> (x Data..:? "DeliveryUri")
            Prelude.<*> (x Data..:? "EndpointTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MessageConfig")
            Prelude.<*> (x Data..:? "NextActivity")
            Prelude.<*> (x Data..:? "TemplateName")
            Prelude.<*> (x Data..:? "TemplateVersion")
      )

instance Prelude.Hashable CustomMessageActivity where
  hashWithSalt _salt CustomMessageActivity' {..} =
    _salt
      `Prelude.hashWithSalt` deliveryUri
      `Prelude.hashWithSalt` endpointTypes
      `Prelude.hashWithSalt` messageConfig
      `Prelude.hashWithSalt` nextActivity
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateVersion

instance Prelude.NFData CustomMessageActivity where
  rnf CustomMessageActivity' {..} =
    Prelude.rnf deliveryUri
      `Prelude.seq` Prelude.rnf endpointTypes
      `Prelude.seq` Prelude.rnf messageConfig
      `Prelude.seq` Prelude.rnf nextActivity
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateVersion

instance Data.ToJSON CustomMessageActivity where
  toJSON CustomMessageActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeliveryUri" Data..=) Prelude.<$> deliveryUri,
            ("EndpointTypes" Data..=) Prelude.<$> endpointTypes,
            ("MessageConfig" Data..=) Prelude.<$> messageConfig,
            ("NextActivity" Data..=) Prelude.<$> nextActivity,
            ("TemplateName" Data..=) Prelude.<$> templateName,
            ("TemplateVersion" Data..=)
              Prelude.<$> templateVersion
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CustomMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CustomMessageActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EndpointTypesElement
import Network.AWS.Pinpoint.Types.JourneyCustomMessage
import Network.AWS.Prelude

-- | The settings for a custom message activity. This type of activity calls an AWS Lambda function or web hook that sends messages to participants.
--
--
--
-- /See:/ 'customMessageActivity' smart constructor.
data CustomMessageActivity = CustomMessageActivity'
  { _cmaTemplateName ::
      !(Maybe Text),
    _cmaTemplateVersion :: !(Maybe Text),
    _cmaEndpointTypes ::
      !(Maybe [EndpointTypesElement]),
    _cmaNextActivity :: !(Maybe Text),
    _cmaDeliveryURI :: !(Maybe Text),
    _cmaMessageConfig ::
      !(Maybe JourneyCustomMessage)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomMessageActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmaTemplateName' - The name of the custom message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- * 'cmaTemplateVersion' - The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- * 'cmaEndpointTypes' - The types of endpoints to send the custom message to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
--
-- * 'cmaNextActivity' - The unique identifier for the next activity to perform, after Amazon Pinpoint calls the AWS Lambda function or web hook.
--
-- * 'cmaDeliveryURI' - The destination to send the campaign or treatment to. This value can be one of the following:     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
--
-- * 'cmaMessageConfig' - Specifies the message data included in a custom channel message that's sent to participants in a journey.
customMessageActivity ::
  CustomMessageActivity
customMessageActivity =
  CustomMessageActivity'
    { _cmaTemplateName = Nothing,
      _cmaTemplateVersion = Nothing,
      _cmaEndpointTypes = Nothing,
      _cmaNextActivity = Nothing,
      _cmaDeliveryURI = Nothing,
      _cmaMessageConfig = Nothing
    }

-- | The name of the custom message template to use for the message. If specified, this value must match the name of an existing message template.
cmaTemplateName :: Lens' CustomMessageActivity (Maybe Text)
cmaTemplateName = lens _cmaTemplateName (\s a -> s {_cmaTemplateName = a})

-- | The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
cmaTemplateVersion :: Lens' CustomMessageActivity (Maybe Text)
cmaTemplateVersion = lens _cmaTemplateVersion (\s a -> s {_cmaTemplateVersion = a})

-- | The types of endpoints to send the custom message to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
cmaEndpointTypes :: Lens' CustomMessageActivity [EndpointTypesElement]
cmaEndpointTypes = lens _cmaEndpointTypes (\s a -> s {_cmaEndpointTypes = a}) . _Default . _Coerce

-- | The unique identifier for the next activity to perform, after Amazon Pinpoint calls the AWS Lambda function or web hook.
cmaNextActivity :: Lens' CustomMessageActivity (Maybe Text)
cmaNextActivity = lens _cmaNextActivity (\s a -> s {_cmaNextActivity = a})

-- | The destination to send the campaign or treatment to. This value can be one of the following:     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
cmaDeliveryURI :: Lens' CustomMessageActivity (Maybe Text)
cmaDeliveryURI = lens _cmaDeliveryURI (\s a -> s {_cmaDeliveryURI = a})

-- | Specifies the message data included in a custom channel message that's sent to participants in a journey.
cmaMessageConfig :: Lens' CustomMessageActivity (Maybe JourneyCustomMessage)
cmaMessageConfig = lens _cmaMessageConfig (\s a -> s {_cmaMessageConfig = a})

instance FromJSON CustomMessageActivity where
  parseJSON =
    withObject
      "CustomMessageActivity"
      ( \x ->
          CustomMessageActivity'
            <$> (x .:? "TemplateName")
            <*> (x .:? "TemplateVersion")
            <*> (x .:? "EndpointTypes" .!= mempty)
            <*> (x .:? "NextActivity")
            <*> (x .:? "DeliveryUri")
            <*> (x .:? "MessageConfig")
      )

instance Hashable CustomMessageActivity

instance NFData CustomMessageActivity

instance ToJSON CustomMessageActivity where
  toJSON CustomMessageActivity' {..} =
    object
      ( catMaybes
          [ ("TemplateName" .=) <$> _cmaTemplateName,
            ("TemplateVersion" .=) <$> _cmaTemplateVersion,
            ("EndpointTypes" .=) <$> _cmaEndpointTypes,
            ("NextActivity" .=) <$> _cmaNextActivity,
            ("DeliveryUri" .=) <$> _cmaDeliveryURI,
            ("MessageConfig" .=) <$> _cmaMessageConfig
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import Network.AWS.Prelude

-- | Specifies the channel type and other settings for an endpoint.
--
--
--
-- /See:/ 'endpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { _erRequestId ::
      !(Maybe Text),
    _erMetrics :: !(Maybe (Map Text (Double))),
    _erLocation :: !(Maybe EndpointLocation),
    _erDemographic :: !(Maybe EndpointDemographic),
    _erAddress :: !(Maybe Text),
    _erEffectiveDate :: !(Maybe Text),
    _erUser :: !(Maybe EndpointUser),
    _erAttributes :: !(Maybe (Map Text ([Text]))),
    _erEndpointStatus :: !(Maybe Text),
    _erOptOut :: !(Maybe Text),
    _erChannelType :: !(Maybe ChannelType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erRequestId' - The unique identifier for the most recent request to update the endpoint.
--
-- * 'erMetrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- * 'erLocation' - The geographic information for the endpoint.
--
-- * 'erDemographic' - The demographic information for the endpoint, such as the time zone and platform.
--
-- * 'erAddress' - The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
--
-- * 'erEffectiveDate' - The date and time, in ISO 8601 format, when the endpoint is updated.
--
-- * 'erUser' - One or more custom attributes that describe the user who's associated with the endpoint.
--
-- * 'erAttributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive. An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- * 'erEndpointStatus' - Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- * 'erOptOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- * 'erChannelType' - The channel to use when sending messages or push notifications to the endpoint.
endpointRequest ::
  EndpointRequest
endpointRequest =
  EndpointRequest'
    { _erRequestId = Nothing,
      _erMetrics = Nothing,
      _erLocation = Nothing,
      _erDemographic = Nothing,
      _erAddress = Nothing,
      _erEffectiveDate = Nothing,
      _erUser = Nothing,
      _erAttributes = Nothing,
      _erEndpointStatus = Nothing,
      _erOptOut = Nothing,
      _erChannelType = Nothing
    }

-- | The unique identifier for the most recent request to update the endpoint.
erRequestId :: Lens' EndpointRequest (Maybe Text)
erRequestId = lens _erRequestId (\s a -> s {_erRequestId = a})

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
erMetrics :: Lens' EndpointRequest (HashMap Text (Double))
erMetrics = lens _erMetrics (\s a -> s {_erMetrics = a}) . _Default . _Map

-- | The geographic information for the endpoint.
erLocation :: Lens' EndpointRequest (Maybe EndpointLocation)
erLocation = lens _erLocation (\s a -> s {_erLocation = a})

-- | The demographic information for the endpoint, such as the time zone and platform.
erDemographic :: Lens' EndpointRequest (Maybe EndpointDemographic)
erDemographic = lens _erDemographic (\s a -> s {_erDemographic = a})

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
erAddress :: Lens' EndpointRequest (Maybe Text)
erAddress = lens _erAddress (\s a -> s {_erAddress = a})

-- | The date and time, in ISO 8601 format, when the endpoint is updated.
erEffectiveDate :: Lens' EndpointRequest (Maybe Text)
erEffectiveDate = lens _erEffectiveDate (\s a -> s {_erEffectiveDate = a})

-- | One or more custom attributes that describe the user who's associated with the endpoint.
erUser :: Lens' EndpointRequest (Maybe EndpointUser)
erUser = lens _erUser (\s a -> s {_erUser = a})

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive. An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
erAttributes :: Lens' EndpointRequest (HashMap Text ([Text]))
erAttributes = lens _erAttributes (\s a -> s {_erAttributes = a}) . _Default . _Map

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
erEndpointStatus :: Lens' EndpointRequest (Maybe Text)
erEndpointStatus = lens _erEndpointStatus (\s a -> s {_erEndpointStatus = a})

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
erOptOut :: Lens' EndpointRequest (Maybe Text)
erOptOut = lens _erOptOut (\s a -> s {_erOptOut = a})

-- | The channel to use when sending messages or push notifications to the endpoint.
erChannelType :: Lens' EndpointRequest (Maybe ChannelType)
erChannelType = lens _erChannelType (\s a -> s {_erChannelType = a})

instance Hashable EndpointRequest

instance NFData EndpointRequest

instance ToJSON EndpointRequest where
  toJSON EndpointRequest' {..} =
    object
      ( catMaybes
          [ ("RequestId" .=) <$> _erRequestId,
            ("Metrics" .=) <$> _erMetrics,
            ("Location" .=) <$> _erLocation,
            ("Demographic" .=) <$> _erDemographic,
            ("Address" .=) <$> _erAddress,
            ("EffectiveDate" .=) <$> _erEffectiveDate,
            ("User" .=) <$> _erUser,
            ("Attributes" .=) <$> _erAttributes,
            ("EndpointStatus" .=) <$> _erEndpointStatus,
            ("OptOut" .=) <$> _erOptOut,
            ("ChannelType" .=) <$> _erChannelType
          ]
      )

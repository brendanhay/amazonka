{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointBatchItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointBatchItem where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import Network.AWS.Prelude

-- | Specifies an endpoint to create or update and the settings and attributes to set or change for the endpoint.
--
--
--
-- /See:/ 'endpointBatchItem' smart constructor.
data EndpointBatchItem = EndpointBatchItem'
  { _ebiRequestId ::
      !(Maybe Text),
    _ebiMetrics :: !(Maybe (Map Text (Double))),
    _ebiLocation :: !(Maybe EndpointLocation),
    _ebiDemographic :: !(Maybe EndpointDemographic),
    _ebiAddress :: !(Maybe Text),
    _ebiEffectiveDate :: !(Maybe Text),
    _ebiUser :: !(Maybe EndpointUser),
    _ebiAttributes :: !(Maybe (Map Text ([Text]))),
    _ebiEndpointStatus :: !(Maybe Text),
    _ebiOptOut :: !(Maybe Text),
    _ebiId :: !(Maybe Text),
    _ebiChannelType :: !(Maybe ChannelType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointBatchItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebiRequestId' - The unique identifier for the request to create or update the endpoint.
--
-- * 'ebiMetrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- * 'ebiLocation' - The geographic information for the endpoint.
--
-- * 'ebiDemographic' - The demographic information for the endpoint, such as the time zone and platform.
--
-- * 'ebiAddress' - The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
--
-- * 'ebiEffectiveDate' - The date and time, in ISO 8601 format, when the endpoint was created or updated.
--
-- * 'ebiUser' - One or more custom attributes that describe the user who's associated with the endpoint.
--
-- * 'ebiAttributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive. An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- * 'ebiEndpointStatus' - Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- * 'ebiOptOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- * 'ebiId' - The unique identifier for the endpoint in the context of the batch.
--
-- * 'ebiChannelType' - The channel to use when sending messages or push notifications to the endpoint.
endpointBatchItem ::
  EndpointBatchItem
endpointBatchItem =
  EndpointBatchItem'
    { _ebiRequestId = Nothing,
      _ebiMetrics = Nothing,
      _ebiLocation = Nothing,
      _ebiDemographic = Nothing,
      _ebiAddress = Nothing,
      _ebiEffectiveDate = Nothing,
      _ebiUser = Nothing,
      _ebiAttributes = Nothing,
      _ebiEndpointStatus = Nothing,
      _ebiOptOut = Nothing,
      _ebiId = Nothing,
      _ebiChannelType = Nothing
    }

-- | The unique identifier for the request to create or update the endpoint.
ebiRequestId :: Lens' EndpointBatchItem (Maybe Text)
ebiRequestId = lens _ebiRequestId (\s a -> s {_ebiRequestId = a})

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
ebiMetrics :: Lens' EndpointBatchItem (HashMap Text (Double))
ebiMetrics = lens _ebiMetrics (\s a -> s {_ebiMetrics = a}) . _Default . _Map

-- | The geographic information for the endpoint.
ebiLocation :: Lens' EndpointBatchItem (Maybe EndpointLocation)
ebiLocation = lens _ebiLocation (\s a -> s {_ebiLocation = a})

-- | The demographic information for the endpoint, such as the time zone and platform.
ebiDemographic :: Lens' EndpointBatchItem (Maybe EndpointDemographic)
ebiDemographic = lens _ebiDemographic (\s a -> s {_ebiDemographic = a})

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
ebiAddress :: Lens' EndpointBatchItem (Maybe Text)
ebiAddress = lens _ebiAddress (\s a -> s {_ebiAddress = a})

-- | The date and time, in ISO 8601 format, when the endpoint was created or updated.
ebiEffectiveDate :: Lens' EndpointBatchItem (Maybe Text)
ebiEffectiveDate = lens _ebiEffectiveDate (\s a -> s {_ebiEffectiveDate = a})

-- | One or more custom attributes that describe the user who's associated with the endpoint.
ebiUser :: Lens' EndpointBatchItem (Maybe EndpointUser)
ebiUser = lens _ebiUser (\s a -> s {_ebiUser = a})

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive. An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
ebiAttributes :: Lens' EndpointBatchItem (HashMap Text ([Text]))
ebiAttributes = lens _ebiAttributes (\s a -> s {_ebiAttributes = a}) . _Default . _Map

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
ebiEndpointStatus :: Lens' EndpointBatchItem (Maybe Text)
ebiEndpointStatus = lens _ebiEndpointStatus (\s a -> s {_ebiEndpointStatus = a})

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
ebiOptOut :: Lens' EndpointBatchItem (Maybe Text)
ebiOptOut = lens _ebiOptOut (\s a -> s {_ebiOptOut = a})

-- | The unique identifier for the endpoint in the context of the batch.
ebiId :: Lens' EndpointBatchItem (Maybe Text)
ebiId = lens _ebiId (\s a -> s {_ebiId = a})

-- | The channel to use when sending messages or push notifications to the endpoint.
ebiChannelType :: Lens' EndpointBatchItem (Maybe ChannelType)
ebiChannelType = lens _ebiChannelType (\s a -> s {_ebiChannelType = a})

instance Hashable EndpointBatchItem

instance NFData EndpointBatchItem

instance ToJSON EndpointBatchItem where
  toJSON EndpointBatchItem' {..} =
    object
      ( catMaybes
          [ ("RequestId" .=) <$> _ebiRequestId,
            ("Metrics" .=) <$> _ebiMetrics,
            ("Location" .=) <$> _ebiLocation,
            ("Demographic" .=) <$> _ebiDemographic,
            ("Address" .=) <$> _ebiAddress,
            ("EffectiveDate" .=) <$> _ebiEffectiveDate,
            ("User" .=) <$> _ebiUser,
            ("Attributes" .=) <$> _ebiAttributes,
            ("EndpointStatus" .=) <$> _ebiEndpointStatus,
            ("OptOut" .=) <$> _ebiOptOut,
            ("Id" .=) <$> _ebiId,
            ("ChannelType" .=) <$> _ebiChannelType
          ]
      )

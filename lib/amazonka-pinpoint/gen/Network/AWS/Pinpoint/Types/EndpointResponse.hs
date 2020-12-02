{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import Network.AWS.Prelude

-- | Provides information about the channel type and other settings for an endpoint.
--
--
--
-- /See:/ 'endpointResponse' smart constructor.
data EndpointResponse = EndpointResponse'
  { _endRequestId ::
      !(Maybe Text),
    _endMetrics :: !(Maybe (Map Text (Double))),
    _endLocation :: !(Maybe EndpointLocation),
    _endDemographic :: !(Maybe EndpointDemographic),
    _endCohortId :: !(Maybe Text),
    _endAddress :: !(Maybe Text),
    _endEffectiveDate :: !(Maybe Text),
    _endUser :: !(Maybe EndpointUser),
    _endApplicationId :: !(Maybe Text),
    _endAttributes :: !(Maybe (Map Text ([Text]))),
    _endEndpointStatus :: !(Maybe Text),
    _endOptOut :: !(Maybe Text),
    _endId :: !(Maybe Text),
    _endCreationDate :: !(Maybe Text),
    _endChannelType :: !(Maybe ChannelType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'endRequestId' - The unique identifier for the most recent request to update the endpoint.
--
-- * 'endMetrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- * 'endLocation' - The geographic information for the endpoint.
--
-- * 'endDemographic' - The demographic information for the endpoint, such as the time zone and platform.
--
-- * 'endCohortId' - A number from 0-99 that represents the cohort that the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an application. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for campaigns.
--
-- * 'endAddress' - The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For example, the address for a push-notification channel is typically the token provided by a push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. The address for the SMS channel is a phone number in E.164 format, such as +12065550100. The address for the email channel is an email address.
--
-- * 'endEffectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last updated.
--
-- * 'endUser' - One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
--
-- * 'endApplicationId' - The unique identifier for the application that's associated with the endpoint.
--
-- * 'endAttributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments.
--
-- * 'endEndpointStatus' - Specifies whether messages or push notifications are sent to the endpoint. Possible values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- * 'endOptOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- * 'endId' - The unique identifier that you assigned to the endpoint. The identifier should be a globally unique identifier (GUID) to ensure that it doesn't conflict with other endpoint identifiers that are associated with the application.
--
-- * 'endCreationDate' - The date and time, in ISO 8601 format, when the endpoint was created.
--
-- * 'endChannelType' - The channel that's used when sending messages or push notifications to the endpoint.
endpointResponse ::
  EndpointResponse
endpointResponse =
  EndpointResponse'
    { _endRequestId = Nothing,
      _endMetrics = Nothing,
      _endLocation = Nothing,
      _endDemographic = Nothing,
      _endCohortId = Nothing,
      _endAddress = Nothing,
      _endEffectiveDate = Nothing,
      _endUser = Nothing,
      _endApplicationId = Nothing,
      _endAttributes = Nothing,
      _endEndpointStatus = Nothing,
      _endOptOut = Nothing,
      _endId = Nothing,
      _endCreationDate = Nothing,
      _endChannelType = Nothing
    }

-- | The unique identifier for the most recent request to update the endpoint.
endRequestId :: Lens' EndpointResponse (Maybe Text)
endRequestId = lens _endRequestId (\s a -> s {_endRequestId = a})

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
endMetrics :: Lens' EndpointResponse (HashMap Text (Double))
endMetrics = lens _endMetrics (\s a -> s {_endMetrics = a}) . _Default . _Map

-- | The geographic information for the endpoint.
endLocation :: Lens' EndpointResponse (Maybe EndpointLocation)
endLocation = lens _endLocation (\s a -> s {_endLocation = a})

-- | The demographic information for the endpoint, such as the time zone and platform.
endDemographic :: Lens' EndpointResponse (Maybe EndpointDemographic)
endDemographic = lens _endDemographic (\s a -> s {_endDemographic = a})

-- | A number from 0-99 that represents the cohort that the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an application. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for campaigns.
endCohortId :: Lens' EndpointResponse (Maybe Text)
endCohortId = lens _endCohortId (\s a -> s {_endCohortId = a})

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For example, the address for a push-notification channel is typically the token provided by a push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. The address for the SMS channel is a phone number in E.164 format, such as +12065550100. The address for the email channel is an email address.
endAddress :: Lens' EndpointResponse (Maybe Text)
endAddress = lens _endAddress (\s a -> s {_endAddress = a})

-- | The date and time, in ISO 8601 format, when the endpoint was last updated.
endEffectiveDate :: Lens' EndpointResponse (Maybe Text)
endEffectiveDate = lens _endEffectiveDate (\s a -> s {_endEffectiveDate = a})

-- | One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
endUser :: Lens' EndpointResponse (Maybe EndpointUser)
endUser = lens _endUser (\s a -> s {_endUser = a})

-- | The unique identifier for the application that's associated with the endpoint.
endApplicationId :: Lens' EndpointResponse (Maybe Text)
endApplicationId = lens _endApplicationId (\s a -> s {_endApplicationId = a})

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments.
endAttributes :: Lens' EndpointResponse (HashMap Text ([Text]))
endAttributes = lens _endAttributes (\s a -> s {_endAttributes = a}) . _Default . _Map

-- | Specifies whether messages or push notifications are sent to the endpoint. Possible values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
endEndpointStatus :: Lens' EndpointResponse (Maybe Text)
endEndpointStatus = lens _endEndpointStatus (\s a -> s {_endEndpointStatus = a})

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
endOptOut :: Lens' EndpointResponse (Maybe Text)
endOptOut = lens _endOptOut (\s a -> s {_endOptOut = a})

-- | The unique identifier that you assigned to the endpoint. The identifier should be a globally unique identifier (GUID) to ensure that it doesn't conflict with other endpoint identifiers that are associated with the application.
endId :: Lens' EndpointResponse (Maybe Text)
endId = lens _endId (\s a -> s {_endId = a})

-- | The date and time, in ISO 8601 format, when the endpoint was created.
endCreationDate :: Lens' EndpointResponse (Maybe Text)
endCreationDate = lens _endCreationDate (\s a -> s {_endCreationDate = a})

-- | The channel that's used when sending messages or push notifications to the endpoint.
endChannelType :: Lens' EndpointResponse (Maybe ChannelType)
endChannelType = lens _endChannelType (\s a -> s {_endChannelType = a})

instance FromJSON EndpointResponse where
  parseJSON =
    withObject
      "EndpointResponse"
      ( \x ->
          EndpointResponse'
            <$> (x .:? "RequestId")
            <*> (x .:? "Metrics" .!= mempty)
            <*> (x .:? "Location")
            <*> (x .:? "Demographic")
            <*> (x .:? "CohortId")
            <*> (x .:? "Address")
            <*> (x .:? "EffectiveDate")
            <*> (x .:? "User")
            <*> (x .:? "ApplicationId")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "EndpointStatus")
            <*> (x .:? "OptOut")
            <*> (x .:? "Id")
            <*> (x .:? "CreationDate")
            <*> (x .:? "ChannelType")
      )

instance Hashable EndpointResponse

instance NFData EndpointResponse

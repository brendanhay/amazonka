{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PublicEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PublicEndpoint where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import Network.AWS.Prelude

-- | Specifies the properties and attributes of an endpoint that's associated with an event.
--
--
--
-- /See:/ 'publicEndpoint' smart constructor.
data PublicEndpoint = PublicEndpoint'
  { _peRequestId ::
      !(Maybe Text),
    _peMetrics :: !(Maybe (Map Text (Double))),
    _peLocation :: !(Maybe EndpointLocation),
    _peDemographic :: !(Maybe EndpointDemographic),
    _peAddress :: !(Maybe Text),
    _peEffectiveDate :: !(Maybe Text),
    _peUser :: !(Maybe EndpointUser),
    _peAttributes :: !(Maybe (Map Text ([Text]))),
    _peEndpointStatus :: !(Maybe Text),
    _peOptOut :: !(Maybe Text),
    _peChannelType :: !(Maybe ChannelType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peRequestId' - A unique identifier that's generated each time the endpoint is updated.
--
-- * 'peMetrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- * 'peLocation' - The geographic information for the endpoint.
--
-- * 'peDemographic' - The demographic information for the endpoint, such as the time zone and platform.
--
-- * 'peAddress' - The unique identifier for the recipient, such as a device token, email address, or mobile phone number.
--
-- * 'peEffectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last updated.
--
-- * 'peUser' - One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
--
-- * 'peAttributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. You can use these attributes as filter criteria when you create segments.
--
-- * 'peEndpointStatus' - Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- * 'peOptOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- * 'peChannelType' - The channel that's used when sending messages or push notifications to the endpoint.
publicEndpoint ::
  PublicEndpoint
publicEndpoint =
  PublicEndpoint'
    { _peRequestId = Nothing,
      _peMetrics = Nothing,
      _peLocation = Nothing,
      _peDemographic = Nothing,
      _peAddress = Nothing,
      _peEffectiveDate = Nothing,
      _peUser = Nothing,
      _peAttributes = Nothing,
      _peEndpointStatus = Nothing,
      _peOptOut = Nothing,
      _peChannelType = Nothing
    }

-- | A unique identifier that's generated each time the endpoint is updated.
peRequestId :: Lens' PublicEndpoint (Maybe Text)
peRequestId = lens _peRequestId (\s a -> s {_peRequestId = a})

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
peMetrics :: Lens' PublicEndpoint (HashMap Text (Double))
peMetrics = lens _peMetrics (\s a -> s {_peMetrics = a}) . _Default . _Map

-- | The geographic information for the endpoint.
peLocation :: Lens' PublicEndpoint (Maybe EndpointLocation)
peLocation = lens _peLocation (\s a -> s {_peLocation = a})

-- | The demographic information for the endpoint, such as the time zone and platform.
peDemographic :: Lens' PublicEndpoint (Maybe EndpointDemographic)
peDemographic = lens _peDemographic (\s a -> s {_peDemographic = a})

-- | The unique identifier for the recipient, such as a device token, email address, or mobile phone number.
peAddress :: Lens' PublicEndpoint (Maybe Text)
peAddress = lens _peAddress (\s a -> s {_peAddress = a})

-- | The date and time, in ISO 8601 format, when the endpoint was last updated.
peEffectiveDate :: Lens' PublicEndpoint (Maybe Text)
peEffectiveDate = lens _peEffectiveDate (\s a -> s {_peEffectiveDate = a})

-- | One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
peUser :: Lens' PublicEndpoint (Maybe EndpointUser)
peUser = lens _peUser (\s a -> s {_peUser = a})

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. You can use these attributes as filter criteria when you create segments.
peAttributes :: Lens' PublicEndpoint (HashMap Text ([Text]))
peAttributes = lens _peAttributes (\s a -> s {_peAttributes = a}) . _Default . _Map

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint. Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
peEndpointStatus :: Lens' PublicEndpoint (Maybe Text)
peEndpointStatus = lens _peEndpointStatus (\s a -> s {_peEndpointStatus = a})

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
peOptOut :: Lens' PublicEndpoint (Maybe Text)
peOptOut = lens _peOptOut (\s a -> s {_peOptOut = a})

-- | The channel that's used when sending messages or push notifications to the endpoint.
peChannelType :: Lens' PublicEndpoint (Maybe ChannelType)
peChannelType = lens _peChannelType (\s a -> s {_peChannelType = a})

instance Hashable PublicEndpoint

instance NFData PublicEndpoint

instance ToJSON PublicEndpoint where
  toJSON PublicEndpoint' {..} =
    object
      ( catMaybes
          [ ("RequestId" .=) <$> _peRequestId,
            ("Metrics" .=) <$> _peMetrics,
            ("Location" .=) <$> _peLocation,
            ("Demographic" .=) <$> _peDemographic,
            ("Address" .=) <$> _peAddress,
            ("EffectiveDate" .=) <$> _peEffectiveDate,
            ("User" .=) <$> _peUser,
            ("Attributes" .=) <$> _peAttributes,
            ("EndpointStatus" .=) <$> _peEndpointStatus,
            ("OptOut" .=) <$> _peOptOut,
            ("ChannelType" .=) <$> _peChannelType
          ]
      )

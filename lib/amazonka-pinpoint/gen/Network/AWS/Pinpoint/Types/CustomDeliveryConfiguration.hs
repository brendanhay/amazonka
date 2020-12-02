{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EndpointTypesElement
import Network.AWS.Prelude

-- | Specifies the delivery configuration settings for sending a campaign or campaign treatment through a custom channel. This object is required if you use the CampaignCustomMessage object to define the message to send for the campaign or campaign treatment.
--
--
--
-- /See:/ 'customDeliveryConfiguration' smart constructor.
data CustomDeliveryConfiguration = CustomDeliveryConfiguration'
  { _cdcEndpointTypes ::
      !(Maybe [EndpointTypesElement]),
    _cdcDeliveryURI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomDeliveryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcEndpointTypes' - The types of endpoints to send the campaign or treatment to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
--
-- * 'cdcDeliveryURI' - The destination to send the campaign or treatment to. This value can be one of the following:     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
customDeliveryConfiguration ::
  -- | 'cdcDeliveryURI'
  Text ->
  CustomDeliveryConfiguration
customDeliveryConfiguration pDeliveryURI_ =
  CustomDeliveryConfiguration'
    { _cdcEndpointTypes = Nothing,
      _cdcDeliveryURI = pDeliveryURI_
    }

-- | The types of endpoints to send the campaign or treatment to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
cdcEndpointTypes :: Lens' CustomDeliveryConfiguration [EndpointTypesElement]
cdcEndpointTypes = lens _cdcEndpointTypes (\s a -> s {_cdcEndpointTypes = a}) . _Default . _Coerce

-- | The destination to send the campaign or treatment to. This value can be one of the following:     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
cdcDeliveryURI :: Lens' CustomDeliveryConfiguration Text
cdcDeliveryURI = lens _cdcDeliveryURI (\s a -> s {_cdcDeliveryURI = a})

instance FromJSON CustomDeliveryConfiguration where
  parseJSON =
    withObject
      "CustomDeliveryConfiguration"
      ( \x ->
          CustomDeliveryConfiguration'
            <$> (x .:? "EndpointTypes" .!= mempty) <*> (x .: "DeliveryUri")
      )

instance Hashable CustomDeliveryConfiguration

instance NFData CustomDeliveryConfiguration

instance ToJSON CustomDeliveryConfiguration where
  toJSON CustomDeliveryConfiguration' {..} =
    object
      ( catMaybes
          [ ("EndpointTypes" .=) <$> _cdcEndpointTypes,
            Just ("DeliveryUri" .= _cdcDeliveryURI)
          ]
      )

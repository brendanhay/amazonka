{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Endpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Endpoint for mobile app and device.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eAttributes ::
      !(Maybe (Map Text (Text))),
    _eEndpointARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAttributes' - Attributes for endpoint.
--
-- * 'eEndpointARN' - EndpointArn for mobile app and device.
endpoint ::
  Endpoint
endpoint =
  Endpoint' {_eAttributes = Nothing, _eEndpointARN = Nothing}

-- | Attributes for endpoint.
eAttributes :: Lens' Endpoint (HashMap Text (Text))
eAttributes = lens _eAttributes (\s a -> s {_eAttributes = a}) . _Default . _Map

-- | EndpointArn for mobile app and device.
eEndpointARN :: Lens' Endpoint (Maybe Text)
eEndpointARN = lens _eEndpointARN (\s a -> s {_eEndpointARN = a})

instance FromXML Endpoint where
  parseXML x =
    Endpoint'
      <$> ( x .@? "Attributes" .!@ mempty
              >>= may (parseXMLMap "entry" "key" "value")
          )
      <*> (x .@? "EndpointArn")

instance Hashable Endpoint

instance NFData Endpoint

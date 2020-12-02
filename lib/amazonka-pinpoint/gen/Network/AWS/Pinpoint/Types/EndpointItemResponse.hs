{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointItemResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the status code and message that result from processing data for an endpoint.
--
--
--
-- /See:/ 'endpointItemResponse' smart constructor.
data EndpointItemResponse = EndpointItemResponse'
  { _eiMessage ::
      !(Maybe Text),
    _eiStatusCode :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiMessage' - The custom message that's returned in the response as a result of processing the endpoint data.
--
-- * 'eiStatusCode' - The status code that's returned in the response as a result of processing the endpoint data.
endpointItemResponse ::
  EndpointItemResponse
endpointItemResponse =
  EndpointItemResponse'
    { _eiMessage = Nothing,
      _eiStatusCode = Nothing
    }

-- | The custom message that's returned in the response as a result of processing the endpoint data.
eiMessage :: Lens' EndpointItemResponse (Maybe Text)
eiMessage = lens _eiMessage (\s a -> s {_eiMessage = a})

-- | The status code that's returned in the response as a result of processing the endpoint data.
eiStatusCode :: Lens' EndpointItemResponse (Maybe Int)
eiStatusCode = lens _eiStatusCode (\s a -> s {_eiStatusCode = a})

instance FromJSON EndpointItemResponse where
  parseJSON =
    withObject
      "EndpointItemResponse"
      ( \x ->
          EndpointItemResponse'
            <$> (x .:? "Message") <*> (x .:? "StatusCode")
      )

instance Hashable EndpointItemResponse

instance NFData EndpointItemResponse

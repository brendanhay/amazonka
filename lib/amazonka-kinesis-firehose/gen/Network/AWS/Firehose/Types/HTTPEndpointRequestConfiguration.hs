{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration where

import Network.AWS.Firehose.Types.ContentEncoding
import Network.AWS.Firehose.Types.HTTPEndpointCommonAttribute
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration of the HTTP endpoint request.
--
--
--
-- /See:/ 'hTTPEndpointRequestConfiguration' smart constructor.
data HTTPEndpointRequestConfiguration = HTTPEndpointRequestConfiguration'
  { _httpercCommonAttributes ::
      !( Maybe
           [HTTPEndpointCommonAttribute]
       ),
    _httpercContentEncoding ::
      !(Maybe ContentEncoding)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointRequestConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpercCommonAttributes' - Describes the metadata sent to the HTTP endpoint destination.
--
-- * 'httpercContentEncoding' - Kinesis Data Firehose uses the content encoding to compress the body of a request before sending the request to the destination. For more information, see <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding> in MDN Web Docs, the official Mozilla documentation.
hTTPEndpointRequestConfiguration ::
  HTTPEndpointRequestConfiguration
hTTPEndpointRequestConfiguration =
  HTTPEndpointRequestConfiguration'
    { _httpercCommonAttributes =
        Nothing,
      _httpercContentEncoding = Nothing
    }

-- | Describes the metadata sent to the HTTP endpoint destination.
httpercCommonAttributes :: Lens' HTTPEndpointRequestConfiguration [HTTPEndpointCommonAttribute]
httpercCommonAttributes = lens _httpercCommonAttributes (\s a -> s {_httpercCommonAttributes = a}) . _Default . _Coerce

-- | Kinesis Data Firehose uses the content encoding to compress the body of a request before sending the request to the destination. For more information, see <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding> in MDN Web Docs, the official Mozilla documentation.
httpercContentEncoding :: Lens' HTTPEndpointRequestConfiguration (Maybe ContentEncoding)
httpercContentEncoding = lens _httpercContentEncoding (\s a -> s {_httpercContentEncoding = a})

instance FromJSON HTTPEndpointRequestConfiguration where
  parseJSON =
    withObject
      "HTTPEndpointRequestConfiguration"
      ( \x ->
          HTTPEndpointRequestConfiguration'
            <$> (x .:? "CommonAttributes" .!= mempty)
            <*> (x .:? "ContentEncoding")
      )

instance Hashable HTTPEndpointRequestConfiguration

instance NFData HTTPEndpointRequestConfiguration

instance ToJSON HTTPEndpointRequestConfiguration where
  toJSON HTTPEndpointRequestConfiguration' {..} =
    object
      ( catMaybes
          [ ("CommonAttributes" .=) <$> _httpercCommonAttributes,
            ("ContentEncoding" .=) <$> _httpercContentEncoding
          ]
      )

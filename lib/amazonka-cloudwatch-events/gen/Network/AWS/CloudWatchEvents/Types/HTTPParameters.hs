{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.HTTPParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.HTTPParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | These are custom parameter to be used when the target is an API Gateway REST APIs.
--
--
--
-- /See:/ 'hTTPParameters' smart constructor.
data HTTPParameters = HTTPParameters'
  { _httppPathParameterValues ::
      !(Maybe [Text]),
    _httppQueryStringParameters :: !(Maybe (Map Text (Text))),
    _httppHeaderParameters :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httppPathParameterValues' - The path parameter values to be used to populate API Gateway REST API path wildcards ("*").
--
-- * 'httppQueryStringParameters' - The query string keys/values that need to be sent as part of request invoking the API Gateway REST API.
--
-- * 'httppHeaderParameters' - The headers that need to be sent as part of request invoking the API Gateway REST API.
hTTPParameters ::
  HTTPParameters
hTTPParameters =
  HTTPParameters'
    { _httppPathParameterValues = Nothing,
      _httppQueryStringParameters = Nothing,
      _httppHeaderParameters = Nothing
    }

-- | The path parameter values to be used to populate API Gateway REST API path wildcards ("*").
httppPathParameterValues :: Lens' HTTPParameters [Text]
httppPathParameterValues = lens _httppPathParameterValues (\s a -> s {_httppPathParameterValues = a}) . _Default . _Coerce

-- | The query string keys/values that need to be sent as part of request invoking the API Gateway REST API.
httppQueryStringParameters :: Lens' HTTPParameters (HashMap Text (Text))
httppQueryStringParameters = lens _httppQueryStringParameters (\s a -> s {_httppQueryStringParameters = a}) . _Default . _Map

-- | The headers that need to be sent as part of request invoking the API Gateway REST API.
httppHeaderParameters :: Lens' HTTPParameters (HashMap Text (Text))
httppHeaderParameters = lens _httppHeaderParameters (\s a -> s {_httppHeaderParameters = a}) . _Default . _Map

instance FromJSON HTTPParameters where
  parseJSON =
    withObject
      "HTTPParameters"
      ( \x ->
          HTTPParameters'
            <$> (x .:? "PathParameterValues" .!= mempty)
            <*> (x .:? "QueryStringParameters" .!= mempty)
            <*> (x .:? "HeaderParameters" .!= mempty)
      )

instance Hashable HTTPParameters

instance NFData HTTPParameters

instance ToJSON HTTPParameters where
  toJSON HTTPParameters' {..} =
    object
      ( catMaybes
          [ ("PathParameterValues" .=) <$> _httppPathParameterValues,
            ("QueryStringParameters" .=) <$> _httppQueryStringParameters,
            ("HeaderParameters" .=) <$> _httppHeaderParameters
          ]
      )

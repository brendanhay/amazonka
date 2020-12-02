{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ElasticsearchSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ElasticsearchSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines an Elasticsearch endpoint.
--
--
--
-- /See:/ 'elasticsearchSettings' smart constructor.
data ElasticsearchSettings = ElasticsearchSettings'
  { _esFullLoadErrorPercentage ::
      !(Maybe Int),
    _esErrorRetryDuration :: !(Maybe Int),
    _esServiceAccessRoleARN :: !Text,
    _esEndpointURI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esFullLoadErrorPercentage' - The maximum percentage of records that can fail to be written before a full load operation stops. To avoid early failure, this counter is only effective after 1000 records are transferred. Elasticsearch also has the concept of error monitoring during the last 10 minutes of an Observation Window. If transfer of all records fail in the last 10 minutes, the full load operation stops.
--
-- * 'esErrorRetryDuration' - The maximum number of seconds for which DMS retries failed API requests to the Elasticsearch cluster.
--
-- * 'esServiceAccessRoleARN' - The Amazon Resource Name (ARN) used by service to access the IAM role.
--
-- * 'esEndpointURI' - The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a transport protocol (http/https) is not specified.
elasticsearchSettings ::
  -- | 'esServiceAccessRoleARN'
  Text ->
  -- | 'esEndpointURI'
  Text ->
  ElasticsearchSettings
elasticsearchSettings pServiceAccessRoleARN_ pEndpointURI_ =
  ElasticsearchSettings'
    { _esFullLoadErrorPercentage = Nothing,
      _esErrorRetryDuration = Nothing,
      _esServiceAccessRoleARN = pServiceAccessRoleARN_,
      _esEndpointURI = pEndpointURI_
    }

-- | The maximum percentage of records that can fail to be written before a full load operation stops. To avoid early failure, this counter is only effective after 1000 records are transferred. Elasticsearch also has the concept of error monitoring during the last 10 minutes of an Observation Window. If transfer of all records fail in the last 10 minutes, the full load operation stops.
esFullLoadErrorPercentage :: Lens' ElasticsearchSettings (Maybe Int)
esFullLoadErrorPercentage = lens _esFullLoadErrorPercentage (\s a -> s {_esFullLoadErrorPercentage = a})

-- | The maximum number of seconds for which DMS retries failed API requests to the Elasticsearch cluster.
esErrorRetryDuration :: Lens' ElasticsearchSettings (Maybe Int)
esErrorRetryDuration = lens _esErrorRetryDuration (\s a -> s {_esErrorRetryDuration = a})

-- | The Amazon Resource Name (ARN) used by service to access the IAM role.
esServiceAccessRoleARN :: Lens' ElasticsearchSettings Text
esServiceAccessRoleARN = lens _esServiceAccessRoleARN (\s a -> s {_esServiceAccessRoleARN = a})

-- | The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a transport protocol (http/https) is not specified.
esEndpointURI :: Lens' ElasticsearchSettings Text
esEndpointURI = lens _esEndpointURI (\s a -> s {_esEndpointURI = a})

instance FromJSON ElasticsearchSettings where
  parseJSON =
    withObject
      "ElasticsearchSettings"
      ( \x ->
          ElasticsearchSettings'
            <$> (x .:? "FullLoadErrorPercentage")
            <*> (x .:? "ErrorRetryDuration")
            <*> (x .: "ServiceAccessRoleArn")
            <*> (x .: "EndpointUri")
      )

instance Hashable ElasticsearchSettings

instance NFData ElasticsearchSettings

instance ToJSON ElasticsearchSettings where
  toJSON ElasticsearchSettings' {..} =
    object
      ( catMaybes
          [ ("FullLoadErrorPercentage" .=) <$> _esFullLoadErrorPercentage,
            ("ErrorRetryDuration" .=) <$> _esErrorRetryDuration,
            Just ("ServiceAccessRoleArn" .= _esServiceAccessRoleARN),
            Just ("EndpointUri" .= _esEndpointURI)
          ]
      )

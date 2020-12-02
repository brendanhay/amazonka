{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Elasticsearch data source configuration.
--
--
--
-- /See:/ 'elasticsearchDataSourceConfig' smart constructor.
data ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig'
  { _edscEndpoint ::
      !Text,
    _edscAwsRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchDataSourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edscEndpoint' - The endpoint.
--
-- * 'edscAwsRegion' - The AWS Region.
elasticsearchDataSourceConfig ::
  -- | 'edscEndpoint'
  Text ->
  -- | 'edscAwsRegion'
  Text ->
  ElasticsearchDataSourceConfig
elasticsearchDataSourceConfig pEndpoint_ pAwsRegion_ =
  ElasticsearchDataSourceConfig'
    { _edscEndpoint = pEndpoint_,
      _edscAwsRegion = pAwsRegion_
    }

-- | The endpoint.
edscEndpoint :: Lens' ElasticsearchDataSourceConfig Text
edscEndpoint = lens _edscEndpoint (\s a -> s {_edscEndpoint = a})

-- | The AWS Region.
edscAwsRegion :: Lens' ElasticsearchDataSourceConfig Text
edscAwsRegion = lens _edscAwsRegion (\s a -> s {_edscAwsRegion = a})

instance FromJSON ElasticsearchDataSourceConfig where
  parseJSON =
    withObject
      "ElasticsearchDataSourceConfig"
      ( \x ->
          ElasticsearchDataSourceConfig'
            <$> (x .: "endpoint") <*> (x .: "awsRegion")
      )

instance Hashable ElasticsearchDataSourceConfig

instance NFData ElasticsearchDataSourceConfig

instance ToJSON ElasticsearchDataSourceConfig where
  toJSON ElasticsearchDataSourceConfig' {..} =
    object
      ( catMaybes
          [ Just ("endpoint" .= _edscEndpoint),
            Just ("awsRegion" .= _edscAwsRegion)
          ]
      )

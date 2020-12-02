{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus where

import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the configuration status for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchClusterConfigStatus' smart constructor.
data ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus'
  { _eccsOptions ::
      !ElasticsearchClusterConfig,
    _eccsStatus ::
      !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchClusterConfigStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eccsOptions' - Specifies the cluster configuration for the specified Elasticsearch domain.
--
-- * 'eccsStatus' - Specifies the status of the configuration for the specified Elasticsearch domain.
elasticsearchClusterConfigStatus ::
  -- | 'eccsOptions'
  ElasticsearchClusterConfig ->
  -- | 'eccsStatus'
  OptionStatus ->
  ElasticsearchClusterConfigStatus
elasticsearchClusterConfigStatus pOptions_ pStatus_ =
  ElasticsearchClusterConfigStatus'
    { _eccsOptions = pOptions_,
      _eccsStatus = pStatus_
    }

-- | Specifies the cluster configuration for the specified Elasticsearch domain.
eccsOptions :: Lens' ElasticsearchClusterConfigStatus ElasticsearchClusterConfig
eccsOptions = lens _eccsOptions (\s a -> s {_eccsOptions = a})

-- | Specifies the status of the configuration for the specified Elasticsearch domain.
eccsStatus :: Lens' ElasticsearchClusterConfigStatus OptionStatus
eccsStatus = lens _eccsStatus (\s a -> s {_eccsStatus = a})

instance FromJSON ElasticsearchClusterConfigStatus where
  parseJSON =
    withObject
      "ElasticsearchClusterConfigStatus"
      ( \x ->
          ElasticsearchClusterConfigStatus'
            <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable ElasticsearchClusterConfigStatus

instance NFData ElasticsearchClusterConfigStatus

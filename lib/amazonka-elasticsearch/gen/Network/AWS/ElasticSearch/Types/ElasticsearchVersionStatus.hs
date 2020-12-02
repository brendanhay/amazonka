{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the Elasticsearch version options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchVersionStatus' smart constructor.
data ElasticsearchVersionStatus = ElasticsearchVersionStatus'
  { _evsOptions ::
      !Text,
    _evsStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchVersionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evsOptions' - Specifies the Elasticsearch version for the specified Elasticsearch domain.
--
-- * 'evsStatus' - Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
elasticsearchVersionStatus ::
  -- | 'evsOptions'
  Text ->
  -- | 'evsStatus'
  OptionStatus ->
  ElasticsearchVersionStatus
elasticsearchVersionStatus pOptions_ pStatus_ =
  ElasticsearchVersionStatus'
    { _evsOptions = pOptions_,
      _evsStatus = pStatus_
    }

-- | Specifies the Elasticsearch version for the specified Elasticsearch domain.
evsOptions :: Lens' ElasticsearchVersionStatus Text
evsOptions = lens _evsOptions (\s a -> s {_evsOptions = a})

-- | Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
evsStatus :: Lens' ElasticsearchVersionStatus OptionStatus
evsStatus = lens _evsStatus (\s a -> s {_evsStatus = a})

instance FromJSON ElasticsearchVersionStatus where
  parseJSON =
    withObject
      "ElasticsearchVersionStatus"
      ( \x ->
          ElasticsearchVersionStatus'
            <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable ElasticsearchVersionStatus

instance NFData ElasticsearchVersionStatus

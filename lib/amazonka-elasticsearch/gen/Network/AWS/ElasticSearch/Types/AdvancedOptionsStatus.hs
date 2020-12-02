{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:
--
--
--     * Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.    * Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.
--
-- For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> .
--
--
-- /See:/ 'advancedOptionsStatus' smart constructor.
data AdvancedOptionsStatus = AdvancedOptionsStatus'
  { _aosOptions ::
      !(Map Text (Text)),
    _aosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdvancedOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aosOptions' - Specifies the status of advanced options for the specified Elasticsearch domain.
--
-- * 'aosStatus' - Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
advancedOptionsStatus ::
  -- | 'aosStatus'
  OptionStatus ->
  AdvancedOptionsStatus
advancedOptionsStatus pStatus_ =
  AdvancedOptionsStatus'
    { _aosOptions = mempty,
      _aosStatus = pStatus_
    }

-- | Specifies the status of advanced options for the specified Elasticsearch domain.
aosOptions :: Lens' AdvancedOptionsStatus (HashMap Text (Text))
aosOptions = lens _aosOptions (\s a -> s {_aosOptions = a}) . _Map

-- | Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
aosStatus :: Lens' AdvancedOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\s a -> s {_aosStatus = a})

instance FromJSON AdvancedOptionsStatus where
  parseJSON =
    withObject
      "AdvancedOptionsStatus"
      ( \x ->
          AdvancedOptionsStatus'
            <$> (x .:? "Options" .!= mempty) <*> (x .: "Status")
      )

instance Hashable AdvancedOptionsStatus

instance NFData AdvancedOptionsStatus

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EBSOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EBSOptionsStatus where

import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the EBS options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'ebsOptionsStatus' smart constructor.
data EBSOptionsStatus = EBSOptionsStatus'
  { _eosOptions ::
      !EBSOptions,
    _eosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eosOptions' - Specifies the EBS options for the specified Elasticsearch domain.
--
-- * 'eosStatus' - Specifies the status of the EBS options for the specified Elasticsearch domain.
ebsOptionsStatus ::
  -- | 'eosOptions'
  EBSOptions ->
  -- | 'eosStatus'
  OptionStatus ->
  EBSOptionsStatus
ebsOptionsStatus pOptions_ pStatus_ =
  EBSOptionsStatus' {_eosOptions = pOptions_, _eosStatus = pStatus_}

-- | Specifies the EBS options for the specified Elasticsearch domain.
eosOptions :: Lens' EBSOptionsStatus EBSOptions
eosOptions = lens _eosOptions (\s a -> s {_eosOptions = a})

-- | Specifies the status of the EBS options for the specified Elasticsearch domain.
eosStatus :: Lens' EBSOptionsStatus OptionStatus
eosStatus = lens _eosStatus (\s a -> s {_eosStatus = a})

instance FromJSON EBSOptionsStatus where
  parseJSON =
    withObject
      "EBSOptionsStatus"
      (\x -> EBSOptionsStatus' <$> (x .: "Options") <*> (x .: "Status"))

instance Hashable EBSOptionsStatus

instance NFData EBSOptionsStatus

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Limits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.Limits where

import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.StorageType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Limits for given InstanceType and for each of it's role.
--
-- Limits contains following @'StorageTypes,' @ @'InstanceLimits' @ and @'AdditionalLimits' @
--
--
-- /See:/ 'limits' smart constructor.
data Limits = Limits'
  { _lInstanceLimits :: !(Maybe InstanceLimits),
    _lAdditionalLimits :: !(Maybe [AdditionalLimit]),
    _lStorageTypes :: !(Maybe [StorageType])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Limits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lInstanceLimits' - Undocumented member.
--
-- * 'lAdditionalLimits' - List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
--
-- * 'lStorageTypes' - StorageType represents the list of storage related types and attributes that are available for given InstanceType.
limits ::
  Limits
limits =
  Limits'
    { _lInstanceLimits = Nothing,
      _lAdditionalLimits = Nothing,
      _lStorageTypes = Nothing
    }

-- | Undocumented member.
lInstanceLimits :: Lens' Limits (Maybe InstanceLimits)
lInstanceLimits = lens _lInstanceLimits (\s a -> s {_lInstanceLimits = a})

-- | List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
lAdditionalLimits :: Lens' Limits [AdditionalLimit]
lAdditionalLimits = lens _lAdditionalLimits (\s a -> s {_lAdditionalLimits = a}) . _Default . _Coerce

-- | StorageType represents the list of storage related types and attributes that are available for given InstanceType.
lStorageTypes :: Lens' Limits [StorageType]
lStorageTypes = lens _lStorageTypes (\s a -> s {_lStorageTypes = a}) . _Default . _Coerce

instance FromJSON Limits where
  parseJSON =
    withObject
      "Limits"
      ( \x ->
          Limits'
            <$> (x .:? "InstanceLimits")
            <*> (x .:? "AdditionalLimits" .!= mempty)
            <*> (x .:? "StorageTypes" .!= mempty)
      )

instance Hashable Limits

instance NFData Limits

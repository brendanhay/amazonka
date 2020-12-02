{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.StorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.StorageType where

import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | StorageTypes represents the list of storage related types and their attributes that are available for given InstanceType.
--
--
--
-- /See:/ 'storageType' smart constructor.
data StorageType = StorageType'
  { _stStorageTypeLimits ::
      !(Maybe [StorageTypeLimit]),
    _stStorageSubTypeName :: !(Maybe Text),
    _stStorageTypeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StorageType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stStorageTypeLimits' - List of limits that are applicable for given storage type.
--
-- * 'stStorageSubTypeName' - Undocumented member.
--
-- * 'stStorageTypeName' - Undocumented member.
storageType ::
  StorageType
storageType =
  StorageType'
    { _stStorageTypeLimits = Nothing,
      _stStorageSubTypeName = Nothing,
      _stStorageTypeName = Nothing
    }

-- | List of limits that are applicable for given storage type.
stStorageTypeLimits :: Lens' StorageType [StorageTypeLimit]
stStorageTypeLimits = lens _stStorageTypeLimits (\s a -> s {_stStorageTypeLimits = a}) . _Default . _Coerce

-- | Undocumented member.
stStorageSubTypeName :: Lens' StorageType (Maybe Text)
stStorageSubTypeName = lens _stStorageSubTypeName (\s a -> s {_stStorageSubTypeName = a})

-- | Undocumented member.
stStorageTypeName :: Lens' StorageType (Maybe Text)
stStorageTypeName = lens _stStorageTypeName (\s a -> s {_stStorageTypeName = a})

instance FromJSON StorageType where
  parseJSON =
    withObject
      "StorageType"
      ( \x ->
          StorageType'
            <$> (x .:? "StorageTypeLimits" .!= mempty)
            <*> (x .:? "StorageSubTypeName")
            <*> (x .:? "StorageTypeName")
      )

instance Hashable StorageType

instance NFData StorageType

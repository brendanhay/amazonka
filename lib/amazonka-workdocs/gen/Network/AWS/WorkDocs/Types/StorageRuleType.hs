{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.StorageRuleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.StorageRuleType where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.StorageType

-- | Describes the storage for a user.
--
--
--
-- /See:/ 'storageRuleType' smart constructor.
data StorageRuleType = StorageRuleType'
  { _srtStorageAllocatedInBytes ::
      !(Maybe Nat),
    _srtStorageType :: !(Maybe StorageType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StorageRuleType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtStorageAllocatedInBytes' - The amount of storage allocated, in bytes.
--
-- * 'srtStorageType' - The type of storage.
storageRuleType ::
  StorageRuleType
storageRuleType =
  StorageRuleType'
    { _srtStorageAllocatedInBytes = Nothing,
      _srtStorageType = Nothing
    }

-- | The amount of storage allocated, in bytes.
srtStorageAllocatedInBytes :: Lens' StorageRuleType (Maybe Natural)
srtStorageAllocatedInBytes = lens _srtStorageAllocatedInBytes (\s a -> s {_srtStorageAllocatedInBytes = a}) . mapping _Nat

-- | The type of storage.
srtStorageType :: Lens' StorageRuleType (Maybe StorageType)
srtStorageType = lens _srtStorageType (\s a -> s {_srtStorageType = a})

instance FromJSON StorageRuleType where
  parseJSON =
    withObject
      "StorageRuleType"
      ( \x ->
          StorageRuleType'
            <$> (x .:? "StorageAllocatedInBytes") <*> (x .:? "StorageType")
      )

instance Hashable StorageRuleType

instance NFData StorageRuleType

instance ToJSON StorageRuleType where
  toJSON StorageRuleType' {..} =
    object
      ( catMaybes
          [ ("StorageAllocatedInBytes" .=) <$> _srtStorageAllocatedInBytes,
            ("StorageType" .=) <$> _srtStorageType
          ]
      )

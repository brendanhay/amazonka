{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.StorageTypeLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.StorageTypeLimit where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Limits that are applicable for given storage type.
--
--
--
-- /See:/ 'storageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { _stlLimitName ::
      !(Maybe Text),
    _stlLimitValues :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StorageTypeLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stlLimitName' - Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable     * MinimumVolumeSizeMinimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumVolumeSizeMaximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumIopsMaximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.     * MinimumIopsMinimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
--
-- * 'stlLimitValues' - Values for the @'StorageTypeLimit$LimitName' @ .
storageTypeLimit ::
  StorageTypeLimit
storageTypeLimit =
  StorageTypeLimit'
    { _stlLimitName = Nothing,
      _stlLimitValues = Nothing
    }

-- | Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable     * MinimumVolumeSizeMinimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumVolumeSizeMaximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumIopsMaximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.     * MinimumIopsMinimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
stlLimitName :: Lens' StorageTypeLimit (Maybe Text)
stlLimitName = lens _stlLimitName (\s a -> s {_stlLimitName = a})

-- | Values for the @'StorageTypeLimit$LimitName' @ .
stlLimitValues :: Lens' StorageTypeLimit [Text]
stlLimitValues = lens _stlLimitValues (\s a -> s {_stlLimitValues = a}) . _Default . _Coerce

instance FromJSON StorageTypeLimit where
  parseJSON =
    withObject
      "StorageTypeLimit"
      ( \x ->
          StorageTypeLimit'
            <$> (x .:? "LimitName") <*> (x .:? "LimitValues" .!= mempty)
      )

instance Hashable StorageTypeLimit

instance NFData StorageTypeLimit

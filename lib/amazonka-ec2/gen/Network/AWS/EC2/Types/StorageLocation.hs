{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StorageLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StorageLocation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a storage location in Amazon S3.
--
--
--
-- /See:/ 'storageLocation' smart constructor.
data StorageLocation = StorageLocation'
  { _slBucket :: !(Maybe Text),
    _slKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StorageLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucket' - The name of the S3 bucket.
--
-- * 'slKey' - The key.
storageLocation ::
  StorageLocation
storageLocation =
  StorageLocation' {_slBucket = Nothing, _slKey = Nothing}

-- | The name of the S3 bucket.
slBucket :: Lens' StorageLocation (Maybe Text)
slBucket = lens _slBucket (\s a -> s {_slBucket = a})

-- | The key.
slKey :: Lens' StorageLocation (Maybe Text)
slKey = lens _slKey (\s a -> s {_slKey = a})

instance Hashable StorageLocation

instance NFData StorageLocation

instance ToQuery StorageLocation where
  toQuery StorageLocation' {..} =
    mconcat ["Bucket" =: _slBucket, "Key" =: _slKey]

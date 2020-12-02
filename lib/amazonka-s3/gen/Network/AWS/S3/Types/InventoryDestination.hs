{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryS3BucketDestination

-- | Specifies the inventory configuration for an Amazon S3 bucket.
--
--
--
-- /See:/ 'inventoryDestination' smart constructor.
newtype InventoryDestination = InventoryDestination'
  { _idS3BucketDestination ::
      InventoryS3BucketDestination
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idS3BucketDestination' - Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
inventoryDestination ::
  -- | 'idS3BucketDestination'
  InventoryS3BucketDestination ->
  InventoryDestination
inventoryDestination pS3BucketDestination_ =
  InventoryDestination'
    { _idS3BucketDestination =
        pS3BucketDestination_
    }

-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
idS3BucketDestination :: Lens' InventoryDestination InventoryS3BucketDestination
idS3BucketDestination = lens _idS3BucketDestination (\s a -> s {_idS3BucketDestination = a})

instance FromXML InventoryDestination where
  parseXML x = InventoryDestination' <$> (x .@ "S3BucketDestination")

instance Hashable InventoryDestination

instance NFData InventoryDestination

instance ToXML InventoryDestination where
  toXML InventoryDestination' {..} =
    mconcat ["S3BucketDestination" @= _idS3BucketDestination]

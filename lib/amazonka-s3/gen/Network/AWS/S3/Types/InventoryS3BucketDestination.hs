{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryS3BucketDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryS3BucketDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryEncryption
import Network.AWS.S3.Types.InventoryFormat

-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
--
--
--
-- /See:/ 'inventoryS3BucketDestination' smart constructor.
data InventoryS3BucketDestination = InventoryS3BucketDestination'
  { _isbdPrefix ::
      !(Maybe Text),
    _isbdAccountId :: !(Maybe Text),
    _isbdEncryption ::
      !(Maybe InventoryEncryption),
    _isbdBucket :: !BucketName,
    _isbdFormat :: !InventoryFormat
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryS3BucketDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isbdPrefix' - The prefix that is prepended to all inventory results.
--
-- * 'isbdAccountId' - The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- * 'isbdEncryption' - Contains the type of server-side encryption used to encrypt the inventory results.
--
-- * 'isbdBucket' - The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
--
-- * 'isbdFormat' - Specifies the output format of the inventory results.
inventoryS3BucketDestination ::
  -- | 'isbdBucket'
  BucketName ->
  -- | 'isbdFormat'
  InventoryFormat ->
  InventoryS3BucketDestination
inventoryS3BucketDestination pBucket_ pFormat_ =
  InventoryS3BucketDestination'
    { _isbdPrefix = Nothing,
      _isbdAccountId = Nothing,
      _isbdEncryption = Nothing,
      _isbdBucket = pBucket_,
      _isbdFormat = pFormat_
    }

-- | The prefix that is prepended to all inventory results.
isbdPrefix :: Lens' InventoryS3BucketDestination (Maybe Text)
isbdPrefix = lens _isbdPrefix (\s a -> s {_isbdPrefix = a})

-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
isbdAccountId :: Lens' InventoryS3BucketDestination (Maybe Text)
isbdAccountId = lens _isbdAccountId (\s a -> s {_isbdAccountId = a})

-- | Contains the type of server-side encryption used to encrypt the inventory results.
isbdEncryption :: Lens' InventoryS3BucketDestination (Maybe InventoryEncryption)
isbdEncryption = lens _isbdEncryption (\s a -> s {_isbdEncryption = a})

-- | The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
isbdBucket :: Lens' InventoryS3BucketDestination BucketName
isbdBucket = lens _isbdBucket (\s a -> s {_isbdBucket = a})

-- | Specifies the output format of the inventory results.
isbdFormat :: Lens' InventoryS3BucketDestination InventoryFormat
isbdFormat = lens _isbdFormat (\s a -> s {_isbdFormat = a})

instance FromXML InventoryS3BucketDestination where
  parseXML x =
    InventoryS3BucketDestination'
      <$> (x .@? "Prefix")
      <*> (x .@? "AccountId")
      <*> (x .@? "Encryption")
      <*> (x .@ "Bucket")
      <*> (x .@ "Format")

instance Hashable InventoryS3BucketDestination

instance NFData InventoryS3BucketDestination

instance ToXML InventoryS3BucketDestination where
  toXML InventoryS3BucketDestination' {..} =
    mconcat
      [ "Prefix" @= _isbdPrefix,
        "AccountId" @= _isbdAccountId,
        "Encryption" @= _isbdEncryption,
        "Bucket" @= _isbdBucket,
        "Format" @= _isbdFormat
      ]

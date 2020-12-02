{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.S3Location where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Encryption
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.MetadataEntry
import Network.AWS.S3.Types.ObjectCannedACL
import Network.AWS.S3.Types.StorageClass
import Network.AWS.S3.Types.Tagging

-- | Describes an Amazon S3 location that will receive the results of the restore request.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slCannedACL ::
      !(Maybe ObjectCannedACL),
    _slAccessControlList :: !(Maybe [Grant]),
    _slUserMetadata :: !(Maybe [MetadataEntry]),
    _slEncryption :: !(Maybe Encryption),
    _slStorageClass :: !(Maybe StorageClass),
    _slTagging :: !(Maybe Tagging),
    _slBucketName :: !BucketName,
    _slPrefix :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCannedACL' - The canned ACL to apply to the restore results.
--
-- * 'slAccessControlList' - A list of grants that control access to the staged results.
--
-- * 'slUserMetadata' - A list of metadata to store with the restore results in S3.
--
-- * 'slEncryption' - Undocumented member.
--
-- * 'slStorageClass' - The class of storage used to store the restore results.
--
-- * 'slTagging' - The tag-set that is applied to the restore results.
--
-- * 'slBucketName' - The name of the bucket where the restore results will be placed.
--
-- * 'slPrefix' - The prefix that is prepended to the restore results for this request.
s3Location ::
  -- | 'slBucketName'
  BucketName ->
  -- | 'slPrefix'
  Text ->
  S3Location
s3Location pBucketName_ pPrefix_ =
  S3Location'
    { _slCannedACL = Nothing,
      _slAccessControlList = Nothing,
      _slUserMetadata = Nothing,
      _slEncryption = Nothing,
      _slStorageClass = Nothing,
      _slTagging = Nothing,
      _slBucketName = pBucketName_,
      _slPrefix = pPrefix_
    }

-- | The canned ACL to apply to the restore results.
slCannedACL :: Lens' S3Location (Maybe ObjectCannedACL)
slCannedACL = lens _slCannedACL (\s a -> s {_slCannedACL = a})

-- | A list of grants that control access to the staged results.
slAccessControlList :: Lens' S3Location [Grant]
slAccessControlList = lens _slAccessControlList (\s a -> s {_slAccessControlList = a}) . _Default . _Coerce

-- | A list of metadata to store with the restore results in S3.
slUserMetadata :: Lens' S3Location [MetadataEntry]
slUserMetadata = lens _slUserMetadata (\s a -> s {_slUserMetadata = a}) . _Default . _Coerce

-- | Undocumented member.
slEncryption :: Lens' S3Location (Maybe Encryption)
slEncryption = lens _slEncryption (\s a -> s {_slEncryption = a})

-- | The class of storage used to store the restore results.
slStorageClass :: Lens' S3Location (Maybe StorageClass)
slStorageClass = lens _slStorageClass (\s a -> s {_slStorageClass = a})

-- | The tag-set that is applied to the restore results.
slTagging :: Lens' S3Location (Maybe Tagging)
slTagging = lens _slTagging (\s a -> s {_slTagging = a})

-- | The name of the bucket where the restore results will be placed.
slBucketName :: Lens' S3Location BucketName
slBucketName = lens _slBucketName (\s a -> s {_slBucketName = a})

-- | The prefix that is prepended to the restore results for this request.
slPrefix :: Lens' S3Location Text
slPrefix = lens _slPrefix (\s a -> s {_slPrefix = a})

instance Hashable S3Location

instance NFData S3Location

instance ToXML S3Location where
  toXML S3Location' {..} =
    mconcat
      [ "CannedACL" @= _slCannedACL,
        "AccessControlList"
          @= toXML (toXMLList "Grant" <$> _slAccessControlList),
        "UserMetadata"
          @= toXML (toXMLList "MetadataEntry" <$> _slUserMetadata),
        "Encryption" @= _slEncryption,
        "StorageClass" @= _slStorageClass,
        "Tagging" @= _slTagging,
        "BucketName" @= _slBucketName,
        "Prefix" @= _slPrefix
      ]

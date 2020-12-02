{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.S3Location where

import Network.AWS.Glacier.Types.CannedACL
import Network.AWS.Glacier.Types.Encryption
import Network.AWS.Glacier.Types.Grant
import Network.AWS.Glacier.Types.StorageClass
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the location in Amazon S3 where the select job results are stored.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slCannedACL :: !(Maybe CannedACL),
    _slPrefix :: !(Maybe Text),
    _slBucketName :: !(Maybe Text),
    _slAccessControlList :: !(Maybe [Grant]),
    _slUserMetadata :: !(Maybe (Map Text (Text))),
    _slEncryption :: !(Maybe Encryption),
    _slStorageClass :: !(Maybe StorageClass),
    _slTagging :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCannedACL' - The canned access control list (ACL) to apply to the job results.
--
-- * 'slPrefix' - The prefix that is prepended to the results for this request.
--
-- * 'slBucketName' - The name of the Amazon S3 bucket where the job results are stored.
--
-- * 'slAccessControlList' - A list of grants that control access to the staged results.
--
-- * 'slUserMetadata' - A map of metadata to store with the job results in Amazon S3.
--
-- * 'slEncryption' - Contains information about the encryption used to store the job results in Amazon S3.
--
-- * 'slStorageClass' - The storage class used to store the job results.
--
-- * 'slTagging' - The tag-set that is applied to the job results.
s3Location ::
  S3Location
s3Location =
  S3Location'
    { _slCannedACL = Nothing,
      _slPrefix = Nothing,
      _slBucketName = Nothing,
      _slAccessControlList = Nothing,
      _slUserMetadata = Nothing,
      _slEncryption = Nothing,
      _slStorageClass = Nothing,
      _slTagging = Nothing
    }

-- | The canned access control list (ACL) to apply to the job results.
slCannedACL :: Lens' S3Location (Maybe CannedACL)
slCannedACL = lens _slCannedACL (\s a -> s {_slCannedACL = a})

-- | The prefix that is prepended to the results for this request.
slPrefix :: Lens' S3Location (Maybe Text)
slPrefix = lens _slPrefix (\s a -> s {_slPrefix = a})

-- | The name of the Amazon S3 bucket where the job results are stored.
slBucketName :: Lens' S3Location (Maybe Text)
slBucketName = lens _slBucketName (\s a -> s {_slBucketName = a})

-- | A list of grants that control access to the staged results.
slAccessControlList :: Lens' S3Location [Grant]
slAccessControlList = lens _slAccessControlList (\s a -> s {_slAccessControlList = a}) . _Default . _Coerce

-- | A map of metadata to store with the job results in Amazon S3.
slUserMetadata :: Lens' S3Location (HashMap Text (Text))
slUserMetadata = lens _slUserMetadata (\s a -> s {_slUserMetadata = a}) . _Default . _Map

-- | Contains information about the encryption used to store the job results in Amazon S3.
slEncryption :: Lens' S3Location (Maybe Encryption)
slEncryption = lens _slEncryption (\s a -> s {_slEncryption = a})

-- | The storage class used to store the job results.
slStorageClass :: Lens' S3Location (Maybe StorageClass)
slStorageClass = lens _slStorageClass (\s a -> s {_slStorageClass = a})

-- | The tag-set that is applied to the job results.
slTagging :: Lens' S3Location (HashMap Text (Text))
slTagging = lens _slTagging (\s a -> s {_slTagging = a}) . _Default . _Map

instance FromJSON S3Location where
  parseJSON =
    withObject
      "S3Location"
      ( \x ->
          S3Location'
            <$> (x .:? "CannedACL")
            <*> (x .:? "Prefix")
            <*> (x .:? "BucketName")
            <*> (x .:? "AccessControlList" .!= mempty)
            <*> (x .:? "UserMetadata" .!= mempty)
            <*> (x .:? "Encryption")
            <*> (x .:? "StorageClass")
            <*> (x .:? "Tagging" .!= mempty)
      )

instance Hashable S3Location

instance NFData S3Location

instance ToJSON S3Location where
  toJSON S3Location' {..} =
    object
      ( catMaybes
          [ ("CannedACL" .=) <$> _slCannedACL,
            ("Prefix" .=) <$> _slPrefix,
            ("BucketName" .=) <$> _slBucketName,
            ("AccessControlList" .=) <$> _slAccessControlList,
            ("UserMetadata" .=) <$> _slUserMetadata,
            ("Encryption" .=) <$> _slEncryption,
            ("StorageClass" .=) <$> _slStorageClass,
            ("Tagging" .=) <$> _slTagging
          ]
      )

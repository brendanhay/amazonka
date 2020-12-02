{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MultipartUpload where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Initiator
import Network.AWS.S3.Types.Owner
import Network.AWS.S3.Types.StorageClass

-- | Container for the @MultipartUpload@ for the Amazon S3 object.
--
--
--
-- /See:/ 'multipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { _muInitiated ::
      !(Maybe ISO8601),
    _muInitiator :: !(Maybe Initiator),
    _muOwner :: !(Maybe Owner),
    _muKey :: !(Maybe ObjectKey),
    _muStorageClass :: !(Maybe StorageClass),
    _muUploadId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'muInitiated' - Date and time at which the multipart upload was initiated.
--
-- * 'muInitiator' - Identifies who initiated the multipart upload.
--
-- * 'muOwner' - Specifies the owner of the object that is part of the multipart upload.
--
-- * 'muKey' - Key of the object for which the multipart upload was initiated.
--
-- * 'muStorageClass' - The class of storage used to store the object.
--
-- * 'muUploadId' - Upload ID that identifies the multipart upload.
multipartUpload ::
  MultipartUpload
multipartUpload =
  MultipartUpload'
    { _muInitiated = Nothing,
      _muInitiator = Nothing,
      _muOwner = Nothing,
      _muKey = Nothing,
      _muStorageClass = Nothing,
      _muUploadId = Nothing
    }

-- | Date and time at which the multipart upload was initiated.
muInitiated :: Lens' MultipartUpload (Maybe UTCTime)
muInitiated = lens _muInitiated (\s a -> s {_muInitiated = a}) . mapping _Time

-- | Identifies who initiated the multipart upload.
muInitiator :: Lens' MultipartUpload (Maybe Initiator)
muInitiator = lens _muInitiator (\s a -> s {_muInitiator = a})

-- | Specifies the owner of the object that is part of the multipart upload.
muOwner :: Lens' MultipartUpload (Maybe Owner)
muOwner = lens _muOwner (\s a -> s {_muOwner = a})

-- | Key of the object for which the multipart upload was initiated.
muKey :: Lens' MultipartUpload (Maybe ObjectKey)
muKey = lens _muKey (\s a -> s {_muKey = a})

-- | The class of storage used to store the object.
muStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
muStorageClass = lens _muStorageClass (\s a -> s {_muStorageClass = a})

-- | Upload ID that identifies the multipart upload.
muUploadId :: Lens' MultipartUpload (Maybe Text)
muUploadId = lens _muUploadId (\s a -> s {_muUploadId = a})

instance FromXML MultipartUpload where
  parseXML x =
    MultipartUpload'
      <$> (x .@? "Initiated")
      <*> (x .@? "Initiator")
      <*> (x .@? "Owner")
      <*> (x .@? "Key")
      <*> (x .@? "StorageClass")
      <*> (x .@? "UploadId")

instance Hashable MultipartUpload

instance NFData MultipartUpload

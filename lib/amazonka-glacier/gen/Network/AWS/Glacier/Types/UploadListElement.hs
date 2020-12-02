{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.UploadListElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.UploadListElement where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of in-progress multipart uploads for a vault.
--
--
--
-- /See:/ 'uploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { _uleMultipartUploadId ::
      !(Maybe Text),
    _ulePartSizeInBytes :: !(Maybe Integer),
    _uleArchiveDescription :: !(Maybe Text),
    _uleVaultARN :: !(Maybe Text),
    _uleCreationDate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UploadListElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uleMultipartUploadId' - The ID of a multipart upload.
--
-- * 'ulePartSizeInBytes' - The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
--
-- * 'uleArchiveDescription' - The description of the archive that was specified in the Initiate Multipart Upload request.
--
-- * 'uleVaultARN' - The Amazon Resource Name (ARN) of the vault that contains the archive.
--
-- * 'uleCreationDate' - The UTC time at which the multipart upload was initiated.
uploadListElement ::
  UploadListElement
uploadListElement =
  UploadListElement'
    { _uleMultipartUploadId = Nothing,
      _ulePartSizeInBytes = Nothing,
      _uleArchiveDescription = Nothing,
      _uleVaultARN = Nothing,
      _uleCreationDate = Nothing
    }

-- | The ID of a multipart upload.
uleMultipartUploadId :: Lens' UploadListElement (Maybe Text)
uleMultipartUploadId = lens _uleMultipartUploadId (\s a -> s {_uleMultipartUploadId = a})

-- | The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
ulePartSizeInBytes :: Lens' UploadListElement (Maybe Integer)
ulePartSizeInBytes = lens _ulePartSizeInBytes (\s a -> s {_ulePartSizeInBytes = a})

-- | The description of the archive that was specified in the Initiate Multipart Upload request.
uleArchiveDescription :: Lens' UploadListElement (Maybe Text)
uleArchiveDescription = lens _uleArchiveDescription (\s a -> s {_uleArchiveDescription = a})

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
uleVaultARN :: Lens' UploadListElement (Maybe Text)
uleVaultARN = lens _uleVaultARN (\s a -> s {_uleVaultARN = a})

-- | The UTC time at which the multipart upload was initiated.
uleCreationDate :: Lens' UploadListElement (Maybe Text)
uleCreationDate = lens _uleCreationDate (\s a -> s {_uleCreationDate = a})

instance FromJSON UploadListElement where
  parseJSON =
    withObject
      "UploadListElement"
      ( \x ->
          UploadListElement'
            <$> (x .:? "MultipartUploadId")
            <*> (x .:? "PartSizeInBytes")
            <*> (x .:? "ArchiveDescription")
            <*> (x .:? "VaultARN")
            <*> (x .:? "CreationDate")
      )

instance Hashable UploadListElement

instance NFData UploadListElement

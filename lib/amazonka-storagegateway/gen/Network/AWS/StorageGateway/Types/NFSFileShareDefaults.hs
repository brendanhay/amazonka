{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NFSFileShareDefaults where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes Network File System (NFS) file share default values. Files and folders stored as Amazon S3 objects in S3 buckets don't, by default, have Unix file permissions assigned to them. Upon discovery in an S3 bucket by Storage Gateway, the S3 objects that represent files and folders are assigned these default Unix permissions. This operation is only supported for file gateways.
--
--
--
-- /See:/ 'nFSFileShareDefaults' smart constructor.
data NFSFileShareDefaults = NFSFileShareDefaults'
  { _nfsfsdFileMode ::
      !(Maybe Text),
    _nfsfsdOwnerId :: !(Maybe Nat),
    _nfsfsdDirectoryMode :: !(Maybe Text),
    _nfsfsdGroupId :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NFSFileShareDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfsfsdFileMode' - The Unix file mode in the form "nnnn". For example, @0666@ represents the default file mode inside the file share. The default value is @0666@ .
--
-- * 'nfsfsdOwnerId' - The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is @nfsnobody@ .
--
-- * 'nfsfsdDirectoryMode' - The Unix directory mode in the form "nnnn". For example, @0666@ represents the default access mode for all directories inside the file share. The default value is @0777@ .
--
-- * 'nfsfsdGroupId' - The default group ID for the file share (unless the files have another group ID specified). The default value is @nfsnobody@ .
nFSFileShareDefaults ::
  NFSFileShareDefaults
nFSFileShareDefaults =
  NFSFileShareDefaults'
    { _nfsfsdFileMode = Nothing,
      _nfsfsdOwnerId = Nothing,
      _nfsfsdDirectoryMode = Nothing,
      _nfsfsdGroupId = Nothing
    }

-- | The Unix file mode in the form "nnnn". For example, @0666@ represents the default file mode inside the file share. The default value is @0666@ .
nfsfsdFileMode :: Lens' NFSFileShareDefaults (Maybe Text)
nfsfsdFileMode = lens _nfsfsdFileMode (\s a -> s {_nfsfsdFileMode = a})

-- | The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is @nfsnobody@ .
nfsfsdOwnerId :: Lens' NFSFileShareDefaults (Maybe Natural)
nfsfsdOwnerId = lens _nfsfsdOwnerId (\s a -> s {_nfsfsdOwnerId = a}) . mapping _Nat

-- | The Unix directory mode in the form "nnnn". For example, @0666@ represents the default access mode for all directories inside the file share. The default value is @0777@ .
nfsfsdDirectoryMode :: Lens' NFSFileShareDefaults (Maybe Text)
nfsfsdDirectoryMode = lens _nfsfsdDirectoryMode (\s a -> s {_nfsfsdDirectoryMode = a})

-- | The default group ID for the file share (unless the files have another group ID specified). The default value is @nfsnobody@ .
nfsfsdGroupId :: Lens' NFSFileShareDefaults (Maybe Natural)
nfsfsdGroupId = lens _nfsfsdGroupId (\s a -> s {_nfsfsdGroupId = a}) . mapping _Nat

instance FromJSON NFSFileShareDefaults where
  parseJSON =
    withObject
      "NFSFileShareDefaults"
      ( \x ->
          NFSFileShareDefaults'
            <$> (x .:? "FileMode")
            <*> (x .:? "OwnerId")
            <*> (x .:? "DirectoryMode")
            <*> (x .:? "GroupId")
      )

instance Hashable NFSFileShareDefaults

instance NFData NFSFileShareDefaults

instance ToJSON NFSFileShareDefaults where
  toJSON NFSFileShareDefaults' {..} =
    object
      ( catMaybes
          [ ("FileMode" .=) <$> _nfsfsdFileMode,
            ("OwnerId" .=) <$> _nfsfsdOwnerId,
            ("DirectoryMode" .=) <$> _nfsfsdDirectoryMode,
            ("GroupId" .=) <$> _nfsfsdGroupId
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SSHPublicKeyMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SSHPublicKeyMetadata where

import Network.AWS.IAM.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an SSH public key, without the key's body or fingerprint.
--
--
-- This data type is used as a response element in the 'ListSSHPublicKeys' operation.
--
--
-- /See:/ 'sshPublicKeyMetadata' smart constructor.
data SSHPublicKeyMetadata = SSHPublicKeyMetadata'
  { _spkmUserName ::
      !Text,
    _spkmSSHPublicKeyId :: !Text,
    _spkmStatus :: !StatusType,
    _spkmUploadDate :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSHPublicKeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spkmUserName' - The name of the IAM user associated with the SSH public key.
--
-- * 'spkmSSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- * 'spkmStatus' - The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- * 'spkmUploadDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
sshPublicKeyMetadata ::
  -- | 'spkmUserName'
  Text ->
  -- | 'spkmSSHPublicKeyId'
  Text ->
  -- | 'spkmStatus'
  StatusType ->
  -- | 'spkmUploadDate'
  UTCTime ->
  SSHPublicKeyMetadata
sshPublicKeyMetadata
  pUserName_
  pSSHPublicKeyId_
  pStatus_
  pUploadDate_ =
    SSHPublicKeyMetadata'
      { _spkmUserName = pUserName_,
        _spkmSSHPublicKeyId = pSSHPublicKeyId_,
        _spkmStatus = pStatus_,
        _spkmUploadDate = _Time # pUploadDate_
      }

-- | The name of the IAM user associated with the SSH public key.
spkmUserName :: Lens' SSHPublicKeyMetadata Text
spkmUserName = lens _spkmUserName (\s a -> s {_spkmUserName = a})

-- | The unique identifier for the SSH public key.
spkmSSHPublicKeyId :: Lens' SSHPublicKeyMetadata Text
spkmSSHPublicKeyId = lens _spkmSSHPublicKeyId (\s a -> s {_spkmSSHPublicKeyId = a})

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
spkmStatus :: Lens' SSHPublicKeyMetadata StatusType
spkmStatus = lens _spkmStatus (\s a -> s {_spkmStatus = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
spkmUploadDate :: Lens' SSHPublicKeyMetadata UTCTime
spkmUploadDate = lens _spkmUploadDate (\s a -> s {_spkmUploadDate = a}) . _Time

instance FromXML SSHPublicKeyMetadata where
  parseXML x =
    SSHPublicKeyMetadata'
      <$> (x .@ "UserName")
      <*> (x .@ "SSHPublicKeyId")
      <*> (x .@ "Status")
      <*> (x .@ "UploadDate")

instance Hashable SSHPublicKeyMetadata

instance NFData SSHPublicKeyMetadata

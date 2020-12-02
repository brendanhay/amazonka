{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SSHPublicKey where

import Network.AWS.IAM.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an SSH public key.
--
--
-- This data type is used as a response element in the 'GetSSHPublicKey' and 'UploadSSHPublicKey' operations.
--
--
-- /See:/ 'sshPublicKey' smart constructor.
data SSHPublicKey = SSHPublicKey'
  { _spkUploadDate ::
      !(Maybe ISO8601),
    _spkUserName :: !Text,
    _spkSSHPublicKeyId :: !Text,
    _spkFingerprint :: !Text,
    _spkSSHPublicKeyBody :: !Text,
    _spkStatus :: !StatusType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spkUploadDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
--
-- * 'spkUserName' - The name of the IAM user associated with the SSH public key.
--
-- * 'spkSSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- * 'spkFingerprint' - The MD5 message digest of the SSH public key.
--
-- * 'spkSSHPublicKeyBody' - The SSH public key.
--
-- * 'spkStatus' - The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
sshPublicKey ::
  -- | 'spkUserName'
  Text ->
  -- | 'spkSSHPublicKeyId'
  Text ->
  -- | 'spkFingerprint'
  Text ->
  -- | 'spkSSHPublicKeyBody'
  Text ->
  -- | 'spkStatus'
  StatusType ->
  SSHPublicKey
sshPublicKey
  pUserName_
  pSSHPublicKeyId_
  pFingerprint_
  pSSHPublicKeyBody_
  pStatus_ =
    SSHPublicKey'
      { _spkUploadDate = Nothing,
        _spkUserName = pUserName_,
        _spkSSHPublicKeyId = pSSHPublicKeyId_,
        _spkFingerprint = pFingerprint_,
        _spkSSHPublicKeyBody = pSSHPublicKeyBody_,
        _spkStatus = pStatus_
      }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
spkUploadDate :: Lens' SSHPublicKey (Maybe UTCTime)
spkUploadDate = lens _spkUploadDate (\s a -> s {_spkUploadDate = a}) . mapping _Time

-- | The name of the IAM user associated with the SSH public key.
spkUserName :: Lens' SSHPublicKey Text
spkUserName = lens _spkUserName (\s a -> s {_spkUserName = a})

-- | The unique identifier for the SSH public key.
spkSSHPublicKeyId :: Lens' SSHPublicKey Text
spkSSHPublicKeyId = lens _spkSSHPublicKeyId (\s a -> s {_spkSSHPublicKeyId = a})

-- | The MD5 message digest of the SSH public key.
spkFingerprint :: Lens' SSHPublicKey Text
spkFingerprint = lens _spkFingerprint (\s a -> s {_spkFingerprint = a})

-- | The SSH public key.
spkSSHPublicKeyBody :: Lens' SSHPublicKey Text
spkSSHPublicKeyBody = lens _spkSSHPublicKeyBody (\s a -> s {_spkSSHPublicKeyBody = a})

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
spkStatus :: Lens' SSHPublicKey StatusType
spkStatus = lens _spkStatus (\s a -> s {_spkStatus = a})

instance FromXML SSHPublicKey where
  parseXML x =
    SSHPublicKey'
      <$> (x .@? "UploadDate")
      <*> (x .@ "UserName")
      <*> (x .@ "SSHPublicKeyId")
      <*> (x .@ "Fingerprint")
      <*> (x .@ "SSHPublicKeyBody")
      <*> (x .@ "Status")

instance Hashable SSHPublicKey

instance NFData SSHPublicKey

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SSHPublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SSHPublicKey where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens

-- | Contains information about an SSH public key.
--
-- This data type is used as a response element in the GetSSHPublicKey and
-- UploadSSHPublicKey operations.
--
-- /See:/ 'newSSHPublicKey' smart constructor.
data SSHPublicKey = SSHPublicKey'
  { -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
    -- public key was uploaded.
    uploadDate :: Core.Maybe Core.ISO8601,
    -- | The name of the IAM user associated with the SSH public key.
    userName :: Core.Text,
    -- | The unique identifier for the SSH public key.
    sSHPublicKeyId :: Core.Text,
    -- | The MD5 message digest of the SSH public key.
    fingerprint :: Core.Text,
    -- | The SSH public key.
    sSHPublicKeyBody :: Core.Text,
    -- | The status of the SSH public key. @Active@ means that the key can be
    -- used for authentication with an AWS CodeCommit repository. @Inactive@
    -- means that the key cannot be used.
    status :: StatusType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SSHPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadDate', 'sSHPublicKey_uploadDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
-- public key was uploaded.
--
-- 'userName', 'sSHPublicKey_userName' - The name of the IAM user associated with the SSH public key.
--
-- 'sSHPublicKeyId', 'sSHPublicKey_sSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- 'fingerprint', 'sSHPublicKey_fingerprint' - The MD5 message digest of the SSH public key.
--
-- 'sSHPublicKeyBody', 'sSHPublicKey_sSHPublicKeyBody' - The SSH public key.
--
-- 'status', 'sSHPublicKey_status' - The status of the SSH public key. @Active@ means that the key can be
-- used for authentication with an AWS CodeCommit repository. @Inactive@
-- means that the key cannot be used.
newSSHPublicKey ::
  -- | 'userName'
  Core.Text ->
  -- | 'sSHPublicKeyId'
  Core.Text ->
  -- | 'fingerprint'
  Core.Text ->
  -- | 'sSHPublicKeyBody'
  Core.Text ->
  -- | 'status'
  StatusType ->
  SSHPublicKey
newSSHPublicKey
  pUserName_
  pSSHPublicKeyId_
  pFingerprint_
  pSSHPublicKeyBody_
  pStatus_ =
    SSHPublicKey'
      { uploadDate = Core.Nothing,
        userName = pUserName_,
        sSHPublicKeyId = pSSHPublicKeyId_,
        fingerprint = pFingerprint_,
        sSHPublicKeyBody = pSSHPublicKeyBody_,
        status = pStatus_
      }

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
-- public key was uploaded.
sSHPublicKey_uploadDate :: Lens.Lens' SSHPublicKey (Core.Maybe Core.UTCTime)
sSHPublicKey_uploadDate = Lens.lens (\SSHPublicKey' {uploadDate} -> uploadDate) (\s@SSHPublicKey' {} a -> s {uploadDate = a} :: SSHPublicKey) Core.. Lens.mapping Core._Time

-- | The name of the IAM user associated with the SSH public key.
sSHPublicKey_userName :: Lens.Lens' SSHPublicKey Core.Text
sSHPublicKey_userName = Lens.lens (\SSHPublicKey' {userName} -> userName) (\s@SSHPublicKey' {} a -> s {userName = a} :: SSHPublicKey)

-- | The unique identifier for the SSH public key.
sSHPublicKey_sSHPublicKeyId :: Lens.Lens' SSHPublicKey Core.Text
sSHPublicKey_sSHPublicKeyId = Lens.lens (\SSHPublicKey' {sSHPublicKeyId} -> sSHPublicKeyId) (\s@SSHPublicKey' {} a -> s {sSHPublicKeyId = a} :: SSHPublicKey)

-- | The MD5 message digest of the SSH public key.
sSHPublicKey_fingerprint :: Lens.Lens' SSHPublicKey Core.Text
sSHPublicKey_fingerprint = Lens.lens (\SSHPublicKey' {fingerprint} -> fingerprint) (\s@SSHPublicKey' {} a -> s {fingerprint = a} :: SSHPublicKey)

-- | The SSH public key.
sSHPublicKey_sSHPublicKeyBody :: Lens.Lens' SSHPublicKey Core.Text
sSHPublicKey_sSHPublicKeyBody = Lens.lens (\SSHPublicKey' {sSHPublicKeyBody} -> sSHPublicKeyBody) (\s@SSHPublicKey' {} a -> s {sSHPublicKeyBody = a} :: SSHPublicKey)

-- | The status of the SSH public key. @Active@ means that the key can be
-- used for authentication with an AWS CodeCommit repository. @Inactive@
-- means that the key cannot be used.
sSHPublicKey_status :: Lens.Lens' SSHPublicKey StatusType
sSHPublicKey_status = Lens.lens (\SSHPublicKey' {status} -> status) (\s@SSHPublicKey' {} a -> s {status = a} :: SSHPublicKey)

instance Core.FromXML SSHPublicKey where
  parseXML x =
    SSHPublicKey'
      Core.<$> (x Core..@? "UploadDate")
      Core.<*> (x Core..@ "UserName")
      Core.<*> (x Core..@ "SSHPublicKeyId")
      Core.<*> (x Core..@ "Fingerprint")
      Core.<*> (x Core..@ "SSHPublicKeyBody")
      Core.<*> (x Core..@ "Status")

instance Core.Hashable SSHPublicKey

instance Core.NFData SSHPublicKey

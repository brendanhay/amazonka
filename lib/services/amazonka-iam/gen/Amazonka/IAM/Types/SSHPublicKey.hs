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
-- Module      : Amazonka.IAM.Types.SSHPublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.SSHPublicKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.StatusType
import qualified Amazonka.Prelude as Prelude

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
    uploadDate :: Prelude.Maybe Data.ISO8601,
    -- | The name of the IAM user associated with the SSH public key.
    userName :: Prelude.Text,
    -- | The unique identifier for the SSH public key.
    sSHPublicKeyId :: Prelude.Text,
    -- | The MD5 message digest of the SSH public key.
    fingerprint :: Prelude.Text,
    -- | The SSH public key.
    sSHPublicKeyBody :: Prelude.Text,
    -- | The status of the SSH public key. @Active@ means that the key can be
    -- used for authentication with an CodeCommit repository. @Inactive@ means
    -- that the key cannot be used.
    status :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- used for authentication with an CodeCommit repository. @Inactive@ means
-- that the key cannot be used.
newSSHPublicKey ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'sSHPublicKeyId'
  Prelude.Text ->
  -- | 'fingerprint'
  Prelude.Text ->
  -- | 'sSHPublicKeyBody'
  Prelude.Text ->
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
      { uploadDate = Prelude.Nothing,
        userName = pUserName_,
        sSHPublicKeyId = pSSHPublicKeyId_,
        fingerprint = pFingerprint_,
        sSHPublicKeyBody = pSSHPublicKeyBody_,
        status = pStatus_
      }

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
-- public key was uploaded.
sSHPublicKey_uploadDate :: Lens.Lens' SSHPublicKey (Prelude.Maybe Prelude.UTCTime)
sSHPublicKey_uploadDate = Lens.lens (\SSHPublicKey' {uploadDate} -> uploadDate) (\s@SSHPublicKey' {} a -> s {uploadDate = a} :: SSHPublicKey) Prelude.. Lens.mapping Data._Time

-- | The name of the IAM user associated with the SSH public key.
sSHPublicKey_userName :: Lens.Lens' SSHPublicKey Prelude.Text
sSHPublicKey_userName = Lens.lens (\SSHPublicKey' {userName} -> userName) (\s@SSHPublicKey' {} a -> s {userName = a} :: SSHPublicKey)

-- | The unique identifier for the SSH public key.
sSHPublicKey_sSHPublicKeyId :: Lens.Lens' SSHPublicKey Prelude.Text
sSHPublicKey_sSHPublicKeyId = Lens.lens (\SSHPublicKey' {sSHPublicKeyId} -> sSHPublicKeyId) (\s@SSHPublicKey' {} a -> s {sSHPublicKeyId = a} :: SSHPublicKey)

-- | The MD5 message digest of the SSH public key.
sSHPublicKey_fingerprint :: Lens.Lens' SSHPublicKey Prelude.Text
sSHPublicKey_fingerprint = Lens.lens (\SSHPublicKey' {fingerprint} -> fingerprint) (\s@SSHPublicKey' {} a -> s {fingerprint = a} :: SSHPublicKey)

-- | The SSH public key.
sSHPublicKey_sSHPublicKeyBody :: Lens.Lens' SSHPublicKey Prelude.Text
sSHPublicKey_sSHPublicKeyBody = Lens.lens (\SSHPublicKey' {sSHPublicKeyBody} -> sSHPublicKeyBody) (\s@SSHPublicKey' {} a -> s {sSHPublicKeyBody = a} :: SSHPublicKey)

-- | The status of the SSH public key. @Active@ means that the key can be
-- used for authentication with an CodeCommit repository. @Inactive@ means
-- that the key cannot be used.
sSHPublicKey_status :: Lens.Lens' SSHPublicKey StatusType
sSHPublicKey_status = Lens.lens (\SSHPublicKey' {status} -> status) (\s@SSHPublicKey' {} a -> s {status = a} :: SSHPublicKey)

instance Data.FromXML SSHPublicKey where
  parseXML x =
    SSHPublicKey'
      Prelude.<$> (x Data..@? "UploadDate")
      Prelude.<*> (x Data..@ "UserName")
      Prelude.<*> (x Data..@ "SSHPublicKeyId")
      Prelude.<*> (x Data..@ "Fingerprint")
      Prelude.<*> (x Data..@ "SSHPublicKeyBody")
      Prelude.<*> (x Data..@ "Status")

instance Prelude.Hashable SSHPublicKey where
  hashWithSalt _salt SSHPublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` uploadDate
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` sSHPublicKeyId
      `Prelude.hashWithSalt` fingerprint
      `Prelude.hashWithSalt` sSHPublicKeyBody
      `Prelude.hashWithSalt` status

instance Prelude.NFData SSHPublicKey where
  rnf SSHPublicKey' {..} =
    Prelude.rnf uploadDate
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf sSHPublicKeyId
      `Prelude.seq` Prelude.rnf fingerprint
      `Prelude.seq` Prelude.rnf sSHPublicKeyBody
      `Prelude.seq` Prelude.rnf status

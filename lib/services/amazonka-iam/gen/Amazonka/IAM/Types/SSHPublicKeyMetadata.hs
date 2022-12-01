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
-- Module      : Amazonka.IAM.Types.SSHPublicKeyMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.SSHPublicKeyMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types.StatusType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an SSH public key, without the key\'s body or
-- fingerprint.
--
-- This data type is used as a response element in the ListSSHPublicKeys
-- operation.
--
-- /See:/ 'newSSHPublicKeyMetadata' smart constructor.
data SSHPublicKeyMetadata = SSHPublicKeyMetadata'
  { -- | The name of the IAM user associated with the SSH public key.
    userName :: Prelude.Text,
    -- | The unique identifier for the SSH public key.
    sSHPublicKeyId :: Prelude.Text,
    -- | The status of the SSH public key. @Active@ means that the key can be
    -- used for authentication with an CodeCommit repository. @Inactive@ means
    -- that the key cannot be used.
    status :: StatusType,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
    -- public key was uploaded.
    uploadDate :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSHPublicKeyMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'sSHPublicKeyMetadata_userName' - The name of the IAM user associated with the SSH public key.
--
-- 'sSHPublicKeyId', 'sSHPublicKeyMetadata_sSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- 'status', 'sSHPublicKeyMetadata_status' - The status of the SSH public key. @Active@ means that the key can be
-- used for authentication with an CodeCommit repository. @Inactive@ means
-- that the key cannot be used.
--
-- 'uploadDate', 'sSHPublicKeyMetadata_uploadDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
-- public key was uploaded.
newSSHPublicKeyMetadata ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'sSHPublicKeyId'
  Prelude.Text ->
  -- | 'status'
  StatusType ->
  -- | 'uploadDate'
  Prelude.UTCTime ->
  SSHPublicKeyMetadata
newSSHPublicKeyMetadata
  pUserName_
  pSSHPublicKeyId_
  pStatus_
  pUploadDate_ =
    SSHPublicKeyMetadata'
      { userName = pUserName_,
        sSHPublicKeyId = pSSHPublicKeyId_,
        status = pStatus_,
        uploadDate = Core._Time Lens.# pUploadDate_
      }

-- | The name of the IAM user associated with the SSH public key.
sSHPublicKeyMetadata_userName :: Lens.Lens' SSHPublicKeyMetadata Prelude.Text
sSHPublicKeyMetadata_userName = Lens.lens (\SSHPublicKeyMetadata' {userName} -> userName) (\s@SSHPublicKeyMetadata' {} a -> s {userName = a} :: SSHPublicKeyMetadata)

-- | The unique identifier for the SSH public key.
sSHPublicKeyMetadata_sSHPublicKeyId :: Lens.Lens' SSHPublicKeyMetadata Prelude.Text
sSHPublicKeyMetadata_sSHPublicKeyId = Lens.lens (\SSHPublicKeyMetadata' {sSHPublicKeyId} -> sSHPublicKeyId) (\s@SSHPublicKeyMetadata' {} a -> s {sSHPublicKeyId = a} :: SSHPublicKeyMetadata)

-- | The status of the SSH public key. @Active@ means that the key can be
-- used for authentication with an CodeCommit repository. @Inactive@ means
-- that the key cannot be used.
sSHPublicKeyMetadata_status :: Lens.Lens' SSHPublicKeyMetadata StatusType
sSHPublicKeyMetadata_status = Lens.lens (\SSHPublicKeyMetadata' {status} -> status) (\s@SSHPublicKeyMetadata' {} a -> s {status = a} :: SSHPublicKeyMetadata)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
-- public key was uploaded.
sSHPublicKeyMetadata_uploadDate :: Lens.Lens' SSHPublicKeyMetadata Prelude.UTCTime
sSHPublicKeyMetadata_uploadDate = Lens.lens (\SSHPublicKeyMetadata' {uploadDate} -> uploadDate) (\s@SSHPublicKeyMetadata' {} a -> s {uploadDate = a} :: SSHPublicKeyMetadata) Prelude.. Core._Time

instance Core.FromXML SSHPublicKeyMetadata where
  parseXML x =
    SSHPublicKeyMetadata'
      Prelude.<$> (x Core..@ "UserName")
      Prelude.<*> (x Core..@ "SSHPublicKeyId")
      Prelude.<*> (x Core..@ "Status")
      Prelude.<*> (x Core..@ "UploadDate")

instance Prelude.Hashable SSHPublicKeyMetadata where
  hashWithSalt _salt SSHPublicKeyMetadata' {..} =
    _salt `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` sSHPublicKeyId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` uploadDate

instance Prelude.NFData SSHPublicKeyMetadata where
  rnf SSHPublicKeyMetadata' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf sSHPublicKeyId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf uploadDate

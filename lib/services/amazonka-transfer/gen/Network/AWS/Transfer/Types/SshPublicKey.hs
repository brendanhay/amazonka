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
-- Module      : Network.AWS.Transfer.Types.SshPublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transfer.Types.SshPublicKey where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the public Secure Shell (SSH) key that is
-- associated with a user account for the specific file transfer
-- protocol-enabled server (as identified by @ServerId@). The information
-- returned includes the date the key was imported, the public key
-- contents, and the public key ID. A user can store more than one SSH
-- public key associated with their user name on a specific server.
--
-- /See:/ 'newSshPublicKey' smart constructor.
data SshPublicKey = SshPublicKey'
  { -- | Specifies the date that the public key was added to the user account.
    dateImported :: Core.POSIX,
    -- | Specifies the content of the SSH public key as specified by the
    -- @PublicKeyId@.
    sshPublicKeyBody :: Prelude.Text,
    -- | Specifies the @SshPublicKeyId@ parameter contains the identifier of the
    -- public key.
    sshPublicKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SshPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateImported', 'sshPublicKey_dateImported' - Specifies the date that the public key was added to the user account.
--
-- 'sshPublicKeyBody', 'sshPublicKey_sshPublicKeyBody' - Specifies the content of the SSH public key as specified by the
-- @PublicKeyId@.
--
-- 'sshPublicKeyId', 'sshPublicKey_sshPublicKeyId' - Specifies the @SshPublicKeyId@ parameter contains the identifier of the
-- public key.
newSshPublicKey ::
  -- | 'dateImported'
  Prelude.UTCTime ->
  -- | 'sshPublicKeyBody'
  Prelude.Text ->
  -- | 'sshPublicKeyId'
  Prelude.Text ->
  SshPublicKey
newSshPublicKey
  pDateImported_
  pSshPublicKeyBody_
  pSshPublicKeyId_ =
    SshPublicKey'
      { dateImported =
          Core._Time Lens.# pDateImported_,
        sshPublicKeyBody = pSshPublicKeyBody_,
        sshPublicKeyId = pSshPublicKeyId_
      }

-- | Specifies the date that the public key was added to the user account.
sshPublicKey_dateImported :: Lens.Lens' SshPublicKey Prelude.UTCTime
sshPublicKey_dateImported = Lens.lens (\SshPublicKey' {dateImported} -> dateImported) (\s@SshPublicKey' {} a -> s {dateImported = a} :: SshPublicKey) Prelude.. Core._Time

-- | Specifies the content of the SSH public key as specified by the
-- @PublicKeyId@.
sshPublicKey_sshPublicKeyBody :: Lens.Lens' SshPublicKey Prelude.Text
sshPublicKey_sshPublicKeyBody = Lens.lens (\SshPublicKey' {sshPublicKeyBody} -> sshPublicKeyBody) (\s@SshPublicKey' {} a -> s {sshPublicKeyBody = a} :: SshPublicKey)

-- | Specifies the @SshPublicKeyId@ parameter contains the identifier of the
-- public key.
sshPublicKey_sshPublicKeyId :: Lens.Lens' SshPublicKey Prelude.Text
sshPublicKey_sshPublicKeyId = Lens.lens (\SshPublicKey' {sshPublicKeyId} -> sshPublicKeyId) (\s@SshPublicKey' {} a -> s {sshPublicKeyId = a} :: SshPublicKey)

instance Core.FromJSON SshPublicKey where
  parseJSON =
    Core.withObject
      "SshPublicKey"
      ( \x ->
          SshPublicKey'
            Prelude.<$> (x Core..: "DateImported")
            Prelude.<*> (x Core..: "SshPublicKeyBody")
            Prelude.<*> (x Core..: "SshPublicKeyId")
      )

instance Prelude.Hashable SshPublicKey

instance Prelude.NFData SshPublicKey

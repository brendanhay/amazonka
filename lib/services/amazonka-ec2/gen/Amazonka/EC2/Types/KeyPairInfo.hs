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
-- Module      : Amazonka.EC2.Types.KeyPairInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.KeyPairInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.KeyType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a key pair.
--
-- /See:/ 'newKeyPairInfo' smart constructor.
data KeyPairInfo = KeyPairInfo'
  { -- | If you used Amazon EC2 to create the key pair, this is the date and time
    -- when the key was created, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 date-time format>,
    -- in the UTC time zone.
    --
    -- If you imported an existing key pair to Amazon EC2, this is the date and
    -- time the key was imported, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 date-time format>,
    -- in the UTC time zone.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | If you used CreateKeyPair to create the key pair:
    --
    -- -   For RSA key pairs, the key fingerprint is the SHA-1 digest of the
    --     DER encoded private key.
    --
    -- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
    --     SHA-256 digest, which is the default for OpenSSH, starting with
    --     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
    --
    -- If you used ImportKeyPair to provide Amazon Web Services the public key:
    --
    -- -   For RSA key pairs, the key fingerprint is the MD5 public key
    --     fingerprint as specified in section 4 of RFC4716.
    --
    -- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
    --     SHA-256 digest, which is the default for OpenSSH, starting with
    --     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
    keyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the key pair.
    keyPairId :: Prelude.Maybe Prelude.Text,
    -- | The type of key pair.
    keyType :: Prelude.Maybe KeyType,
    -- | The public key material.
    publicKey :: Prelude.Maybe Prelude.Text,
    -- | Any tags applied to the key pair.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyPairInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'keyPairInfo_createTime' - If you used Amazon EC2 to create the key pair, this is the date and time
-- when the key was created, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 date-time format>,
-- in the UTC time zone.
--
-- If you imported an existing key pair to Amazon EC2, this is the date and
-- time the key was imported, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 date-time format>,
-- in the UTC time zone.
--
-- 'keyFingerprint', 'keyPairInfo_keyFingerprint' - If you used CreateKeyPair to create the key pair:
--
-- -   For RSA key pairs, the key fingerprint is the SHA-1 digest of the
--     DER encoded private key.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
--
-- If you used ImportKeyPair to provide Amazon Web Services the public key:
--
-- -   For RSA key pairs, the key fingerprint is the MD5 public key
--     fingerprint as specified in section 4 of RFC4716.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
--
-- 'keyName', 'keyPairInfo_keyName' - The name of the key pair.
--
-- 'keyPairId', 'keyPairInfo_keyPairId' - The ID of the key pair.
--
-- 'keyType', 'keyPairInfo_keyType' - The type of key pair.
--
-- 'publicKey', 'keyPairInfo_publicKey' - The public key material.
--
-- 'tags', 'keyPairInfo_tags' - Any tags applied to the key pair.
newKeyPairInfo ::
  KeyPairInfo
newKeyPairInfo =
  KeyPairInfo'
    { createTime = Prelude.Nothing,
      keyFingerprint = Prelude.Nothing,
      keyName = Prelude.Nothing,
      keyPairId = Prelude.Nothing,
      keyType = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | If you used Amazon EC2 to create the key pair, this is the date and time
-- when the key was created, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 date-time format>,
-- in the UTC time zone.
--
-- If you imported an existing key pair to Amazon EC2, this is the date and
-- time the key was imported, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 date-time format>,
-- in the UTC time zone.
keyPairInfo_createTime :: Lens.Lens' KeyPairInfo (Prelude.Maybe Prelude.UTCTime)
keyPairInfo_createTime = Lens.lens (\KeyPairInfo' {createTime} -> createTime) (\s@KeyPairInfo' {} a -> s {createTime = a} :: KeyPairInfo) Prelude.. Lens.mapping Data._Time

-- | If you used CreateKeyPair to create the key pair:
--
-- -   For RSA key pairs, the key fingerprint is the SHA-1 digest of the
--     DER encoded private key.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
--
-- If you used ImportKeyPair to provide Amazon Web Services the public key:
--
-- -   For RSA key pairs, the key fingerprint is the MD5 public key
--     fingerprint as specified in section 4 of RFC4716.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
keyPairInfo_keyFingerprint :: Lens.Lens' KeyPairInfo (Prelude.Maybe Prelude.Text)
keyPairInfo_keyFingerprint = Lens.lens (\KeyPairInfo' {keyFingerprint} -> keyFingerprint) (\s@KeyPairInfo' {} a -> s {keyFingerprint = a} :: KeyPairInfo)

-- | The name of the key pair.
keyPairInfo_keyName :: Lens.Lens' KeyPairInfo (Prelude.Maybe Prelude.Text)
keyPairInfo_keyName = Lens.lens (\KeyPairInfo' {keyName} -> keyName) (\s@KeyPairInfo' {} a -> s {keyName = a} :: KeyPairInfo)

-- | The ID of the key pair.
keyPairInfo_keyPairId :: Lens.Lens' KeyPairInfo (Prelude.Maybe Prelude.Text)
keyPairInfo_keyPairId = Lens.lens (\KeyPairInfo' {keyPairId} -> keyPairId) (\s@KeyPairInfo' {} a -> s {keyPairId = a} :: KeyPairInfo)

-- | The type of key pair.
keyPairInfo_keyType :: Lens.Lens' KeyPairInfo (Prelude.Maybe KeyType)
keyPairInfo_keyType = Lens.lens (\KeyPairInfo' {keyType} -> keyType) (\s@KeyPairInfo' {} a -> s {keyType = a} :: KeyPairInfo)

-- | The public key material.
keyPairInfo_publicKey :: Lens.Lens' KeyPairInfo (Prelude.Maybe Prelude.Text)
keyPairInfo_publicKey = Lens.lens (\KeyPairInfo' {publicKey} -> publicKey) (\s@KeyPairInfo' {} a -> s {publicKey = a} :: KeyPairInfo)

-- | Any tags applied to the key pair.
keyPairInfo_tags :: Lens.Lens' KeyPairInfo (Prelude.Maybe [Tag])
keyPairInfo_tags = Lens.lens (\KeyPairInfo' {tags} -> tags) (\s@KeyPairInfo' {} a -> s {tags = a} :: KeyPairInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML KeyPairInfo where
  parseXML x =
    KeyPairInfo'
      Prelude.<$> (x Data..@? "createTime")
      Prelude.<*> (x Data..@? "keyFingerprint")
      Prelude.<*> (x Data..@? "keyName")
      Prelude.<*> (x Data..@? "keyPairId")
      Prelude.<*> (x Data..@? "keyType")
      Prelude.<*> (x Data..@? "publicKey")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable KeyPairInfo where
  hashWithSalt _salt KeyPairInfo' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` keyFingerprint
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` keyPairId
      `Prelude.hashWithSalt` keyType
      `Prelude.hashWithSalt` publicKey
      `Prelude.hashWithSalt` tags

instance Prelude.NFData KeyPairInfo where
  rnf KeyPairInfo' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf keyFingerprint
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf keyPairId
      `Prelude.seq` Prelude.rnf keyType
      `Prelude.seq` Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf tags

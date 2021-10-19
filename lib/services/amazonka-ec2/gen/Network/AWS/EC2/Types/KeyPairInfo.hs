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
-- Module      : Network.AWS.EC2.Types.KeyPairInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.KeyPairInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.KeyType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a key pair.
--
-- /See:/ 'newKeyPairInfo' smart constructor.
data KeyPairInfo = KeyPairInfo'
  { -- | If you used CreateKeyPair to create the key pair:
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
    -- | The type of key pair.
    keyType :: Prelude.Maybe KeyType,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the key pair.
    keyPairId :: Prelude.Maybe Prelude.Text,
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
-- 'keyType', 'keyPairInfo_keyType' - The type of key pair.
--
-- 'keyName', 'keyPairInfo_keyName' - The name of the key pair.
--
-- 'keyPairId', 'keyPairInfo_keyPairId' - The ID of the key pair.
--
-- 'tags', 'keyPairInfo_tags' - Any tags applied to the key pair.
newKeyPairInfo ::
  KeyPairInfo
newKeyPairInfo =
  KeyPairInfo'
    { keyFingerprint = Prelude.Nothing,
      keyType = Prelude.Nothing,
      keyName = Prelude.Nothing,
      keyPairId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

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

-- | The type of key pair.
keyPairInfo_keyType :: Lens.Lens' KeyPairInfo (Prelude.Maybe KeyType)
keyPairInfo_keyType = Lens.lens (\KeyPairInfo' {keyType} -> keyType) (\s@KeyPairInfo' {} a -> s {keyType = a} :: KeyPairInfo)

-- | The name of the key pair.
keyPairInfo_keyName :: Lens.Lens' KeyPairInfo (Prelude.Maybe Prelude.Text)
keyPairInfo_keyName = Lens.lens (\KeyPairInfo' {keyName} -> keyName) (\s@KeyPairInfo' {} a -> s {keyName = a} :: KeyPairInfo)

-- | The ID of the key pair.
keyPairInfo_keyPairId :: Lens.Lens' KeyPairInfo (Prelude.Maybe Prelude.Text)
keyPairInfo_keyPairId = Lens.lens (\KeyPairInfo' {keyPairId} -> keyPairId) (\s@KeyPairInfo' {} a -> s {keyPairId = a} :: KeyPairInfo)

-- | Any tags applied to the key pair.
keyPairInfo_tags :: Lens.Lens' KeyPairInfo (Prelude.Maybe [Tag])
keyPairInfo_tags = Lens.lens (\KeyPairInfo' {tags} -> tags) (\s@KeyPairInfo' {} a -> s {tags = a} :: KeyPairInfo) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML KeyPairInfo where
  parseXML x =
    KeyPairInfo'
      Prelude.<$> (x Core..@? "keyFingerprint")
      Prelude.<*> (x Core..@? "keyType")
      Prelude.<*> (x Core..@? "keyName")
      Prelude.<*> (x Core..@? "keyPairId")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable KeyPairInfo

instance Prelude.NFData KeyPairInfo

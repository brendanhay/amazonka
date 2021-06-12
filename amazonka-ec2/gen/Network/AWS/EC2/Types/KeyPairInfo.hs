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
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a key pair.
--
-- /See:/ 'newKeyPairInfo' smart constructor.
data KeyPairInfo = KeyPairInfo'
  { -- | If you used CreateKeyPair to create the key pair, this is the SHA-1
    -- digest of the DER encoded private key. If you used ImportKeyPair to
    -- provide AWS the public key, this is the MD5 public key fingerprint as
    -- specified in section 4 of RFC4716.
    keyFingerprint :: Core.Maybe Core.Text,
    -- | The ID of the key pair.
    keyPairId :: Core.Maybe Core.Text,
    -- | Any tags applied to the key pair.
    tags :: Core.Maybe [Tag],
    -- | The name of the key pair.
    keyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KeyPairInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyFingerprint', 'keyPairInfo_keyFingerprint' - If you used CreateKeyPair to create the key pair, this is the SHA-1
-- digest of the DER encoded private key. If you used ImportKeyPair to
-- provide AWS the public key, this is the MD5 public key fingerprint as
-- specified in section 4 of RFC4716.
--
-- 'keyPairId', 'keyPairInfo_keyPairId' - The ID of the key pair.
--
-- 'tags', 'keyPairInfo_tags' - Any tags applied to the key pair.
--
-- 'keyName', 'keyPairInfo_keyName' - The name of the key pair.
newKeyPairInfo ::
  KeyPairInfo
newKeyPairInfo =
  KeyPairInfo'
    { keyFingerprint = Core.Nothing,
      keyPairId = Core.Nothing,
      tags = Core.Nothing,
      keyName = Core.Nothing
    }

-- | If you used CreateKeyPair to create the key pair, this is the SHA-1
-- digest of the DER encoded private key. If you used ImportKeyPair to
-- provide AWS the public key, this is the MD5 public key fingerprint as
-- specified in section 4 of RFC4716.
keyPairInfo_keyFingerprint :: Lens.Lens' KeyPairInfo (Core.Maybe Core.Text)
keyPairInfo_keyFingerprint = Lens.lens (\KeyPairInfo' {keyFingerprint} -> keyFingerprint) (\s@KeyPairInfo' {} a -> s {keyFingerprint = a} :: KeyPairInfo)

-- | The ID of the key pair.
keyPairInfo_keyPairId :: Lens.Lens' KeyPairInfo (Core.Maybe Core.Text)
keyPairInfo_keyPairId = Lens.lens (\KeyPairInfo' {keyPairId} -> keyPairId) (\s@KeyPairInfo' {} a -> s {keyPairId = a} :: KeyPairInfo)

-- | Any tags applied to the key pair.
keyPairInfo_tags :: Lens.Lens' KeyPairInfo (Core.Maybe [Tag])
keyPairInfo_tags = Lens.lens (\KeyPairInfo' {tags} -> tags) (\s@KeyPairInfo' {} a -> s {tags = a} :: KeyPairInfo) Core.. Lens.mapping Lens._Coerce

-- | The name of the key pair.
keyPairInfo_keyName :: Lens.Lens' KeyPairInfo (Core.Maybe Core.Text)
keyPairInfo_keyName = Lens.lens (\KeyPairInfo' {keyName} -> keyName) (\s@KeyPairInfo' {} a -> s {keyName = a} :: KeyPairInfo)

instance Core.FromXML KeyPairInfo where
  parseXML x =
    KeyPairInfo'
      Core.<$> (x Core..@? "keyFingerprint")
      Core.<*> (x Core..@? "keyPairId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "keyName")

instance Core.Hashable KeyPairInfo

instance Core.NFData KeyPairInfo

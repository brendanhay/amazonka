{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.EncryptionEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EncryptionEntity where

import Network.AWS.CloudFront.Types.FieldPatterns
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Complex data type for field-level encryption profiles that includes the
-- encryption key and field pattern specifications.
--
-- /See:/ 'newEncryptionEntity' smart constructor.
data EncryptionEntity = EncryptionEntity'
  { -- | The public key associated with a set of field-level encryption patterns,
    -- to be used when encrypting the fields that match the patterns.
    publicKeyId :: Prelude.Text,
    -- | The provider associated with the public key being used for encryption.
    -- This value must also be provided with the private key for applications
    -- to be able to decrypt data.
    providerId :: Prelude.Text,
    -- | Field patterns in a field-level encryption content type profile specify
    -- the fields that you want to be encrypted. You can provide the full field
    -- name, or any beginning characters followed by a wildcard (*). You can\'t
    -- overlap field patterns. For example, you can\'t have both ABC* and AB*.
    -- Note that field patterns are case-sensitive.
    fieldPatterns :: FieldPatterns
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EncryptionEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKeyId', 'encryptionEntity_publicKeyId' - The public key associated with a set of field-level encryption patterns,
-- to be used when encrypting the fields that match the patterns.
--
-- 'providerId', 'encryptionEntity_providerId' - The provider associated with the public key being used for encryption.
-- This value must also be provided with the private key for applications
-- to be able to decrypt data.
--
-- 'fieldPatterns', 'encryptionEntity_fieldPatterns' - Field patterns in a field-level encryption content type profile specify
-- the fields that you want to be encrypted. You can provide the full field
-- name, or any beginning characters followed by a wildcard (*). You can\'t
-- overlap field patterns. For example, you can\'t have both ABC* and AB*.
-- Note that field patterns are case-sensitive.
newEncryptionEntity ::
  -- | 'publicKeyId'
  Prelude.Text ->
  -- | 'providerId'
  Prelude.Text ->
  -- | 'fieldPatterns'
  FieldPatterns ->
  EncryptionEntity
newEncryptionEntity
  pPublicKeyId_
  pProviderId_
  pFieldPatterns_ =
    EncryptionEntity'
      { publicKeyId = pPublicKeyId_,
        providerId = pProviderId_,
        fieldPatterns = pFieldPatterns_
      }

-- | The public key associated with a set of field-level encryption patterns,
-- to be used when encrypting the fields that match the patterns.
encryptionEntity_publicKeyId :: Lens.Lens' EncryptionEntity Prelude.Text
encryptionEntity_publicKeyId = Lens.lens (\EncryptionEntity' {publicKeyId} -> publicKeyId) (\s@EncryptionEntity' {} a -> s {publicKeyId = a} :: EncryptionEntity)

-- | The provider associated with the public key being used for encryption.
-- This value must also be provided with the private key for applications
-- to be able to decrypt data.
encryptionEntity_providerId :: Lens.Lens' EncryptionEntity Prelude.Text
encryptionEntity_providerId = Lens.lens (\EncryptionEntity' {providerId} -> providerId) (\s@EncryptionEntity' {} a -> s {providerId = a} :: EncryptionEntity)

-- | Field patterns in a field-level encryption content type profile specify
-- the fields that you want to be encrypted. You can provide the full field
-- name, or any beginning characters followed by a wildcard (*). You can\'t
-- overlap field patterns. For example, you can\'t have both ABC* and AB*.
-- Note that field patterns are case-sensitive.
encryptionEntity_fieldPatterns :: Lens.Lens' EncryptionEntity FieldPatterns
encryptionEntity_fieldPatterns = Lens.lens (\EncryptionEntity' {fieldPatterns} -> fieldPatterns) (\s@EncryptionEntity' {} a -> s {fieldPatterns = a} :: EncryptionEntity)

instance Prelude.FromXML EncryptionEntity where
  parseXML x =
    EncryptionEntity'
      Prelude.<$> (x Prelude..@ "PublicKeyId")
      Prelude.<*> (x Prelude..@ "ProviderId")
      Prelude.<*> (x Prelude..@ "FieldPatterns")

instance Prelude.Hashable EncryptionEntity

instance Prelude.NFData EncryptionEntity

instance Prelude.ToXML EncryptionEntity where
  toXML EncryptionEntity' {..} =
    Prelude.mconcat
      [ "PublicKeyId" Prelude.@= publicKeyId,
        "ProviderId" Prelude.@= providerId,
        "FieldPatterns" Prelude.@= fieldPatterns
      ]

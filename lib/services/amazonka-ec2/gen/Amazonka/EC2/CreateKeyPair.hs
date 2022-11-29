{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.CreateKeyPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an ED25519 or 2048-bit RSA key pair with the specified name and
-- in the specified PEM or PPK format. Amazon EC2 stores the public key and
-- displays the private key for you to save to a file. The private key is
-- returned as an unencrypted PEM encoded PKCS#1 private key or an
-- unencrypted PPK formatted private key for use with PuTTY. If a key with
-- the specified name already exists, Amazon EC2 returns an error.
--
-- The key pair returned to you is available only in the Amazon Web
-- Services Region in which you create it. If you prefer, you can create
-- your own key pair using a third-party tool and upload it to any Region
-- using ImportKeyPair.
--
-- You can have up to 5,000 key pairs per Amazon Web Services Region.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 key pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateKeyPair
  ( -- * Creating a Request
    CreateKeyPair (..),
    newCreateKeyPair,

    -- * Request Lenses
    createKeyPair_keyType,
    createKeyPair_dryRun,
    createKeyPair_keyFormat,
    createKeyPair_tagSpecifications,
    createKeyPair_keyName,

    -- * Destructuring the Response
    CreateKeyPairResponse (..),
    newCreateKeyPairResponse,

    -- * Response Lenses
    createKeyPairResponse_tags,
    createKeyPairResponse_keyPairId,
    createKeyPairResponse_httpStatus,
    createKeyPairResponse_keyName,
    createKeyPairResponse_keyFingerprint,
    createKeyPairResponse_keyMaterial,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { -- | The type of key pair. Note that ED25519 keys are not supported for
    -- Windows instances.
    --
    -- Default: @rsa@
    keyType :: Prelude.Maybe KeyType,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The format of the key pair.
    --
    -- Default: @pem@
    keyFormat :: Prelude.Maybe KeyFormat,
    -- | The tags to apply to the new key pair.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | A unique name for the key pair.
    --
    -- Constraints: Up to 255 ASCII characters
    keyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyType', 'createKeyPair_keyType' - The type of key pair. Note that ED25519 keys are not supported for
-- Windows instances.
--
-- Default: @rsa@
--
-- 'dryRun', 'createKeyPair_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'keyFormat', 'createKeyPair_keyFormat' - The format of the key pair.
--
-- Default: @pem@
--
-- 'tagSpecifications', 'createKeyPair_tagSpecifications' - The tags to apply to the new key pair.
--
-- 'keyName', 'createKeyPair_keyName' - A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
newCreateKeyPair ::
  -- | 'keyName'
  Prelude.Text ->
  CreateKeyPair
newCreateKeyPair pKeyName_ =
  CreateKeyPair'
    { keyType = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      keyFormat = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      keyName = pKeyName_
    }

-- | The type of key pair. Note that ED25519 keys are not supported for
-- Windows instances.
--
-- Default: @rsa@
createKeyPair_keyType :: Lens.Lens' CreateKeyPair (Prelude.Maybe KeyType)
createKeyPair_keyType = Lens.lens (\CreateKeyPair' {keyType} -> keyType) (\s@CreateKeyPair' {} a -> s {keyType = a} :: CreateKeyPair)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createKeyPair_dryRun :: Lens.Lens' CreateKeyPair (Prelude.Maybe Prelude.Bool)
createKeyPair_dryRun = Lens.lens (\CreateKeyPair' {dryRun} -> dryRun) (\s@CreateKeyPair' {} a -> s {dryRun = a} :: CreateKeyPair)

-- | The format of the key pair.
--
-- Default: @pem@
createKeyPair_keyFormat :: Lens.Lens' CreateKeyPair (Prelude.Maybe KeyFormat)
createKeyPair_keyFormat = Lens.lens (\CreateKeyPair' {keyFormat} -> keyFormat) (\s@CreateKeyPair' {} a -> s {keyFormat = a} :: CreateKeyPair)

-- | The tags to apply to the new key pair.
createKeyPair_tagSpecifications :: Lens.Lens' CreateKeyPair (Prelude.Maybe [TagSpecification])
createKeyPair_tagSpecifications = Lens.lens (\CreateKeyPair' {tagSpecifications} -> tagSpecifications) (\s@CreateKeyPair' {} a -> s {tagSpecifications = a} :: CreateKeyPair) Prelude.. Lens.mapping Lens.coerced

-- | A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
createKeyPair_keyName :: Lens.Lens' CreateKeyPair Prelude.Text
createKeyPair_keyName = Lens.lens (\CreateKeyPair' {keyName} -> keyName) (\s@CreateKeyPair' {} a -> s {keyName = a} :: CreateKeyPair)

instance Core.AWSRequest CreateKeyPair where
  type
    AWSResponse CreateKeyPair =
      CreateKeyPairResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateKeyPairResponse'
            Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "keyPairId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "keyName")
            Prelude.<*> (x Core..@ "keyFingerprint")
            Prelude.<*> (x Core..@ "keyMaterial")
      )

instance Prelude.Hashable CreateKeyPair where
  hashWithSalt _salt CreateKeyPair' {..} =
    _salt `Prelude.hashWithSalt` keyType
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` keyFormat
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` keyName

instance Prelude.NFData CreateKeyPair where
  rnf CreateKeyPair' {..} =
    Prelude.rnf keyType
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf keyFormat
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf keyName

instance Core.ToHeaders CreateKeyPair where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateKeyPair where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateKeyPair where
  toQuery CreateKeyPair' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateKeyPair" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "KeyType" Core.=: keyType,
        "DryRun" Core.=: dryRun,
        "KeyFormat" Core.=: keyFormat,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "KeyName" Core.=: keyName
      ]

-- | Describes a key pair.
--
-- /See:/ 'newCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { -- | Any tags applied to the key pair.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the key pair.
    keyPairId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the key pair.
    keyName :: Prelude.Text,
    -- | -   For RSA key pairs, the key fingerprint is the SHA-1 digest of the
    --     DER encoded private key.
    --
    -- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
    --     SHA-256 digest, which is the default for OpenSSH, starting with
    --     OpenSSH 6.8.
    keyFingerprint :: Prelude.Text,
    -- | An unencrypted PEM encoded RSA or ED25519 private key.
    keyMaterial :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createKeyPairResponse_tags' - Any tags applied to the key pair.
--
-- 'keyPairId', 'createKeyPairResponse_keyPairId' - The ID of the key pair.
--
-- 'httpStatus', 'createKeyPairResponse_httpStatus' - The response's http status code.
--
-- 'keyName', 'createKeyPairResponse_keyName' - The name of the key pair.
--
-- 'keyFingerprint', 'createKeyPairResponse_keyFingerprint' - -   For RSA key pairs, the key fingerprint is the SHA-1 digest of the
--     DER encoded private key.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     OpenSSH 6.8.
--
-- 'keyMaterial', 'createKeyPairResponse_keyMaterial' - An unencrypted PEM encoded RSA or ED25519 private key.
newCreateKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'keyFingerprint'
  Prelude.Text ->
  -- | 'keyMaterial'
  Prelude.Text ->
  CreateKeyPairResponse
newCreateKeyPairResponse
  pHttpStatus_
  pKeyName_
  pKeyFingerprint_
  pKeyMaterial_ =
    CreateKeyPairResponse'
      { tags = Prelude.Nothing,
        keyPairId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        keyName = pKeyName_,
        keyFingerprint = pKeyFingerprint_,
        keyMaterial = Core._Sensitive Lens.# pKeyMaterial_
      }

-- | Any tags applied to the key pair.
createKeyPairResponse_tags :: Lens.Lens' CreateKeyPairResponse (Prelude.Maybe [Tag])
createKeyPairResponse_tags = Lens.lens (\CreateKeyPairResponse' {tags} -> tags) (\s@CreateKeyPairResponse' {} a -> s {tags = a} :: CreateKeyPairResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the key pair.
createKeyPairResponse_keyPairId :: Lens.Lens' CreateKeyPairResponse (Prelude.Maybe Prelude.Text)
createKeyPairResponse_keyPairId = Lens.lens (\CreateKeyPairResponse' {keyPairId} -> keyPairId) (\s@CreateKeyPairResponse' {} a -> s {keyPairId = a} :: CreateKeyPairResponse)

-- | The response's http status code.
createKeyPairResponse_httpStatus :: Lens.Lens' CreateKeyPairResponse Prelude.Int
createKeyPairResponse_httpStatus = Lens.lens (\CreateKeyPairResponse' {httpStatus} -> httpStatus) (\s@CreateKeyPairResponse' {} a -> s {httpStatus = a} :: CreateKeyPairResponse)

-- | The name of the key pair.
createKeyPairResponse_keyName :: Lens.Lens' CreateKeyPairResponse Prelude.Text
createKeyPairResponse_keyName = Lens.lens (\CreateKeyPairResponse' {keyName} -> keyName) (\s@CreateKeyPairResponse' {} a -> s {keyName = a} :: CreateKeyPairResponse)

-- | -   For RSA key pairs, the key fingerprint is the SHA-1 digest of the
--     DER encoded private key.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     OpenSSH 6.8.
createKeyPairResponse_keyFingerprint :: Lens.Lens' CreateKeyPairResponse Prelude.Text
createKeyPairResponse_keyFingerprint = Lens.lens (\CreateKeyPairResponse' {keyFingerprint} -> keyFingerprint) (\s@CreateKeyPairResponse' {} a -> s {keyFingerprint = a} :: CreateKeyPairResponse)

-- | An unencrypted PEM encoded RSA or ED25519 private key.
createKeyPairResponse_keyMaterial :: Lens.Lens' CreateKeyPairResponse Prelude.Text
createKeyPairResponse_keyMaterial = Lens.lens (\CreateKeyPairResponse' {keyMaterial} -> keyMaterial) (\s@CreateKeyPairResponse' {} a -> s {keyMaterial = a} :: CreateKeyPairResponse) Prelude.. Core._Sensitive

instance Prelude.NFData CreateKeyPairResponse where
  rnf CreateKeyPairResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf keyPairId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf keyFingerprint
      `Prelude.seq` Prelude.rnf keyMaterial

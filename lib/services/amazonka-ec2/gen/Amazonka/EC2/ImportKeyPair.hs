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
-- Module      : Amazonka.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the public key from an RSA or ED25519 key pair that you created
-- with a third-party tool. Compare this with CreateKeyPair, in which
-- Amazon Web Services creates the key pair and gives the keys to you
-- (Amazon Web Services keeps a copy of the public key). With
-- ImportKeyPair, you create the key pair and give Amazon Web Services just
-- the public key. The private key is never transferred between you and
-- Amazon Web Services.
--
-- For more information about key pairs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 key pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.ImportKeyPair
  ( -- * Creating a Request
    ImportKeyPair (..),
    newImportKeyPair,

    -- * Request Lenses
    importKeyPair_dryRun,
    importKeyPair_tagSpecifications,
    importKeyPair_keyName,
    importKeyPair_publicKeyMaterial,

    -- * Destructuring the Response
    ImportKeyPairResponse (..),
    newImportKeyPairResponse,

    -- * Response Lenses
    importKeyPairResponse_keyFingerprint,
    importKeyPairResponse_keyName,
    importKeyPairResponse_keyPairId,
    importKeyPairResponse_tags,
    importKeyPairResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the imported key pair.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | A unique name for the key pair.
    keyName :: Prelude.Text,
    -- | The public key. For API calls, the text must be base64-encoded. For
    -- command line tools, base64 encoding is performed for you.
    publicKeyMaterial :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'importKeyPair_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'importKeyPair_tagSpecifications' - The tags to apply to the imported key pair.
--
-- 'keyName', 'importKeyPair_keyName' - A unique name for the key pair.
--
-- 'publicKeyMaterial', 'importKeyPair_publicKeyMaterial' - The public key. For API calls, the text must be base64-encoded. For
-- command line tools, base64 encoding is performed for you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newImportKeyPair ::
  -- | 'keyName'
  Prelude.Text ->
  -- | 'publicKeyMaterial'
  Prelude.ByteString ->
  ImportKeyPair
newImportKeyPair pKeyName_ pPublicKeyMaterial_ =
  ImportKeyPair'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      keyName = pKeyName_,
      publicKeyMaterial =
        Data._Base64 Lens.# pPublicKeyMaterial_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importKeyPair_dryRun :: Lens.Lens' ImportKeyPair (Prelude.Maybe Prelude.Bool)
importKeyPair_dryRun = Lens.lens (\ImportKeyPair' {dryRun} -> dryRun) (\s@ImportKeyPair' {} a -> s {dryRun = a} :: ImportKeyPair)

-- | The tags to apply to the imported key pair.
importKeyPair_tagSpecifications :: Lens.Lens' ImportKeyPair (Prelude.Maybe [TagSpecification])
importKeyPair_tagSpecifications = Lens.lens (\ImportKeyPair' {tagSpecifications} -> tagSpecifications) (\s@ImportKeyPair' {} a -> s {tagSpecifications = a} :: ImportKeyPair) Prelude.. Lens.mapping Lens.coerced

-- | A unique name for the key pair.
importKeyPair_keyName :: Lens.Lens' ImportKeyPair Prelude.Text
importKeyPair_keyName = Lens.lens (\ImportKeyPair' {keyName} -> keyName) (\s@ImportKeyPair' {} a -> s {keyName = a} :: ImportKeyPair)

-- | The public key. For API calls, the text must be base64-encoded. For
-- command line tools, base64 encoding is performed for you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importKeyPair_publicKeyMaterial :: Lens.Lens' ImportKeyPair Prelude.ByteString
importKeyPair_publicKeyMaterial = Lens.lens (\ImportKeyPair' {publicKeyMaterial} -> publicKeyMaterial) (\s@ImportKeyPair' {} a -> s {publicKeyMaterial = a} :: ImportKeyPair) Prelude.. Data._Base64

instance Core.AWSRequest ImportKeyPair where
  type
    AWSResponse ImportKeyPair =
      ImportKeyPairResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ImportKeyPairResponse'
            Prelude.<$> (x Data..@? "keyFingerprint")
            Prelude.<*> (x Data..@? "keyName")
            Prelude.<*> (x Data..@? "keyPairId")
            Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportKeyPair where
  hashWithSalt _salt ImportKeyPair' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` publicKeyMaterial

instance Prelude.NFData ImportKeyPair where
  rnf ImportKeyPair' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf publicKeyMaterial

instance Data.ToHeaders ImportKeyPair where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ImportKeyPair where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportKeyPair where
  toQuery ImportKeyPair' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ImportKeyPair" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "KeyName" Data.=: keyName,
        "PublicKeyMaterial" Data.=: publicKeyMaterial
      ]

-- | /See:/ 'newImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { -- | -   For RSA key pairs, the key fingerprint is the MD5 public key
    --     fingerprint as specified in section 4 of RFC 4716.
    --
    -- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
    --     SHA-256 digest, which is the default for OpenSSH, starting with
    --     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
    keyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The key pair name that you provided.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resulting key pair.
    keyPairId :: Prelude.Maybe Prelude.Text,
    -- | The tags applied to the imported key pair.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyFingerprint', 'importKeyPairResponse_keyFingerprint' - -   For RSA key pairs, the key fingerprint is the MD5 public key
--     fingerprint as specified in section 4 of RFC 4716.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
--
-- 'keyName', 'importKeyPairResponse_keyName' - The key pair name that you provided.
--
-- 'keyPairId', 'importKeyPairResponse_keyPairId' - The ID of the resulting key pair.
--
-- 'tags', 'importKeyPairResponse_tags' - The tags applied to the imported key pair.
--
-- 'httpStatus', 'importKeyPairResponse_httpStatus' - The response's http status code.
newImportKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportKeyPairResponse
newImportKeyPairResponse pHttpStatus_ =
  ImportKeyPairResponse'
    { keyFingerprint =
        Prelude.Nothing,
      keyName = Prelude.Nothing,
      keyPairId = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | -   For RSA key pairs, the key fingerprint is the MD5 public key
--     fingerprint as specified in section 4 of RFC 4716.
--
-- -   For ED25519 key pairs, the key fingerprint is the base64-encoded
--     SHA-256 digest, which is the default for OpenSSH, starting with
--     <http://www.openssh.com/txt/release-6.8 OpenSSH 6.8>.
importKeyPairResponse_keyFingerprint :: Lens.Lens' ImportKeyPairResponse (Prelude.Maybe Prelude.Text)
importKeyPairResponse_keyFingerprint = Lens.lens (\ImportKeyPairResponse' {keyFingerprint} -> keyFingerprint) (\s@ImportKeyPairResponse' {} a -> s {keyFingerprint = a} :: ImportKeyPairResponse)

-- | The key pair name that you provided.
importKeyPairResponse_keyName :: Lens.Lens' ImportKeyPairResponse (Prelude.Maybe Prelude.Text)
importKeyPairResponse_keyName = Lens.lens (\ImportKeyPairResponse' {keyName} -> keyName) (\s@ImportKeyPairResponse' {} a -> s {keyName = a} :: ImportKeyPairResponse)

-- | The ID of the resulting key pair.
importKeyPairResponse_keyPairId :: Lens.Lens' ImportKeyPairResponse (Prelude.Maybe Prelude.Text)
importKeyPairResponse_keyPairId = Lens.lens (\ImportKeyPairResponse' {keyPairId} -> keyPairId) (\s@ImportKeyPairResponse' {} a -> s {keyPairId = a} :: ImportKeyPairResponse)

-- | The tags applied to the imported key pair.
importKeyPairResponse_tags :: Lens.Lens' ImportKeyPairResponse (Prelude.Maybe [Tag])
importKeyPairResponse_tags = Lens.lens (\ImportKeyPairResponse' {tags} -> tags) (\s@ImportKeyPairResponse' {} a -> s {tags = a} :: ImportKeyPairResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
importKeyPairResponse_httpStatus :: Lens.Lens' ImportKeyPairResponse Prelude.Int
importKeyPairResponse_httpStatus = Lens.lens (\ImportKeyPairResponse' {httpStatus} -> httpStatus) (\s@ImportKeyPairResponse' {} a -> s {httpStatus = a} :: ImportKeyPairResponse)

instance Prelude.NFData ImportKeyPairResponse where
  rnf ImportKeyPairResponse' {..} =
    Prelude.rnf keyFingerprint
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf keyPairId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

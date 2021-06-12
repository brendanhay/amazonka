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
-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the public key from an RSA key pair that you created with a
-- third-party tool. Compare this with CreateKeyPair, in which AWS creates
-- the key pair and gives the keys to you (AWS keeps a copy of the public
-- key). With ImportKeyPair, you create the key pair and give AWS just the
-- public key. The private key is never transferred between you and AWS.
--
-- For more information about key pairs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.ImportKeyPair
  ( -- * Creating a Request
    ImportKeyPair (..),
    newImportKeyPair,

    -- * Request Lenses
    importKeyPair_tagSpecifications,
    importKeyPair_dryRun,
    importKeyPair_keyName,
    importKeyPair_publicKeyMaterial,

    -- * Destructuring the Response
    ImportKeyPairResponse (..),
    newImportKeyPairResponse,

    -- * Response Lenses
    importKeyPairResponse_keyFingerprint,
    importKeyPairResponse_keyPairId,
    importKeyPairResponse_tags,
    importKeyPairResponse_keyName,
    importKeyPairResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { -- | The tags to apply to the imported key pair.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | A unique name for the key pair.
    keyName :: Core.Text,
    -- | The public key. For API calls, the text must be base64-encoded. For
    -- command line tools, base64 encoding is performed for you.
    publicKeyMaterial :: Core.Base64
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'importKeyPair_tagSpecifications' - The tags to apply to the imported key pair.
--
-- 'dryRun', 'importKeyPair_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
  Core.Text ->
  -- | 'publicKeyMaterial'
  Core.ByteString ->
  ImportKeyPair
newImportKeyPair pKeyName_ pPublicKeyMaterial_ =
  ImportKeyPair'
    { tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      keyName = pKeyName_,
      publicKeyMaterial =
        Core._Base64 Lens.# pPublicKeyMaterial_
    }

-- | The tags to apply to the imported key pair.
importKeyPair_tagSpecifications :: Lens.Lens' ImportKeyPair (Core.Maybe [TagSpecification])
importKeyPair_tagSpecifications = Lens.lens (\ImportKeyPair' {tagSpecifications} -> tagSpecifications) (\s@ImportKeyPair' {} a -> s {tagSpecifications = a} :: ImportKeyPair) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importKeyPair_dryRun :: Lens.Lens' ImportKeyPair (Core.Maybe Core.Bool)
importKeyPair_dryRun = Lens.lens (\ImportKeyPair' {dryRun} -> dryRun) (\s@ImportKeyPair' {} a -> s {dryRun = a} :: ImportKeyPair)

-- | A unique name for the key pair.
importKeyPair_keyName :: Lens.Lens' ImportKeyPair Core.Text
importKeyPair_keyName = Lens.lens (\ImportKeyPair' {keyName} -> keyName) (\s@ImportKeyPair' {} a -> s {keyName = a} :: ImportKeyPair)

-- | The public key. For API calls, the text must be base64-encoded. For
-- command line tools, base64 encoding is performed for you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importKeyPair_publicKeyMaterial :: Lens.Lens' ImportKeyPair Core.ByteString
importKeyPair_publicKeyMaterial = Lens.lens (\ImportKeyPair' {publicKeyMaterial} -> publicKeyMaterial) (\s@ImportKeyPair' {} a -> s {publicKeyMaterial = a} :: ImportKeyPair) Core.. Core._Base64

instance Core.AWSRequest ImportKeyPair where
  type
    AWSResponse ImportKeyPair =
      ImportKeyPairResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ImportKeyPairResponse'
            Core.<$> (x Core..@? "keyFingerprint")
            Core.<*> (x Core..@? "keyPairId")
            Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "keyName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ImportKeyPair

instance Core.NFData ImportKeyPair

instance Core.ToHeaders ImportKeyPair where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ImportKeyPair where
  toPath = Core.const "/"

instance Core.ToQuery ImportKeyPair where
  toQuery ImportKeyPair' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ImportKeyPair" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "KeyName" Core.=: keyName,
        "PublicKeyMaterial" Core.=: publicKeyMaterial
      ]

-- | /See:/ 'newImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { -- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
    keyFingerprint :: Core.Maybe Core.Text,
    -- | The ID of the resulting key pair.
    keyPairId :: Core.Maybe Core.Text,
    -- | The tags applied to the imported key pair.
    tags :: Core.Maybe [Tag],
    -- | The key pair name you provided.
    keyName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyFingerprint', 'importKeyPairResponse_keyFingerprint' - The MD5 public key fingerprint as specified in section 4 of RFC 4716.
--
-- 'keyPairId', 'importKeyPairResponse_keyPairId' - The ID of the resulting key pair.
--
-- 'tags', 'importKeyPairResponse_tags' - The tags applied to the imported key pair.
--
-- 'keyName', 'importKeyPairResponse_keyName' - The key pair name you provided.
--
-- 'httpStatus', 'importKeyPairResponse_httpStatus' - The response's http status code.
newImportKeyPairResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ImportKeyPairResponse
newImportKeyPairResponse pHttpStatus_ =
  ImportKeyPairResponse'
    { keyFingerprint =
        Core.Nothing,
      keyPairId = Core.Nothing,
      tags = Core.Nothing,
      keyName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
importKeyPairResponse_keyFingerprint :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Core.Text)
importKeyPairResponse_keyFingerprint = Lens.lens (\ImportKeyPairResponse' {keyFingerprint} -> keyFingerprint) (\s@ImportKeyPairResponse' {} a -> s {keyFingerprint = a} :: ImportKeyPairResponse)

-- | The ID of the resulting key pair.
importKeyPairResponse_keyPairId :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Core.Text)
importKeyPairResponse_keyPairId = Lens.lens (\ImportKeyPairResponse' {keyPairId} -> keyPairId) (\s@ImportKeyPairResponse' {} a -> s {keyPairId = a} :: ImportKeyPairResponse)

-- | The tags applied to the imported key pair.
importKeyPairResponse_tags :: Lens.Lens' ImportKeyPairResponse (Core.Maybe [Tag])
importKeyPairResponse_tags = Lens.lens (\ImportKeyPairResponse' {tags} -> tags) (\s@ImportKeyPairResponse' {} a -> s {tags = a} :: ImportKeyPairResponse) Core.. Lens.mapping Lens._Coerce

-- | The key pair name you provided.
importKeyPairResponse_keyName :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Core.Text)
importKeyPairResponse_keyName = Lens.lens (\ImportKeyPairResponse' {keyName} -> keyName) (\s@ImportKeyPairResponse' {} a -> s {keyName = a} :: ImportKeyPairResponse)

-- | The response's http status code.
importKeyPairResponse_httpStatus :: Lens.Lens' ImportKeyPairResponse Core.Int
importKeyPairResponse_httpStatus = Lens.lens (\ImportKeyPairResponse' {httpStatus} -> httpStatus) (\s@ImportKeyPairResponse' {} a -> s {httpStatus = a} :: ImportKeyPairResponse)

instance Core.NFData ImportKeyPairResponse

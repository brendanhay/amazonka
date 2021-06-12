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
-- Module      : Network.AWS.EC2.CreateKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 2048-bit RSA key pair with the specified name. Amazon EC2
-- stores the public key and displays the private key for you to save to a
-- file. The private key is returned as an unencrypted PEM encoded PKCS#1
-- private key. If a key with the specified name already exists, Amazon EC2
-- returns an error.
--
-- You can have up to five thousand key pairs per Region.
--
-- The key pair returned to you is available only in the Region in which
-- you create it. If you prefer, you can create your own key pair using a
-- third-party tool and upload it to any Region using ImportKeyPair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateKeyPair
  ( -- * Creating a Request
    CreateKeyPair (..),
    newCreateKeyPair,

    -- * Request Lenses
    createKeyPair_tagSpecifications,
    createKeyPair_dryRun,
    createKeyPair_keyName,

    -- * Destructuring the Response
    CreateKeyPairResponse (..),
    newCreateKeyPairResponse,

    -- * Response Lenses
    createKeyPairResponse_keyPairId,
    createKeyPairResponse_tags,
    createKeyPairResponse_httpStatus,
    createKeyPairResponse_keyName,
    createKeyPairResponse_keyFingerprint,
    createKeyPairResponse_keyMaterial,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { -- | The tags to apply to the new key pair.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | A unique name for the key pair.
    --
    -- Constraints: Up to 255 ASCII characters
    keyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createKeyPair_tagSpecifications' - The tags to apply to the new key pair.
--
-- 'dryRun', 'createKeyPair_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'keyName', 'createKeyPair_keyName' - A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
newCreateKeyPair ::
  -- | 'keyName'
  Core.Text ->
  CreateKeyPair
newCreateKeyPair pKeyName_ =
  CreateKeyPair'
    { tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      keyName = pKeyName_
    }

-- | The tags to apply to the new key pair.
createKeyPair_tagSpecifications :: Lens.Lens' CreateKeyPair (Core.Maybe [TagSpecification])
createKeyPair_tagSpecifications = Lens.lens (\CreateKeyPair' {tagSpecifications} -> tagSpecifications) (\s@CreateKeyPair' {} a -> s {tagSpecifications = a} :: CreateKeyPair) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createKeyPair_dryRun :: Lens.Lens' CreateKeyPair (Core.Maybe Core.Bool)
createKeyPair_dryRun = Lens.lens (\CreateKeyPair' {dryRun} -> dryRun) (\s@CreateKeyPair' {} a -> s {dryRun = a} :: CreateKeyPair)

-- | A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
createKeyPair_keyName :: Lens.Lens' CreateKeyPair Core.Text
createKeyPair_keyName = Lens.lens (\CreateKeyPair' {keyName} -> keyName) (\s@CreateKeyPair' {} a -> s {keyName = a} :: CreateKeyPair)

instance Core.AWSRequest CreateKeyPair where
  type
    AWSResponse CreateKeyPair =
      CreateKeyPairResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateKeyPairResponse'
            Core.<$> (x Core..@? "keyPairId")
            Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "keyName")
            Core.<*> (x Core..@ "keyFingerprint")
            Core.<*> (x Core..@ "keyMaterial")
      )

instance Core.Hashable CreateKeyPair

instance Core.NFData CreateKeyPair

instance Core.ToHeaders CreateKeyPair where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateKeyPair where
  toPath = Core.const "/"

instance Core.ToQuery CreateKeyPair where
  toQuery CreateKeyPair' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateKeyPair" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "KeyName" Core.=: keyName
      ]

-- | Describes a key pair.
--
-- /See:/ 'newCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { -- | The ID of the key pair.
    keyPairId :: Core.Maybe Core.Text,
    -- | Any tags applied to the key pair.
    tags :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the key pair.
    keyName :: Core.Text,
    -- | The SHA-1 digest of the DER encoded private key.
    keyFingerprint :: Core.Text,
    -- | An unencrypted PEM encoded RSA private key.
    keyMaterial :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairId', 'createKeyPairResponse_keyPairId' - The ID of the key pair.
--
-- 'tags', 'createKeyPairResponse_tags' - Any tags applied to the key pair.
--
-- 'httpStatus', 'createKeyPairResponse_httpStatus' - The response's http status code.
--
-- 'keyName', 'createKeyPairResponse_keyName' - The name of the key pair.
--
-- 'keyFingerprint', 'createKeyPairResponse_keyFingerprint' - The SHA-1 digest of the DER encoded private key.
--
-- 'keyMaterial', 'createKeyPairResponse_keyMaterial' - An unencrypted PEM encoded RSA private key.
newCreateKeyPairResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'keyName'
  Core.Text ->
  -- | 'keyFingerprint'
  Core.Text ->
  -- | 'keyMaterial'
  Core.Text ->
  CreateKeyPairResponse
newCreateKeyPairResponse
  pHttpStatus_
  pKeyName_
  pKeyFingerprint_
  pKeyMaterial_ =
    CreateKeyPairResponse'
      { keyPairId = Core.Nothing,
        tags = Core.Nothing,
        httpStatus = pHttpStatus_,
        keyName = pKeyName_,
        keyFingerprint = pKeyFingerprint_,
        keyMaterial = Core._Sensitive Lens.# pKeyMaterial_
      }

-- | The ID of the key pair.
createKeyPairResponse_keyPairId :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Core.Text)
createKeyPairResponse_keyPairId = Lens.lens (\CreateKeyPairResponse' {keyPairId} -> keyPairId) (\s@CreateKeyPairResponse' {} a -> s {keyPairId = a} :: CreateKeyPairResponse)

-- | Any tags applied to the key pair.
createKeyPairResponse_tags :: Lens.Lens' CreateKeyPairResponse (Core.Maybe [Tag])
createKeyPairResponse_tags = Lens.lens (\CreateKeyPairResponse' {tags} -> tags) (\s@CreateKeyPairResponse' {} a -> s {tags = a} :: CreateKeyPairResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createKeyPairResponse_httpStatus :: Lens.Lens' CreateKeyPairResponse Core.Int
createKeyPairResponse_httpStatus = Lens.lens (\CreateKeyPairResponse' {httpStatus} -> httpStatus) (\s@CreateKeyPairResponse' {} a -> s {httpStatus = a} :: CreateKeyPairResponse)

-- | The name of the key pair.
createKeyPairResponse_keyName :: Lens.Lens' CreateKeyPairResponse Core.Text
createKeyPairResponse_keyName = Lens.lens (\CreateKeyPairResponse' {keyName} -> keyName) (\s@CreateKeyPairResponse' {} a -> s {keyName = a} :: CreateKeyPairResponse)

-- | The SHA-1 digest of the DER encoded private key.
createKeyPairResponse_keyFingerprint :: Lens.Lens' CreateKeyPairResponse Core.Text
createKeyPairResponse_keyFingerprint = Lens.lens (\CreateKeyPairResponse' {keyFingerprint} -> keyFingerprint) (\s@CreateKeyPairResponse' {} a -> s {keyFingerprint = a} :: CreateKeyPairResponse)

-- | An unencrypted PEM encoded RSA private key.
createKeyPairResponse_keyMaterial :: Lens.Lens' CreateKeyPairResponse Core.Text
createKeyPairResponse_keyMaterial = Lens.lens (\CreateKeyPairResponse' {keyMaterial} -> keyMaterial) (\s@CreateKeyPairResponse' {} a -> s {keyMaterial = a} :: CreateKeyPairResponse) Core.. Core._Sensitive

instance Core.NFData CreateKeyPairResponse

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
-- Module      : Network.AWS.KMS.DescribeKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides detailed information about a customer master key (CMK). You can
-- run @DescribeKey@ on a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>
-- or an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMK>.
--
-- This detailed information includes the key ARN, creation date (and
-- deletion date, if applicable), the key state, and the origin and
-- expiration date (if any) of the key material. For CMKs in custom key
-- stores, it includes information about the custom key store, such as the
-- key store ID and the AWS CloudHSM cluster ID. It includes fields, like
-- @KeySpec@, that help you distinguish symmetric from asymmetric CMKs. It
-- also provides information that is particularly important to asymmetric
-- CMKs, such as the key usage (encryption or signing) and the encryption
-- algorithms or signing algorithms that the CMK supports.
--
-- @DescribeKey@ does not return the following information:
--
-- -   Aliases associated with the CMK. To get this information, use
--     ListAliases.
--
-- -   Whether automatic key rotation is enabled on the CMK. To get this
--     information, use GetKeyRotationStatus. Also, some key states prevent
--     a CMK from being automatically rotated. For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html#rotate-keys-how-it-works How Automatic Key Rotation Works>
--     in /AWS Key Management Service Developer Guide/.
--
-- -   Tags on the CMK. To get this information, use ListResourceTags.
--
-- -   Key policies and grants on the CMK. To get this information, use
--     GetKeyPolicy and ListGrants.
--
-- If you call the @DescribeKey@ operation on a /predefined AWS alias/,
-- that is, an AWS alias with no key ID, AWS KMS creates an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys AWS managed CMK>.
-- Then, it associates the alias with the new CMK, and returns the @KeyId@
-- and @Arn@ of the new CMK in the response.
--
-- __Cross-account use__: Yes. To perform this operation with a CMK in a
-- different AWS account, specify the key ARN or alias ARN in the value of
-- the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DescribeKey>
-- (key policy)
--
-- __Related operations:__
--
-- -   GetKeyPolicy
--
-- -   GetKeyRotationStatus
--
-- -   ListAliases
--
-- -   ListGrants
--
-- -   ListKeys
--
-- -   ListResourceTags
--
-- -   ListRetirableGrants
module Network.AWS.KMS.DescribeKey
  ( -- * Creating a Request
    DescribeKey (..),
    newDescribeKey,

    -- * Request Lenses
    describeKey_grantTokens,
    describeKey_keyId,

    -- * Destructuring the Response
    DescribeKeyResponse (..),
    newDescribeKeyResponse,

    -- * Response Lenses
    describeKeyResponse_keyMetadata,
    describeKeyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeKey' smart constructor.
data DescribeKey = DescribeKey'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Core.Maybe [Core.Text],
    -- | Describes the specified customer master key (CMK).
    --
    -- If you specify a predefined AWS alias (an AWS alias with no key ID), KMS
    -- associates the alias with an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys AWS managed CMK>
    -- and returns its @KeyId@ and @Arn@ in the response.
    --
    -- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
    -- name, or alias ARN. When using an alias name, prefix it with
    -- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
    -- the key ARN or alias ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
    -- get the alias name and alias ARN, use ListAliases.
    keyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'describeKey_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'keyId', 'describeKey_keyId' - Describes the specified customer master key (CMK).
--
-- If you specify a predefined AWS alias (an AWS alias with no key ID), KMS
-- associates the alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys AWS managed CMK>
-- and returns its @KeyId@ and @Arn@ in the response.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. When using an alias name, prefix it with
-- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
-- the key ARN or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
-- get the alias name and alias ARN, use ListAliases.
newDescribeKey ::
  -- | 'keyId'
  Core.Text ->
  DescribeKey
newDescribeKey pKeyId_ =
  DescribeKey'
    { grantTokens = Core.Nothing,
      keyId = pKeyId_
    }

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
describeKey_grantTokens :: Lens.Lens' DescribeKey (Core.Maybe [Core.Text])
describeKey_grantTokens = Lens.lens (\DescribeKey' {grantTokens} -> grantTokens) (\s@DescribeKey' {} a -> s {grantTokens = a} :: DescribeKey) Core.. Lens.mapping Lens._Coerce

-- | Describes the specified customer master key (CMK).
--
-- If you specify a predefined AWS alias (an AWS alias with no key ID), KMS
-- associates the alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys AWS managed CMK>
-- and returns its @KeyId@ and @Arn@ in the response.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. When using an alias name, prefix it with
-- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
-- the key ARN or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
-- get the alias name and alias ARN, use ListAliases.
describeKey_keyId :: Lens.Lens' DescribeKey Core.Text
describeKey_keyId = Lens.lens (\DescribeKey' {keyId} -> keyId) (\s@DescribeKey' {} a -> s {keyId = a} :: DescribeKey)

instance Core.AWSRequest DescribeKey where
  type AWSResponse DescribeKey = DescribeKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKeyResponse'
            Core.<$> (x Core..?> "KeyMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeKey

instance Core.NFData DescribeKey

instance Core.ToHeaders DescribeKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.DescribeKey" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeKey where
  toJSON DescribeKey' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GrantTokens" Core..=) Core.<$> grantTokens,
            Core.Just ("KeyId" Core..= keyId)
          ]
      )

instance Core.ToPath DescribeKey where
  toPath = Core.const "/"

instance Core.ToQuery DescribeKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeKeyResponse' smart constructor.
data DescribeKeyResponse = DescribeKeyResponse'
  { -- | Metadata associated with the key.
    keyMetadata :: Core.Maybe KeyMetadata,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyMetadata', 'describeKeyResponse_keyMetadata' - Metadata associated with the key.
--
-- 'httpStatus', 'describeKeyResponse_httpStatus' - The response's http status code.
newDescribeKeyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeKeyResponse
newDescribeKeyResponse pHttpStatus_ =
  DescribeKeyResponse'
    { keyMetadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata associated with the key.
describeKeyResponse_keyMetadata :: Lens.Lens' DescribeKeyResponse (Core.Maybe KeyMetadata)
describeKeyResponse_keyMetadata = Lens.lens (\DescribeKeyResponse' {keyMetadata} -> keyMetadata) (\s@DescribeKeyResponse' {} a -> s {keyMetadata = a} :: DescribeKeyResponse)

-- | The response's http status code.
describeKeyResponse_httpStatus :: Lens.Lens' DescribeKeyResponse Core.Int
describeKeyResponse_httpStatus = Lens.lens (\DescribeKeyResponse' {httpStatus} -> httpStatus) (\s@DescribeKeyResponse' {} a -> s {httpStatus = a} :: DescribeKeyResponse)

instance Core.NFData DescribeKeyResponse

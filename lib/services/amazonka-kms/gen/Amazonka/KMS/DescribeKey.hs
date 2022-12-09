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
-- Module      : Amazonka.KMS.DescribeKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides detailed information about a KMS key. You can run @DescribeKey@
-- on a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>
-- or an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed key>.
--
-- This detailed information includes the key ARN, creation date (and
-- deletion date, if applicable), the key state, and the origin and
-- expiration date (if any) of the key material. It includes fields, like
-- @KeySpec@, that help you distinguish different types of KMS keys. It
-- also displays the key usage (encryption, signing, or generating and
-- verifying MACs) and the algorithms that the KMS key supports.
--
-- For
-- <kms/latest/developerguide/multi-region-keys-overview.html multi-Region keys>,
-- @DescribeKey@ displays the primary key and all related replica keys. For
-- KMS keys in
-- <kms/latest/developerguide/keystore-cloudhsm.html CloudHSM key stores>,
-- it includes information about the key store, such as the key store ID
-- and the CloudHSM cluster ID. For KMS keys in
-- <kms/latest/developerguide/keystore-external.html external key stores>,
-- it includes the custom key store ID and the ID of the external key.
--
-- @DescribeKey@ does not return the following information:
--
-- -   Aliases associated with the KMS key. To get this information, use
--     ListAliases.
--
-- -   Whether automatic key rotation is enabled on the KMS key. To get
--     this information, use GetKeyRotationStatus. Also, some key states
--     prevent a KMS key from being automatically rotated. For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html#rotate-keys-how-it-works How Automatic Key Rotation Works>
--     in the /Key Management Service Developer Guide/.
--
-- -   Tags on the KMS key. To get this information, use ListResourceTags.
--
-- -   Key policies and grants on the KMS key. To get this information, use
--     GetKeyPolicy and ListGrants.
--
-- In general, @DescribeKey@ is a non-mutating operation. It returns data
-- about KMS keys, but doesn\'t change them. However, Amazon Web Services
-- services use @DescribeKey@ to create
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>
-- from a /predefined Amazon Web Services alias/ with no key ID.
--
-- __Cross-account use__: Yes. To perform this operation with a KMS key in
-- a different Amazon Web Services account, specify the key ARN or alias
-- ARN in the value of the @KeyId@ parameter.
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
module Amazonka.KMS.DescribeKey
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeKey' smart constructor.
data DescribeKey = DescribeKey'
  { -- | A list of grant tokens.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Describes the specified KMS key.
    --
    -- If you specify a predefined Amazon Web Services alias (an Amazon Web
    -- Services alias with no key ID), KMS associates the alias with an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html##aws-managed-cmk Amazon Web Services managed key>
    -- and returns its @KeyId@ and @Arn@ in the response.
    --
    -- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
    -- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
    -- key in a different Amazon Web Services account, you must use the key ARN
    -- or alias ARN.
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
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey. To get the alias name and alias ARN, use ListAliases.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'keyId', 'describeKey_keyId' - Describes the specified KMS key.
--
-- If you specify a predefined Amazon Web Services alias (an Amazon Web
-- Services alias with no key ID), KMS associates the alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html##aws-managed-cmk Amazon Web Services managed key>
-- and returns its @KeyId@ and @Arn@ in the response.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
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
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
newDescribeKey ::
  -- | 'keyId'
  Prelude.Text ->
  DescribeKey
newDescribeKey pKeyId_ =
  DescribeKey'
    { grantTokens = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
describeKey_grantTokens :: Lens.Lens' DescribeKey (Prelude.Maybe [Prelude.Text])
describeKey_grantTokens = Lens.lens (\DescribeKey' {grantTokens} -> grantTokens) (\s@DescribeKey' {} a -> s {grantTokens = a} :: DescribeKey) Prelude.. Lens.mapping Lens.coerced

-- | Describes the specified KMS key.
--
-- If you specify a predefined Amazon Web Services alias (an Amazon Web
-- Services alias with no key ID), KMS associates the alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html##aws-managed-cmk Amazon Web Services managed key>
-- and returns its @KeyId@ and @Arn@ in the response.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
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
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
describeKey_keyId :: Lens.Lens' DescribeKey Prelude.Text
describeKey_keyId = Lens.lens (\DescribeKey' {keyId} -> keyId) (\s@DescribeKey' {} a -> s {keyId = a} :: DescribeKey)

instance Core.AWSRequest DescribeKey where
  type AWSResponse DescribeKey = DescribeKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKeyResponse'
            Prelude.<$> (x Data..?> "KeyMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeKey where
  hashWithSalt _salt DescribeKey' {..} =
    _salt `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData DescribeKey where
  rnf DescribeKey' {..} =
    Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToHeaders DescribeKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.DescribeKey" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeKey where
  toJSON DescribeKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Data..=) Prelude.<$> grantTokens,
            Prelude.Just ("KeyId" Data..= keyId)
          ]
      )

instance Data.ToPath DescribeKey where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeKeyResponse' smart constructor.
data DescribeKeyResponse = DescribeKeyResponse'
  { -- | Metadata associated with the key.
    keyMetadata :: Prelude.Maybe KeyMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeKeyResponse
newDescribeKeyResponse pHttpStatus_ =
  DescribeKeyResponse'
    { keyMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata associated with the key.
describeKeyResponse_keyMetadata :: Lens.Lens' DescribeKeyResponse (Prelude.Maybe KeyMetadata)
describeKeyResponse_keyMetadata = Lens.lens (\DescribeKeyResponse' {keyMetadata} -> keyMetadata) (\s@DescribeKeyResponse' {} a -> s {keyMetadata = a} :: DescribeKeyResponse)

-- | The response's http status code.
describeKeyResponse_httpStatus :: Lens.Lens' DescribeKeyResponse Prelude.Int
describeKeyResponse_httpStatus = Lens.lens (\DescribeKeyResponse' {httpStatus} -> httpStatus) (\s@DescribeKeyResponse' {} a -> s {httpStatus = a} :: DescribeKeyResponse)

instance Prelude.NFData DescribeKeyResponse where
  rnf DescribeKeyResponse' {..} =
    Prelude.rnf keyMetadata
      `Prelude.seq` Prelude.rnf httpStatus

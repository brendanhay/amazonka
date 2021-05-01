{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.CreateGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a grant to a customer master key (CMK). The grant allows the
-- grantee principal to use the CMK when the conditions specified in the
-- grant are met. When setting permissions, grants are an alternative to
-- key policies.
--
-- To create a grant that allows a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- only when the request includes a particular
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>,
-- use the @Constraints@ parameter. For details, see GrantConstraints.
--
-- You can create grants on symmetric and asymmetric CMKs. However, if the
-- grant allows an operation that the CMK does not support, @CreateGrant@
-- fails with a @ValidationException@.
--
-- -   Grants for symmetric CMKs cannot allow operations that are not
--     supported for symmetric CMKs, including Sign, Verify, and
--     GetPublicKey. (There are limited exceptions to this rule for legacy
--     operations, but you should not create a grant for an operation that
--     AWS KMS does not support.)
--
-- -   Grants for asymmetric CMKs cannot allow operations that are not
--     supported for asymmetric CMKs, including operations that
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKey generate data keys>
--     or
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyPair data key pairs>,
--     or operations related to
--     <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic key rotation>,
--     <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
--     or CMKs in
--     <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>.
--
-- -   Grants for asymmetric CMKs with a @KeyUsage@ of @ENCRYPT_DECRYPT@
--     cannot allow the Sign or Verify operations. Grants for asymmetric
--     CMKs with a @KeyUsage@ of @SIGN_VERIFY@ cannot allow the Encrypt or
--     Decrypt operations.
--
-- -   Grants for asymmetric CMKs cannot include an encryption context
--     grant constraint. An encryption context is not supported on
--     asymmetric CMKs.
--
-- For information about symmetric and asymmetric CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs>
-- in the /AWS Key Management Service Developer Guide/. For more
-- information about grants, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants>
-- in the //AWS Key Management Service Developer Guide// .
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation on a CMK in a
-- different AWS account, specify the key ARN in the value of the @KeyId@
-- parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateGrant>
-- (key policy)
--
-- __Related operations:__
--
-- -   ListGrants
--
-- -   ListRetirableGrants
--
-- -   RetireGrant
--
-- -   RevokeGrant
module Network.AWS.KMS.CreateGrant
  ( -- * Creating a Request
    CreateGrant (..),
    newCreateGrant,

    -- * Request Lenses
    createGrant_constraints,
    createGrant_grantTokens,
    createGrant_name,
    createGrant_retiringPrincipal,
    createGrant_keyId,
    createGrant_granteePrincipal,
    createGrant_operations,

    -- * Destructuring the Response
    CreateGrantResponse (..),
    newCreateGrantResponse,

    -- * Response Lenses
    createGrantResponse_grantToken,
    createGrantResponse_grantId,
    createGrantResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGrant' smart constructor.
data CreateGrant = CreateGrant'
  { -- | Allows a
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
    -- only when the encryption context matches or includes the encryption
    -- context specified in this structure. For more information about
    -- encryption context, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
    -- in the //AWS Key Management Service Developer Guide// .
    --
    -- Grant constraints are not applied to operations that do not support an
    -- encryption context, such as cryptographic operations with asymmetric
    -- CMKs and management operations, such as DescribeKey or RetireGrant.
    constraints :: Prelude.Maybe GrantConstraints,
    -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | A friendly name for the grant. Use this value to prevent the unintended
    -- creation of duplicate grants when retrying this request.
    --
    -- When this value is absent, all @CreateGrant@ requests result in a new
    -- grant with a unique @GrantId@ even if all the supplied parameters are
    -- identical. This can result in unintended duplicates when you retry the
    -- @CreateGrant@ request.
    --
    -- When this value is present, you can retry a @CreateGrant@ request with
    -- identical parameters; if the grant already exists, the original
    -- @GrantId@ is returned without creating a new grant. Note that the
    -- returned grant token is unique with every @CreateGrant@ request, even
    -- when a duplicate @GrantId@ is returned. All grant tokens for the same
    -- grant ID can be used interchangeably.
    name :: Prelude.Maybe Prelude.Text,
    -- | The principal that is given permission to retire the grant by using
    -- RetireGrant operation.
    --
    -- To specify the principal, use the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of an AWS principal. Valid AWS principals include AWS accounts (root),
    -- IAM users, federated users, and assumed role users. For examples of the
    -- ARN syntax to use for specifying a principal, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)>
    -- in the Example ARNs section of the /AWS General Reference/.
    retiringPrincipal :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the customer master key (CMK) that the grant
    -- applies to.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
    -- specify a CMK in a different AWS account, you must use the key ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Prelude.Text,
    -- | The principal that is given permission to perform the operations that
    -- the grant permits.
    --
    -- To specify the principal, use the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of an AWS principal. Valid AWS principals include AWS accounts (root),
    -- IAM users, IAM roles, federated users, and assumed role users. For
    -- examples of the ARN syntax to use for specifying a principal, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)>
    -- in the Example ARNs section of the /AWS General Reference/.
    granteePrincipal :: Prelude.Text,
    -- | A list of operations that the grant permits.
    operations :: [GrantOperation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'createGrant_constraints' - Allows a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- only when the encryption context matches or includes the encryption
-- context specified in this structure. For more information about
-- encryption context, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the //AWS Key Management Service Developer Guide// .
--
-- Grant constraints are not applied to operations that do not support an
-- encryption context, such as cryptographic operations with asymmetric
-- CMKs and management operations, such as DescribeKey or RetireGrant.
--
-- 'grantTokens', 'createGrant_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'name', 'createGrant_name' - A friendly name for the grant. Use this value to prevent the unintended
-- creation of duplicate grants when retrying this request.
--
-- When this value is absent, all @CreateGrant@ requests result in a new
-- grant with a unique @GrantId@ even if all the supplied parameters are
-- identical. This can result in unintended duplicates when you retry the
-- @CreateGrant@ request.
--
-- When this value is present, you can retry a @CreateGrant@ request with
-- identical parameters; if the grant already exists, the original
-- @GrantId@ is returned without creating a new grant. Note that the
-- returned grant token is unique with every @CreateGrant@ request, even
-- when a duplicate @GrantId@ is returned. All grant tokens for the same
-- grant ID can be used interchangeably.
--
-- 'retiringPrincipal', 'createGrant_retiringPrincipal' - The principal that is given permission to retire the grant by using
-- RetireGrant operation.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an AWS principal. Valid AWS principals include AWS accounts (root),
-- IAM users, federated users, and assumed role users. For examples of the
-- ARN syntax to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)>
-- in the Example ARNs section of the /AWS General Reference/.
--
-- 'keyId', 'createGrant_keyId' - The unique identifier for the customer master key (CMK) that the grant
-- applies to.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
-- specify a CMK in a different AWS account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
--
-- 'granteePrincipal', 'createGrant_granteePrincipal' - The principal that is given permission to perform the operations that
-- the grant permits.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an AWS principal. Valid AWS principals include AWS accounts (root),
-- IAM users, IAM roles, federated users, and assumed role users. For
-- examples of the ARN syntax to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)>
-- in the Example ARNs section of the /AWS General Reference/.
--
-- 'operations', 'createGrant_operations' - A list of operations that the grant permits.
newCreateGrant ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'granteePrincipal'
  Prelude.Text ->
  CreateGrant
newCreateGrant pKeyId_ pGranteePrincipal_ =
  CreateGrant'
    { constraints = Prelude.Nothing,
      grantTokens = Prelude.Nothing,
      name = Prelude.Nothing,
      retiringPrincipal = Prelude.Nothing,
      keyId = pKeyId_,
      granteePrincipal = pGranteePrincipal_,
      operations = Prelude.mempty
    }

-- | Allows a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- only when the encryption context matches or includes the encryption
-- context specified in this structure. For more information about
-- encryption context, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the //AWS Key Management Service Developer Guide// .
--
-- Grant constraints are not applied to operations that do not support an
-- encryption context, such as cryptographic operations with asymmetric
-- CMKs and management operations, such as DescribeKey or RetireGrant.
createGrant_constraints :: Lens.Lens' CreateGrant (Prelude.Maybe GrantConstraints)
createGrant_constraints = Lens.lens (\CreateGrant' {constraints} -> constraints) (\s@CreateGrant' {} a -> s {constraints = a} :: CreateGrant)

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
createGrant_grantTokens :: Lens.Lens' CreateGrant (Prelude.Maybe [Prelude.Text])
createGrant_grantTokens = Lens.lens (\CreateGrant' {grantTokens} -> grantTokens) (\s@CreateGrant' {} a -> s {grantTokens = a} :: CreateGrant) Prelude.. Lens.mapping Prelude._Coerce

-- | A friendly name for the grant. Use this value to prevent the unintended
-- creation of duplicate grants when retrying this request.
--
-- When this value is absent, all @CreateGrant@ requests result in a new
-- grant with a unique @GrantId@ even if all the supplied parameters are
-- identical. This can result in unintended duplicates when you retry the
-- @CreateGrant@ request.
--
-- When this value is present, you can retry a @CreateGrant@ request with
-- identical parameters; if the grant already exists, the original
-- @GrantId@ is returned without creating a new grant. Note that the
-- returned grant token is unique with every @CreateGrant@ request, even
-- when a duplicate @GrantId@ is returned. All grant tokens for the same
-- grant ID can be used interchangeably.
createGrant_name :: Lens.Lens' CreateGrant (Prelude.Maybe Prelude.Text)
createGrant_name = Lens.lens (\CreateGrant' {name} -> name) (\s@CreateGrant' {} a -> s {name = a} :: CreateGrant)

-- | The principal that is given permission to retire the grant by using
-- RetireGrant operation.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an AWS principal. Valid AWS principals include AWS accounts (root),
-- IAM users, federated users, and assumed role users. For examples of the
-- ARN syntax to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)>
-- in the Example ARNs section of the /AWS General Reference/.
createGrant_retiringPrincipal :: Lens.Lens' CreateGrant (Prelude.Maybe Prelude.Text)
createGrant_retiringPrincipal = Lens.lens (\CreateGrant' {retiringPrincipal} -> retiringPrincipal) (\s@CreateGrant' {} a -> s {retiringPrincipal = a} :: CreateGrant)

-- | The unique identifier for the customer master key (CMK) that the grant
-- applies to.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
-- specify a CMK in a different AWS account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
createGrant_keyId :: Lens.Lens' CreateGrant Prelude.Text
createGrant_keyId = Lens.lens (\CreateGrant' {keyId} -> keyId) (\s@CreateGrant' {} a -> s {keyId = a} :: CreateGrant)

-- | The principal that is given permission to perform the operations that
-- the grant permits.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an AWS principal. Valid AWS principals include AWS accounts (root),
-- IAM users, IAM roles, federated users, and assumed role users. For
-- examples of the ARN syntax to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)>
-- in the Example ARNs section of the /AWS General Reference/.
createGrant_granteePrincipal :: Lens.Lens' CreateGrant Prelude.Text
createGrant_granteePrincipal = Lens.lens (\CreateGrant' {granteePrincipal} -> granteePrincipal) (\s@CreateGrant' {} a -> s {granteePrincipal = a} :: CreateGrant)

-- | A list of operations that the grant permits.
createGrant_operations :: Lens.Lens' CreateGrant [GrantOperation]
createGrant_operations = Lens.lens (\CreateGrant' {operations} -> operations) (\s@CreateGrant' {} a -> s {operations = a} :: CreateGrant) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreateGrant where
  type Rs CreateGrant = CreateGrantResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGrantResponse'
            Prelude.<$> (x Prelude..?> "GrantToken")
            Prelude.<*> (x Prelude..?> "GrantId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGrant

instance Prelude.NFData CreateGrant

instance Prelude.ToHeaders CreateGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.CreateGrant" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateGrant where
  toJSON CreateGrant' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Constraints" Prelude..=) Prelude.<$> constraints,
            ("GrantTokens" Prelude..=) Prelude.<$> grantTokens,
            ("Name" Prelude..=) Prelude.<$> name,
            ("RetiringPrincipal" Prelude..=)
              Prelude.<$> retiringPrincipal,
            Prelude.Just ("KeyId" Prelude..= keyId),
            Prelude.Just
              ("GranteePrincipal" Prelude..= granteePrincipal),
            Prelude.Just ("Operations" Prelude..= operations)
          ]
      )

instance Prelude.ToPath CreateGrant where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGrantResponse' smart constructor.
data CreateGrantResponse = CreateGrantResponse'
  { -- | The grant token.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the grant.
    --
    -- You can use the @GrantId@ in a ListGrants, RetireGrant, or RevokeGrant
    -- operation.
    grantId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantToken', 'createGrantResponse_grantToken' - The grant token.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'grantId', 'createGrantResponse_grantId' - The unique identifier for the grant.
--
-- You can use the @GrantId@ in a ListGrants, RetireGrant, or RevokeGrant
-- operation.
--
-- 'httpStatus', 'createGrantResponse_httpStatus' - The response's http status code.
newCreateGrantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGrantResponse
newCreateGrantResponse pHttpStatus_ =
  CreateGrantResponse'
    { grantToken = Prelude.Nothing,
      grantId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The grant token.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
createGrantResponse_grantToken :: Lens.Lens' CreateGrantResponse (Prelude.Maybe Prelude.Text)
createGrantResponse_grantToken = Lens.lens (\CreateGrantResponse' {grantToken} -> grantToken) (\s@CreateGrantResponse' {} a -> s {grantToken = a} :: CreateGrantResponse)

-- | The unique identifier for the grant.
--
-- You can use the @GrantId@ in a ListGrants, RetireGrant, or RevokeGrant
-- operation.
createGrantResponse_grantId :: Lens.Lens' CreateGrantResponse (Prelude.Maybe Prelude.Text)
createGrantResponse_grantId = Lens.lens (\CreateGrantResponse' {grantId} -> grantId) (\s@CreateGrantResponse' {} a -> s {grantId = a} :: CreateGrantResponse)

-- | The response's http status code.
createGrantResponse_httpStatus :: Lens.Lens' CreateGrantResponse Prelude.Int
createGrantResponse_httpStatus = Lens.lens (\CreateGrantResponse' {httpStatus} -> httpStatus) (\s@CreateGrantResponse' {} a -> s {httpStatus = a} :: CreateGrantResponse)

instance Prelude.NFData CreateGrantResponse

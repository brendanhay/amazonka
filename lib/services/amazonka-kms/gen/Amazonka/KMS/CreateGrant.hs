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
-- Module      : Amazonka.KMS.CreateGrant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a grant to a KMS key.
--
-- A /grant/ is a policy instrument that allows Amazon Web Services
-- principals to use KMS keys in cryptographic operations. It also can
-- allow them to view a KMS key (DescribeKey) and create and manage grants.
-- When authorizing access to a KMS key, grants are considered along with
-- key policies and IAM policies. Grants are often used for temporary
-- permissions because you can create one, use its permissions, and delete
-- it without changing your key policies or IAM policies.
--
-- For detailed information about grants, including grant terminology, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants in KMS>
-- in the //Key Management Service Developer Guide// . For examples of
-- working with grants in several programming languages, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/programming-grants.html Programming grants>.
--
-- The @CreateGrant@ operation returns a @GrantToken@ and a @GrantId@.
--
-- -   When you create, retire, or revoke a grant, there might be a brief
--     delay, usually less than five minutes, until the grant is available
--     throughout KMS. This state is known as /eventual consistency/. Once
--     the grant has achieved eventual consistency, the grantee principal
--     can use the permissions in the grant without identifying the grant.
--
--     However, to use the permissions in the grant immediately, use the
--     @GrantToken@ that @CreateGrant@ returns. For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
--     in the //Key Management Service Developer Guide// .
--
-- -   The @CreateGrant@ operation also returns a @GrantId@. You can use
--     the @GrantId@ and a key identifier to identify the grant in the
--     RetireGrant and RevokeGrant operations. To find the grant ID, use
--     the ListGrants or ListRetirableGrants operations.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation on a KMS key in a
-- different Amazon Web Services account, specify the key ARN in the value
-- of the @KeyId@ parameter.
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
module Amazonka.KMS.CreateGrant
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
    createGrantResponse_grantId,
    createGrantResponse_grantToken,
    createGrantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGrant' smart constructor.
data CreateGrant = CreateGrant'
  { -- | Specifies a grant constraint.
    --
    -- KMS supports the @EncryptionContextEquals@ and @EncryptionContextSubset@
    -- grant constraints. Each constraint value can include up to 8 encryption
    -- context pairs. The encryption context value in each constraint cannot
    -- exceed 384 characters. For information about grant constraints, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-grant-overview.html#grant-constraints Using grant constraints>
    -- in the /Key Management Service Developer Guide/. For more information
    -- about encryption context, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption context>
    -- in the //Key Management Service Developer Guide// .
    --
    -- The encryption context grant constraints allow the permissions in the
    -- grant only when the encryption context in the request matches
    -- (@EncryptionContextEquals@) or includes (@EncryptionContextSubset@) the
    -- encryption context specified in this structure.
    --
    -- The encryption context grant constraints are supported only on
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-grant-operations grant operations>
    -- that include an @EncryptionContext@ parameter, such as cryptographic
    -- operations on symmetric encryption KMS keys. Grants with grant
    -- constraints can include the DescribeKey and RetireGrant operations, but
    -- the constraint doesn\'t apply to these operations. If a grant with a
    -- grant constraint includes the @CreateGrant@ operation, the constraint
    -- requires that any grants created with the @CreateGrant@ permission have
    -- an equally strict or stricter encryption context constraint.
    --
    -- You cannot use an encryption context grant constraint for cryptographic
    -- operations with asymmetric KMS keys or HMAC KMS keys. These keys don\'t
    -- support an encryption context.
    constraints :: Prelude.Maybe GrantConstraints,
    -- | A list of grant tokens.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
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
    -- | The principal that has permission to use the RetireGrant operation to
    -- retire the grant.
    --
    -- To specify the principal, use the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of an Amazon Web Services principal. Valid Amazon Web Services
    -- principals include Amazon Web Services accounts (root), IAM users,
    -- federated users, and assumed role users. For examples of the ARN syntax
    -- to use for specifying a principal, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
    -- in the Example ARNs section of the /Amazon Web Services General
    -- Reference/.
    --
    -- The grant determines the retiring principal. Other principals might have
    -- permission to retire the grant or revoke the grant. For details, see
    -- RevokeGrant and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#grant-delete Retiring and revoking grants>
    -- in the /Key Management Service Developer Guide/.
    retiringPrincipal :: Prelude.Maybe Prelude.Text,
    -- | Identifies the KMS key for the grant. The grant gives principals
    -- permission to use this KMS key.
    --
    -- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
    -- different Amazon Web Services account, you must use the key ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text,
    -- | The identity that gets the permissions specified in the grant.
    --
    -- To specify the principal, use the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of an Amazon Web Services principal. Valid Amazon Web Services
    -- principals include Amazon Web Services accounts (root), IAM users, IAM
    -- roles, federated users, and assumed role users. For examples of the ARN
    -- syntax to use for specifying a principal, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
    -- in the Example ARNs section of the /Amazon Web Services General
    -- Reference/.
    granteePrincipal :: Prelude.Text,
    -- | A list of operations that the grant permits.
    --
    -- This list must include only operations that are permitted in a grant.
    -- Also, the operation must be supported on the KMS key. For example, you
    -- cannot create a grant for a symmetric encryption KMS key that allows the
    -- Sign operation, or a grant for an asymmetric KMS key that allows the
    -- GenerateDataKey operation. If you try, KMS returns a @ValidationError@
    -- exception. For details, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-grant-operations Grant operations>
    -- in the /Key Management Service Developer Guide/.
    operations :: [GrantOperation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'createGrant_constraints' - Specifies a grant constraint.
--
-- KMS supports the @EncryptionContextEquals@ and @EncryptionContextSubset@
-- grant constraints. Each constraint value can include up to 8 encryption
-- context pairs. The encryption context value in each constraint cannot
-- exceed 384 characters. For information about grant constraints, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-grant-overview.html#grant-constraints Using grant constraints>
-- in the /Key Management Service Developer Guide/. For more information
-- about encryption context, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption context>
-- in the //Key Management Service Developer Guide// .
--
-- The encryption context grant constraints allow the permissions in the
-- grant only when the encryption context in the request matches
-- (@EncryptionContextEquals@) or includes (@EncryptionContextSubset@) the
-- encryption context specified in this structure.
--
-- The encryption context grant constraints are supported only on
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-grant-operations grant operations>
-- that include an @EncryptionContext@ parameter, such as cryptographic
-- operations on symmetric encryption KMS keys. Grants with grant
-- constraints can include the DescribeKey and RetireGrant operations, but
-- the constraint doesn\'t apply to these operations. If a grant with a
-- grant constraint includes the @CreateGrant@ operation, the constraint
-- requires that any grants created with the @CreateGrant@ permission have
-- an equally strict or stricter encryption context constraint.
--
-- You cannot use an encryption context grant constraint for cryptographic
-- operations with asymmetric KMS keys or HMAC KMS keys. These keys don\'t
-- support an encryption context.
--
-- 'grantTokens', 'createGrant_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
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
-- 'retiringPrincipal', 'createGrant_retiringPrincipal' - The principal that has permission to use the RetireGrant operation to
-- retire the grant.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an Amazon Web Services principal. Valid Amazon Web Services
-- principals include Amazon Web Services accounts (root), IAM users,
-- federated users, and assumed role users. For examples of the ARN syntax
-- to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
--
-- The grant determines the retiring principal. Other principals might have
-- permission to retire the grant or revoke the grant. For details, see
-- RevokeGrant and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#grant-delete Retiring and revoking grants>
-- in the /Key Management Service Developer Guide/.
--
-- 'keyId', 'createGrant_keyId' - Identifies the KMS key for the grant. The grant gives principals
-- permission to use this KMS key.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
--
-- 'granteePrincipal', 'createGrant_granteePrincipal' - The identity that gets the permissions specified in the grant.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an Amazon Web Services principal. Valid Amazon Web Services
-- principals include Amazon Web Services accounts (root), IAM users, IAM
-- roles, federated users, and assumed role users. For examples of the ARN
-- syntax to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
--
-- 'operations', 'createGrant_operations' - A list of operations that the grant permits.
--
-- This list must include only operations that are permitted in a grant.
-- Also, the operation must be supported on the KMS key. For example, you
-- cannot create a grant for a symmetric encryption KMS key that allows the
-- Sign operation, or a grant for an asymmetric KMS key that allows the
-- GenerateDataKey operation. If you try, KMS returns a @ValidationError@
-- exception. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-grant-operations Grant operations>
-- in the /Key Management Service Developer Guide/.
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

-- | Specifies a grant constraint.
--
-- KMS supports the @EncryptionContextEquals@ and @EncryptionContextSubset@
-- grant constraints. Each constraint value can include up to 8 encryption
-- context pairs. The encryption context value in each constraint cannot
-- exceed 384 characters. For information about grant constraints, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-grant-overview.html#grant-constraints Using grant constraints>
-- in the /Key Management Service Developer Guide/. For more information
-- about encryption context, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption context>
-- in the //Key Management Service Developer Guide// .
--
-- The encryption context grant constraints allow the permissions in the
-- grant only when the encryption context in the request matches
-- (@EncryptionContextEquals@) or includes (@EncryptionContextSubset@) the
-- encryption context specified in this structure.
--
-- The encryption context grant constraints are supported only on
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-grant-operations grant operations>
-- that include an @EncryptionContext@ parameter, such as cryptographic
-- operations on symmetric encryption KMS keys. Grants with grant
-- constraints can include the DescribeKey and RetireGrant operations, but
-- the constraint doesn\'t apply to these operations. If a grant with a
-- grant constraint includes the @CreateGrant@ operation, the constraint
-- requires that any grants created with the @CreateGrant@ permission have
-- an equally strict or stricter encryption context constraint.
--
-- You cannot use an encryption context grant constraint for cryptographic
-- operations with asymmetric KMS keys or HMAC KMS keys. These keys don\'t
-- support an encryption context.
createGrant_constraints :: Lens.Lens' CreateGrant (Prelude.Maybe GrantConstraints)
createGrant_constraints = Lens.lens (\CreateGrant' {constraints} -> constraints) (\s@CreateGrant' {} a -> s {constraints = a} :: CreateGrant)

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
createGrant_grantTokens :: Lens.Lens' CreateGrant (Prelude.Maybe [Prelude.Text])
createGrant_grantTokens = Lens.lens (\CreateGrant' {grantTokens} -> grantTokens) (\s@CreateGrant' {} a -> s {grantTokens = a} :: CreateGrant) Prelude.. Lens.mapping Lens.coerced

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

-- | The principal that has permission to use the RetireGrant operation to
-- retire the grant.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an Amazon Web Services principal. Valid Amazon Web Services
-- principals include Amazon Web Services accounts (root), IAM users,
-- federated users, and assumed role users. For examples of the ARN syntax
-- to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
--
-- The grant determines the retiring principal. Other principals might have
-- permission to retire the grant or revoke the grant. For details, see
-- RevokeGrant and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#grant-delete Retiring and revoking grants>
-- in the /Key Management Service Developer Guide/.
createGrant_retiringPrincipal :: Lens.Lens' CreateGrant (Prelude.Maybe Prelude.Text)
createGrant_retiringPrincipal = Lens.lens (\CreateGrant' {retiringPrincipal} -> retiringPrincipal) (\s@CreateGrant' {} a -> s {retiringPrincipal = a} :: CreateGrant)

-- | Identifies the KMS key for the grant. The grant gives principals
-- permission to use this KMS key.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
createGrant_keyId :: Lens.Lens' CreateGrant Prelude.Text
createGrant_keyId = Lens.lens (\CreateGrant' {keyId} -> keyId) (\s@CreateGrant' {} a -> s {keyId = a} :: CreateGrant)

-- | The identity that gets the permissions specified in the grant.
--
-- To specify the principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an Amazon Web Services principal. Valid Amazon Web Services
-- principals include Amazon Web Services accounts (root), IAM users, IAM
-- roles, federated users, and assumed role users. For examples of the ARN
-- syntax to use for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
createGrant_granteePrincipal :: Lens.Lens' CreateGrant Prelude.Text
createGrant_granteePrincipal = Lens.lens (\CreateGrant' {granteePrincipal} -> granteePrincipal) (\s@CreateGrant' {} a -> s {granteePrincipal = a} :: CreateGrant)

-- | A list of operations that the grant permits.
--
-- This list must include only operations that are permitted in a grant.
-- Also, the operation must be supported on the KMS key. For example, you
-- cannot create a grant for a symmetric encryption KMS key that allows the
-- Sign operation, or a grant for an asymmetric KMS key that allows the
-- GenerateDataKey operation. If you try, KMS returns a @ValidationError@
-- exception. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-grant-operations Grant operations>
-- in the /Key Management Service Developer Guide/.
createGrant_operations :: Lens.Lens' CreateGrant [GrantOperation]
createGrant_operations = Lens.lens (\CreateGrant' {operations} -> operations) (\s@CreateGrant' {} a -> s {operations = a} :: CreateGrant) Prelude.. Lens.coerced

instance Core.AWSRequest CreateGrant where
  type AWSResponse CreateGrant = CreateGrantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGrantResponse'
            Prelude.<$> (x Data..?> "GrantId")
            Prelude.<*> (x Data..?> "GrantToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGrant where
  hashWithSalt _salt CreateGrant' {..} =
    _salt `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` retiringPrincipal
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` granteePrincipal
      `Prelude.hashWithSalt` operations

instance Prelude.NFData CreateGrant where
  rnf CreateGrant' {..} =
    Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf retiringPrincipal
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf granteePrincipal
      `Prelude.seq` Prelude.rnf operations

instance Data.ToHeaders CreateGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.CreateGrant" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGrant where
  toJSON CreateGrant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Constraints" Data..=) Prelude.<$> constraints,
            ("GrantTokens" Data..=) Prelude.<$> grantTokens,
            ("Name" Data..=) Prelude.<$> name,
            ("RetiringPrincipal" Data..=)
              Prelude.<$> retiringPrincipal,
            Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just
              ("GranteePrincipal" Data..= granteePrincipal),
            Prelude.Just ("Operations" Data..= operations)
          ]
      )

instance Data.ToPath CreateGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGrantResponse' smart constructor.
data CreateGrantResponse = CreateGrantResponse'
  { -- | The unique identifier for the grant.
    --
    -- You can use the @GrantId@ in a ListGrants, RetireGrant, or RevokeGrant
    -- operation.
    grantId :: Prelude.Maybe Prelude.Text,
    -- | The grant token.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
    grantToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantId', 'createGrantResponse_grantId' - The unique identifier for the grant.
--
-- You can use the @GrantId@ in a ListGrants, RetireGrant, or RevokeGrant
-- operation.
--
-- 'grantToken', 'createGrantResponse_grantToken' - The grant token.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'httpStatus', 'createGrantResponse_httpStatus' - The response's http status code.
newCreateGrantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGrantResponse
newCreateGrantResponse pHttpStatus_ =
  CreateGrantResponse'
    { grantId = Prelude.Nothing,
      grantToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the grant.
--
-- You can use the @GrantId@ in a ListGrants, RetireGrant, or RevokeGrant
-- operation.
createGrantResponse_grantId :: Lens.Lens' CreateGrantResponse (Prelude.Maybe Prelude.Text)
createGrantResponse_grantId = Lens.lens (\CreateGrantResponse' {grantId} -> grantId) (\s@CreateGrantResponse' {} a -> s {grantId = a} :: CreateGrantResponse)

-- | The grant token.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
createGrantResponse_grantToken :: Lens.Lens' CreateGrantResponse (Prelude.Maybe Prelude.Text)
createGrantResponse_grantToken = Lens.lens (\CreateGrantResponse' {grantToken} -> grantToken) (\s@CreateGrantResponse' {} a -> s {grantToken = a} :: CreateGrantResponse)

-- | The response's http status code.
createGrantResponse_httpStatus :: Lens.Lens' CreateGrantResponse Prelude.Int
createGrantResponse_httpStatus = Lens.lens (\CreateGrantResponse' {httpStatus} -> httpStatus) (\s@CreateGrantResponse' {} a -> s {httpStatus = a} :: CreateGrantResponse)

instance Prelude.NFData CreateGrantResponse where
  rnf CreateGrantResponse' {..} =
    Prelude.rnf grantId
      `Prelude.seq` Prelude.rnf grantToken
      `Prelude.seq` Prelude.rnf httpStatus

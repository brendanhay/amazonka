{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CreateGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a grant to a customer master key (CMK). The grant allows the grantee principal to use the CMK when the conditions specified in the grant are met. When setting permissions, grants are an alternative to key policies.
--
-- To create a grant that allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the request includes a particular <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context> , use the @Constraints@ parameter. For details, see 'GrantConstraints' .
-- You can create grants on symmetric and asymmetric CMKs. However, if the grant allows an operation that the CMK does not support, @CreateGrant@ fails with a @ValidationException@ .
--
--     * Grants for symmetric CMKs cannot allow operations that are not supported for symmetric CMKs, including 'Sign' , 'Verify' , and 'GetPublicKey' . (There are limited exceptions to this rule for legacy operations, but you should not create a grant for an operation that AWS KMS does not support.)
--
--
--     * Grants for asymmetric CMKs cannot allow operations that are not supported for asymmetric CMKs, including operations that <https://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKey generate data keys> or <https://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyPair data key pairs> , or operations related to <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic key rotation> , <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores> .
--
--
--     * Grants for asymmetric CMKs with a @KeyUsage@ of @ENCRYPT_DECRYPT@ cannot allow the 'Sign' or 'Verify' operations. Grants for asymmetric CMKs with a @KeyUsage@ of @SIGN_VERIFY@ cannot allow the 'Encrypt' or 'Decrypt' operations.
--
--
--     * Grants for asymmetric CMKs cannot include an encryption context grant constraint. An encryption context is not supported on asymmetric CMKs.
--
--
-- For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter. For more information about grants, see <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants> in the /\/AWS Key Management Service Developer Guide\/ / .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.CreateGrant
  ( -- * Creating a request
    CreateGrant (..),
    mkCreateGrant,

    -- ** Request lenses
    cgKeyId,
    cgGranteePrincipal,
    cgOperations,
    cgConstraints,
    cgGrantTokens,
    cgName,
    cgRetiringPrincipal,

    -- * Destructuring the response
    CreateGrantResponse (..),
    mkCreateGrantResponse,

    -- ** Response lenses
    cgrrsGrantId,
    cgrrsGrantToken,
    cgrrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGrant' smart constructor.
data CreateGrant = CreateGrant'
  { -- | The unique identifier for the customer master key (CMK) that the grant applies to.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
    keyId :: Types.KeyIdType,
    -- | The principal that is given permission to perform the operations that the grant permits.
    --
    -- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
    granteePrincipal :: Types.PrincipalIdType,
    -- | A list of operations that the grant permits.
    operations :: [Types.GrantOperation],
    -- | Allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the encryption context matches or includes the encryption context specified in this structure. For more information about encryption context, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /\/AWS Key Management Service Developer Guide\/ / .
    constraints :: Core.Maybe Types.GrantConstraints,
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Core.Maybe [Types.GrantTokenType],
    -- | A friendly name for identifying the grant. Use this value to prevent the unintended creation of duplicate grants when retrying this request.
    --
    -- When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request.
    -- When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
    name :: Core.Maybe Types.Name,
    -- | The principal that is given permission to retire the grant by using 'RetireGrant' operation.
    --
    -- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
    retiringPrincipal :: Core.Maybe Types.PrincipalIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGrant' value with any optional fields omitted.
mkCreateGrant ::
  -- | 'keyId'
  Types.KeyIdType ->
  -- | 'granteePrincipal'
  Types.PrincipalIdType ->
  CreateGrant
mkCreateGrant keyId granteePrincipal =
  CreateGrant'
    { keyId,
      granteePrincipal,
      operations = Core.mempty,
      constraints = Core.Nothing,
      grantTokens = Core.Nothing,
      name = Core.Nothing,
      retiringPrincipal = Core.Nothing
    }

-- | The unique identifier for the customer master key (CMK) that the grant applies to.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgKeyId :: Lens.Lens' CreateGrant Types.KeyIdType
cgKeyId = Lens.field @"keyId"
{-# DEPRECATED cgKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The principal that is given permission to perform the operations that the grant permits.
--
-- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- /Note:/ Consider using 'granteePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGranteePrincipal :: Lens.Lens' CreateGrant Types.PrincipalIdType
cgGranteePrincipal = Lens.field @"granteePrincipal"
{-# DEPRECATED cgGranteePrincipal "Use generic-lens or generic-optics with 'granteePrincipal' instead." #-}

-- | A list of operations that the grant permits.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgOperations :: Lens.Lens' CreateGrant [Types.GrantOperation]
cgOperations = Lens.field @"operations"
{-# DEPRECATED cgOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | Allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the encryption context matches or includes the encryption context specified in this structure. For more information about encryption context, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /\/AWS Key Management Service Developer Guide\/ / .
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgConstraints :: Lens.Lens' CreateGrant (Core.Maybe Types.GrantConstraints)
cgConstraints = Lens.field @"constraints"
{-# DEPRECATED cgConstraints "Use generic-lens or generic-optics with 'constraints' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGrantTokens :: Lens.Lens' CreateGrant (Core.Maybe [Types.GrantTokenType])
cgGrantTokens = Lens.field @"grantTokens"
{-# DEPRECATED cgGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | A friendly name for identifying the grant. Use this value to prevent the unintended creation of duplicate grants when retrying this request.
--
-- When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request.
-- When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' CreateGrant (Core.Maybe Types.Name)
cgName = Lens.field @"name"
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The principal that is given permission to retire the grant by using 'RetireGrant' operation.
--
-- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- /Note:/ Consider using 'retiringPrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRetiringPrincipal :: Lens.Lens' CreateGrant (Core.Maybe Types.PrincipalIdType)
cgRetiringPrincipal = Lens.field @"retiringPrincipal"
{-# DEPRECATED cgRetiringPrincipal "Use generic-lens or generic-optics with 'retiringPrincipal' instead." #-}

instance Core.FromJSON CreateGrant where
  toJSON CreateGrant {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            Core.Just ("GranteePrincipal" Core..= granteePrincipal),
            Core.Just ("Operations" Core..= operations),
            ("Constraints" Core..=) Core.<$> constraints,
            ("GrantTokens" Core..=) Core.<$> grantTokens,
            ("Name" Core..=) Core.<$> name,
            ("RetiringPrincipal" Core..=) Core.<$> retiringPrincipal
          ]
      )

instance Core.AWSRequest CreateGrant where
  type Rs CreateGrant = CreateGrantResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.CreateGrant")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGrantResponse'
            Core.<$> (x Core..:? "GrantId")
            Core.<*> (x Core..:? "GrantToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGrantResponse' smart constructor.
data CreateGrantResponse = CreateGrantResponse'
  { -- | The unique identifier for the grant.
    --
    -- You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
    grantId :: Core.Maybe Types.GrantId,
    -- | The grant token.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantToken :: Core.Maybe Types.GrantTokenType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGrantResponse' value with any optional fields omitted.
mkCreateGrantResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGrantResponse
mkCreateGrantResponse responseStatus =
  CreateGrantResponse'
    { grantId = Core.Nothing,
      grantToken = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for the grant.
--
-- You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsGrantId :: Lens.Lens' CreateGrantResponse (Core.Maybe Types.GrantId)
cgrrsGrantId = Lens.field @"grantId"
{-# DEPRECATED cgrrsGrantId "Use generic-lens or generic-optics with 'grantId' instead." #-}

-- | The grant token.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsGrantToken :: Lens.Lens' CreateGrantResponse (Core.Maybe Types.GrantTokenType)
cgrrsGrantToken = Lens.field @"grantToken"
{-# DEPRECATED cgrrsGrantToken "Use generic-lens or generic-optics with 'grantToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsResponseStatus :: Lens.Lens' CreateGrantResponse Core.Int
cgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

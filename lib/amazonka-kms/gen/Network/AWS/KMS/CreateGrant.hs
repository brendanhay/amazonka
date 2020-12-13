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
    cgRetiringPrincipal,
    cgGrantTokens,
    cgConstraints,
    cgGranteePrincipal,
    cgName,
    cgOperations,

    -- * Destructuring the response
    CreateGrantResponse (..),
    mkCreateGrantResponse,

    -- ** Response lenses
    cgrsGrantId,
    cgrsGrantToken,
    cgrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

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
    keyId :: Lude.Text,
    -- | The principal that is given permission to retire the grant by using 'RetireGrant' operation.
    --
    -- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
    retiringPrincipal :: Lude.Maybe Lude.Text,
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Lude.Maybe [Lude.Text],
    -- | Allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the encryption context matches or includes the encryption context specified in this structure. For more information about encryption context, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /\/AWS Key Management Service Developer Guide\/ / .
    constraints :: Lude.Maybe GrantConstraints,
    -- | The principal that is given permission to perform the operations that the grant permits.
    --
    -- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
    granteePrincipal :: Lude.Text,
    -- | A friendly name for identifying the grant. Use this value to prevent the unintended creation of duplicate grants when retrying this request.
    --
    -- When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request.
    -- When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
    name :: Lude.Maybe Lude.Text,
    -- | A list of operations that the grant permits.
    operations :: [GrantOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGrant' with the minimum fields required to make a request.
--
-- * 'keyId' - The unique identifier for the customer master key (CMK) that the grant applies to.
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
-- * 'retiringPrincipal' - The principal that is given permission to retire the grant by using 'RetireGrant' operation.
--
-- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'constraints' - Allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the encryption context matches or includes the encryption context specified in this structure. For more information about encryption context, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /\/AWS Key Management Service Developer Guide\/ / .
-- * 'granteePrincipal' - The principal that is given permission to perform the operations that the grant permits.
--
-- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
-- * 'name' - A friendly name for identifying the grant. Use this value to prevent the unintended creation of duplicate grants when retrying this request.
--
-- When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request.
-- When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
-- * 'operations' - A list of operations that the grant permits.
mkCreateGrant ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'granteePrincipal'
  Lude.Text ->
  CreateGrant
mkCreateGrant pKeyId_ pGranteePrincipal_ =
  CreateGrant'
    { keyId = pKeyId_,
      retiringPrincipal = Lude.Nothing,
      grantTokens = Lude.Nothing,
      constraints = Lude.Nothing,
      granteePrincipal = pGranteePrincipal_,
      name = Lude.Nothing,
      operations = Lude.mempty
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
cgKeyId :: Lens.Lens' CreateGrant Lude.Text
cgKeyId = Lens.lens (keyId :: CreateGrant -> Lude.Text) (\s a -> s {keyId = a} :: CreateGrant)
{-# DEPRECATED cgKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The principal that is given permission to retire the grant by using 'RetireGrant' operation.
--
-- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- /Note:/ Consider using 'retiringPrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRetiringPrincipal :: Lens.Lens' CreateGrant (Lude.Maybe Lude.Text)
cgRetiringPrincipal = Lens.lens (retiringPrincipal :: CreateGrant -> Lude.Maybe Lude.Text) (\s a -> s {retiringPrincipal = a} :: CreateGrant)
{-# DEPRECATED cgRetiringPrincipal "Use generic-lens or generic-optics with 'retiringPrincipal' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGrantTokens :: Lens.Lens' CreateGrant (Lude.Maybe [Lude.Text])
cgGrantTokens = Lens.lens (grantTokens :: CreateGrant -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: CreateGrant)
{-# DEPRECATED cgGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the encryption context matches or includes the encryption context specified in this structure. For more information about encryption context, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /\/AWS Key Management Service Developer Guide\/ / .
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgConstraints :: Lens.Lens' CreateGrant (Lude.Maybe GrantConstraints)
cgConstraints = Lens.lens (constraints :: CreateGrant -> Lude.Maybe GrantConstraints) (\s a -> s {constraints = a} :: CreateGrant)
{-# DEPRECATED cgConstraints "Use generic-lens or generic-optics with 'constraints' instead." #-}

-- | The principal that is given permission to perform the operations that the grant permits.
--
-- To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- /Note:/ Consider using 'granteePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGranteePrincipal :: Lens.Lens' CreateGrant Lude.Text
cgGranteePrincipal = Lens.lens (granteePrincipal :: CreateGrant -> Lude.Text) (\s a -> s {granteePrincipal = a} :: CreateGrant)
{-# DEPRECATED cgGranteePrincipal "Use generic-lens or generic-optics with 'granteePrincipal' instead." #-}

-- | A friendly name for identifying the grant. Use this value to prevent the unintended creation of duplicate grants when retrying this request.
--
-- When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request.
-- When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' CreateGrant (Lude.Maybe Lude.Text)
cgName = Lens.lens (name :: CreateGrant -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateGrant)
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of operations that the grant permits.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgOperations :: Lens.Lens' CreateGrant [GrantOperation]
cgOperations = Lens.lens (operations :: CreateGrant -> [GrantOperation]) (\s a -> s {operations = a} :: CreateGrant)
{-# DEPRECATED cgOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

instance Lude.AWSRequest CreateGrant where
  type Rs CreateGrant = CreateGrantResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGrantResponse'
            Lude.<$> (x Lude..?> "GrantId")
            Lude.<*> (x Lude..?> "GrantToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGrant where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.CreateGrant" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGrant where
  toJSON CreateGrant' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            ("RetiringPrincipal" Lude..=) Lude.<$> retiringPrincipal,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            ("Constraints" Lude..=) Lude.<$> constraints,
            Lude.Just ("GranteePrincipal" Lude..= granteePrincipal),
            ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("Operations" Lude..= operations)
          ]
      )

instance Lude.ToPath CreateGrant where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGrant where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGrantResponse' smart constructor.
data CreateGrantResponse = CreateGrantResponse'
  { -- | The unique identifier for the grant.
    --
    -- You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
    grantId :: Lude.Maybe Lude.Text,
    -- | The grant token.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGrantResponse' with the minimum fields required to make a request.
--
-- * 'grantId' - The unique identifier for the grant.
--
-- You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
-- * 'grantToken' - The grant token.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'responseStatus' - The response status code.
mkCreateGrantResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGrantResponse
mkCreateGrantResponse pResponseStatus_ =
  CreateGrantResponse'
    { grantId = Lude.Nothing,
      grantToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for the grant.
--
-- You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGrantId :: Lens.Lens' CreateGrantResponse (Lude.Maybe Lude.Text)
cgrsGrantId = Lens.lens (grantId :: CreateGrantResponse -> Lude.Maybe Lude.Text) (\s a -> s {grantId = a} :: CreateGrantResponse)
{-# DEPRECATED cgrsGrantId "Use generic-lens or generic-optics with 'grantId' instead." #-}

-- | The grant token.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGrantToken :: Lens.Lens' CreateGrantResponse (Lude.Maybe Lude.Text)
cgrsGrantToken = Lens.lens (grantToken :: CreateGrantResponse -> Lude.Maybe Lude.Text) (\s a -> s {grantToken = a} :: CreateGrantResponse)
{-# DEPRECATED cgrsGrantToken "Use generic-lens or generic-optics with 'grantToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResponseStatus :: Lens.Lens' CreateGrantResponse Lude.Int
cgrsResponseStatus = Lens.lens (responseStatus :: CreateGrantResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGrantResponse)
{-# DEPRECATED cgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

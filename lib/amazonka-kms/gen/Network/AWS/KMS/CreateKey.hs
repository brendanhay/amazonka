{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CreateKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a unique customer managed <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master-keys customer master key> (CMK) in your AWS account and Region. You cannot use this operation to create a CMK in a different AWS account.
--
-- You can use the @CreateKey@ operation to create symmetric or asymmetric CMKs.
--
--     * __Symmetric CMKs__ contain a 256-bit symmetric key that never leaves AWS KMS unencrypted. To use the CMK, you must call AWS KMS. You can use a symmetric CMK to encrypt and decrypt small amounts of data, but they are typically used to generate <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data keys> and <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-key-pairs data keys pairs> . For details, see 'GenerateDataKey' and 'GenerateDataKeyPair' .
--
--
--     * __Asymmetric CMKs__ can contain an RSA key pair or an Elliptic Curve (ECC) key pair. The private key in an asymmetric CMK never leaves AWS KMS unencrypted. However, you can use the 'GetPublicKey' operation to download the public key so it can be used outside of AWS KMS. CMKs with RSA key pairs can be used to encrypt or decrypt data or sign and verify messages (but not both). CMKs with ECC key pairs can be used only to sign and verify messages.
--
--
-- For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
-- To create different types of CMKs, use the following guidance:
--
--     * Asymmetric CMKs
--
--     * To create an asymmetric CMK, use the @CustomerMasterKeySpec@ parameter to specify the type of key material in the CMK. Then, use the @KeyUsage@ parameter to determine whether the CMK will be used to encrypt and decrypt or sign and verify. You can't change these properties after the CMK is created.
--
--
--
--     * Symmetric CMKs
--
--     * When creating a symmetric CMK, you don't need to specify the @CustomerMasterKeySpec@ or @KeyUsage@ parameters. The default value for @CustomerMasterKeySpec@ , @SYMMETRIC_DEFAULT@ , and the default value for @KeyUsage@ , @ENCRYPT_DECRYPT@ , are the only valid values for symmetric CMKs.
--
--
--
--     * Imported Key Material
--
--     * To import your own key material, begin by creating a symmetric CMK with no key material. To do this, use the @Origin@ parameter of @CreateKey@ with a value of @EXTERNAL@ . Next, use 'GetParametersForImport' operation to get a public key and import token, and use the public key to encrypt your key material. Then, use 'ImportKeyMaterial' with your import token to import the key material. For step-by-step instructions, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /\/AWS Key Management Service Developer Guide\/ / . You cannot import the key material into an asymmetric CMK.
--
--
--
--     * Custom Key Stores
--
--     * To create a symmetric CMK in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , use the @CustomKeyStoreId@ parameter to specify the custom key store. You must also use the @Origin@ parameter with a value of @AWS_CLOUDHSM@ . The AWS CloudHSM cluster that is associated with the custom key store must have at least two active HSMs in different Availability Zones in the AWS Region.
-- You cannot create an asymmetric CMK in a custom key store. For information about custom key stores in AWS KMS see <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Using Custom Key Stores> in the /\/AWS Key Management Service Developer Guide\/ / .
module Network.AWS.KMS.CreateKey
  ( -- * Creating a request
    CreateKey (..),
    mkCreateKey,

    -- ** Request lenses
    ckOrigin,
    ckCustomerMasterKeySpec,
    ckKeyUsage,
    ckBypassPolicyLockoutSafetyCheck,
    ckPolicy,
    ckDescription,
    ckCustomKeyStoreId,
    ckTags,

    -- * Destructuring the response
    CreateKeyResponse (..),
    mkCreateKeyResponse,

    -- ** Response lenses
    ckrsKeyMetadata,
    ckrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateKey' smart constructor.
data CreateKey = CreateKey'
  { origin :: Lude.Maybe OriginType,
    customerMasterKeySpec :: Lude.Maybe CustomerMasterKeySpec,
    keyUsage :: Lude.Maybe KeyUsageType,
    bypassPolicyLockoutSafetyCheck :: Lude.Maybe Lude.Bool,
    policy :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    customKeyStoreId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateKey' with the minimum fields required to make a request.
--
-- * 'bypassPolicyLockoutSafetyCheck' - A flag to indicate whether to bypass the key policy lockout safety check.
--
-- /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately.
-- For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /\/AWS Key Management Service Developer Guide\/ / .
-- Use this parameter only when you include a policy in the request and you intend to prevent the principal that is making the request from making a subsequent 'PutKeyPolicy' request on the CMK.
-- The default value is false.
-- * 'customKeyStoreId' - Creates the CMK in the specified <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> and the key material in its associated AWS CloudHSM cluster. To create a CMK in a custom key store, you must also specify the @Origin@ parameter with a value of @AWS_CLOUDHSM@ . The AWS CloudHSM cluster that is associated with the custom key store must have at least two active HSMs, each in a different Availability Zone in the Region.
--
-- This parameter is valid only for symmetric CMKs. You cannot create an asymmetric CMK in a custom key store.
-- To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
-- The response includes the custom key store ID and the ID of the AWS CloudHSM cluster.
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
-- * 'customerMasterKeySpec' - Specifies the type of CMK to create. The default value, @SYMMETRIC_DEFAULT@ , creates a CMK with a 256-bit symmetric key for encryption and decryption. For help choosing a key spec for your CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-choose.html How to Choose Your CMK Configuration> in the /AWS Key Management Service Developer Guide/ .
--
-- The @CustomerMasterKeySpec@ determines whether the CMK contains a symmetric key or an asymmetric key pair. It also determines the encryption algorithms or signing algorithms that the CMK supports. You can't change the @CustomerMasterKeySpec@ after the CMK is created. To further restrict the algorithms that can be used with the CMK, use a condition key in its key policy or IAM policy. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-algorithm kms:EncryptionAlgorithm> or <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-signing-algorithm kms:Signing Algorithm> in the /AWS Key Management Service Developer Guide/ .
-- /Important:/ <http://aws.amazon.com/kms/features/#AWS_Service_Integration AWS services that are integrated with AWS KMS> use symmetric CMKs to protect your data. These services do not support asymmetric CMKs. For help determining whether a CMK is symmetric or asymmetric, see <https://docs.aws.amazon.com/kms/latest/developerguide/find-symm-asymm.html Identifying Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
-- AWS KMS supports the following key specs for CMKs:
--
--     * Symmetric key (default)
--
--     * @SYMMETRIC_DEFAULT@ (AES-256-GCM)
--
--
--
--
--     * Asymmetric RSA key pairs
--
--     * @RSA_2048@
--
--
--     * @RSA_3072@
--
--
--     * @RSA_4096@
--
--
--
--
--     * Asymmetric NIST-recommended elliptic curve key pairs
--
--     * @ECC_NIST_P256@ (secp256r1)
--
--
--     * @ECC_NIST_P384@ (secp384r1)
--
--
--     * @ECC_NIST_P521@ (secp521r1)
--
--
--
--
--     * Other asymmetric elliptic curve key pairs
--
--     * @ECC_SECG_P256K1@ (secp256k1), commonly used for cryptocurrencies.
--
--
--
--
-- * 'description' - A description of the CMK.
--
-- Use a description that helps you decide whether the CMK is appropriate for a task.
-- * 'keyUsage' - Determines the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> for which you can use the CMK. The default value is @ENCRYPT_DECRYPT@ . This parameter is required only for asymmetric CMKs. You can't change the @KeyUsage@ value after the CMK is created.
--
-- Select only one valid value.
--
--     * For symmetric CMKs, omit the parameter or specify @ENCRYPT_DECRYPT@ .
--
--
--     * For asymmetric CMKs with RSA key material, specify @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ .
--
--
--     * For asymmetric CMKs with ECC key material, specify @SIGN_VERIFY@ .
--
--
-- * 'origin' - The source of the key material for the CMK. You cannot change the origin after you create the CMK. The default is @AWS_KMS@ , which means AWS KMS creates the key material.
--
-- When the parameter value is @EXTERNAL@ , AWS KMS creates a CMK without key material so that you can import key material from your existing key management infrastructure. For more information about importing key material into AWS KMS, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ . This value is valid only for symmetric CMKs.
-- When the parameter value is @AWS_CLOUDHSM@ , AWS KMS creates the CMK in an AWS KMS <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> and creates its key material in the associated AWS CloudHSM cluster. You must also use the @CustomKeyStoreId@ parameter to identify the custom key store. This value is valid only for symmetric CMKs.
-- * 'policy' - The key policy to attach to the CMK.
--
-- If you provide a key policy, it must meet the following criteria:
--
--     * If you don't set @BypassPolicyLockoutSafetyCheck@ to true, the key policy must allow the principal that is making the @CreateKey@ request to make a subsequent 'PutKeyPolicy' request on the CMK. This reduces the risk that the CMK becomes unmanageable. For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section of the /\/AWS Key Management Service Developer Guide\/ / .
--
--
--     * Each statement in the key policy must contain one or more principals. The principals in the key policy must exist and be visible to AWS KMS. When you create a new AWS principal (for example, an IAM user or role), you might need to enforce a delay before including the new principal in a key policy because the new principal might not be immediately visible to AWS KMS. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible> in the /AWS Identity and Access Management User Guide/ .
--
--
-- If you do not provide a key policy, AWS KMS attaches a default key policy to the CMK. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default Key Policy> in the /AWS Key Management Service Developer Guide/ .
-- The key policy size quota is 32 kilobytes (32768 bytes).
-- * 'tags' - One or more tags. Each tag consists of a tag key and a tag value. Both the tag key and the tag value are required, but the tag value can be an empty (null) string.
--
-- When you add tags to an AWS resource, AWS generates a cost allocation report with usage and costs aggregated by tags. For information about adding, changing, deleting and listing tags for CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging Keys> .
-- Use this parameter to tag the CMK when it is created. To add tags to an existing CMK, use the 'TagResource' operation.
mkCreateKey ::
  CreateKey
mkCreateKey =
  CreateKey'
    { origin = Lude.Nothing,
      customerMasterKeySpec = Lude.Nothing,
      keyUsage = Lude.Nothing,
      bypassPolicyLockoutSafetyCheck = Lude.Nothing,
      policy = Lude.Nothing,
      description = Lude.Nothing,
      customKeyStoreId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The source of the key material for the CMK. You cannot change the origin after you create the CMK. The default is @AWS_KMS@ , which means AWS KMS creates the key material.
--
-- When the parameter value is @EXTERNAL@ , AWS KMS creates a CMK without key material so that you can import key material from your existing key management infrastructure. For more information about importing key material into AWS KMS, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ . This value is valid only for symmetric CMKs.
-- When the parameter value is @AWS_CLOUDHSM@ , AWS KMS creates the CMK in an AWS KMS <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> and creates its key material in the associated AWS CloudHSM cluster. You must also use the @CustomKeyStoreId@ parameter to identify the custom key store. This value is valid only for symmetric CMKs.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckOrigin :: Lens.Lens' CreateKey (Lude.Maybe OriginType)
ckOrigin = Lens.lens (origin :: CreateKey -> Lude.Maybe OriginType) (\s a -> s {origin = a} :: CreateKey)
{-# DEPRECATED ckOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | Specifies the type of CMK to create. The default value, @SYMMETRIC_DEFAULT@ , creates a CMK with a 256-bit symmetric key for encryption and decryption. For help choosing a key spec for your CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-choose.html How to Choose Your CMK Configuration> in the /AWS Key Management Service Developer Guide/ .
--
-- The @CustomerMasterKeySpec@ determines whether the CMK contains a symmetric key or an asymmetric key pair. It also determines the encryption algorithms or signing algorithms that the CMK supports. You can't change the @CustomerMasterKeySpec@ after the CMK is created. To further restrict the algorithms that can be used with the CMK, use a condition key in its key policy or IAM policy. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-algorithm kms:EncryptionAlgorithm> or <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-signing-algorithm kms:Signing Algorithm> in the /AWS Key Management Service Developer Guide/ .
-- /Important:/ <http://aws.amazon.com/kms/features/#AWS_Service_Integration AWS services that are integrated with AWS KMS> use symmetric CMKs to protect your data. These services do not support asymmetric CMKs. For help determining whether a CMK is symmetric or asymmetric, see <https://docs.aws.amazon.com/kms/latest/developerguide/find-symm-asymm.html Identifying Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
-- AWS KMS supports the following key specs for CMKs:
--
--     * Symmetric key (default)
--
--     * @SYMMETRIC_DEFAULT@ (AES-256-GCM)
--
--
--
--
--     * Asymmetric RSA key pairs
--
--     * @RSA_2048@
--
--
--     * @RSA_3072@
--
--
--     * @RSA_4096@
--
--
--
--
--     * Asymmetric NIST-recommended elliptic curve key pairs
--
--     * @ECC_NIST_P256@ (secp256r1)
--
--
--     * @ECC_NIST_P384@ (secp384r1)
--
--
--     * @ECC_NIST_P521@ (secp521r1)
--
--
--
--
--     * Other asymmetric elliptic curve key pairs
--
--     * @ECC_SECG_P256K1@ (secp256k1), commonly used for cryptocurrencies.
--
--
--
--
--
-- /Note:/ Consider using 'customerMasterKeySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckCustomerMasterKeySpec :: Lens.Lens' CreateKey (Lude.Maybe CustomerMasterKeySpec)
ckCustomerMasterKeySpec = Lens.lens (customerMasterKeySpec :: CreateKey -> Lude.Maybe CustomerMasterKeySpec) (\s a -> s {customerMasterKeySpec = a} :: CreateKey)
{-# DEPRECATED ckCustomerMasterKeySpec "Use generic-lens or generic-optics with 'customerMasterKeySpec' instead." #-}

-- | Determines the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> for which you can use the CMK. The default value is @ENCRYPT_DECRYPT@ . This parameter is required only for asymmetric CMKs. You can't change the @KeyUsage@ value after the CMK is created.
--
-- Select only one valid value.
--
--     * For symmetric CMKs, omit the parameter or specify @ENCRYPT_DECRYPT@ .
--
--
--     * For asymmetric CMKs with RSA key material, specify @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ .
--
--
--     * For asymmetric CMKs with ECC key material, specify @SIGN_VERIFY@ .
--
--
--
-- /Note:/ Consider using 'keyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckKeyUsage :: Lens.Lens' CreateKey (Lude.Maybe KeyUsageType)
ckKeyUsage = Lens.lens (keyUsage :: CreateKey -> Lude.Maybe KeyUsageType) (\s a -> s {keyUsage = a} :: CreateKey)
{-# DEPRECATED ckKeyUsage "Use generic-lens or generic-optics with 'keyUsage' instead." #-}

-- | A flag to indicate whether to bypass the key policy lockout safety check.
--
-- /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately.
-- For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /\/AWS Key Management Service Developer Guide\/ / .
-- Use this parameter only when you include a policy in the request and you intend to prevent the principal that is making the request from making a subsequent 'PutKeyPolicy' request on the CMK.
-- The default value is false.
--
-- /Note:/ Consider using 'bypassPolicyLockoutSafetyCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckBypassPolicyLockoutSafetyCheck :: Lens.Lens' CreateKey (Lude.Maybe Lude.Bool)
ckBypassPolicyLockoutSafetyCheck = Lens.lens (bypassPolicyLockoutSafetyCheck :: CreateKey -> Lude.Maybe Lude.Bool) (\s a -> s {bypassPolicyLockoutSafetyCheck = a} :: CreateKey)
{-# DEPRECATED ckBypassPolicyLockoutSafetyCheck "Use generic-lens or generic-optics with 'bypassPolicyLockoutSafetyCheck' instead." #-}

-- | The key policy to attach to the CMK.
--
-- If you provide a key policy, it must meet the following criteria:
--
--     * If you don't set @BypassPolicyLockoutSafetyCheck@ to true, the key policy must allow the principal that is making the @CreateKey@ request to make a subsequent 'PutKeyPolicy' request on the CMK. This reduces the risk that the CMK becomes unmanageable. For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section of the /\/AWS Key Management Service Developer Guide\/ / .
--
--
--     * Each statement in the key policy must contain one or more principals. The principals in the key policy must exist and be visible to AWS KMS. When you create a new AWS principal (for example, an IAM user or role), you might need to enforce a delay before including the new principal in a key policy because the new principal might not be immediately visible to AWS KMS. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible> in the /AWS Identity and Access Management User Guide/ .
--
--
-- If you do not provide a key policy, AWS KMS attaches a default key policy to the CMK. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default Key Policy> in the /AWS Key Management Service Developer Guide/ .
-- The key policy size quota is 32 kilobytes (32768 bytes).
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckPolicy :: Lens.Lens' CreateKey (Lude.Maybe Lude.Text)
ckPolicy = Lens.lens (policy :: CreateKey -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: CreateKey)
{-# DEPRECATED ckPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A description of the CMK.
--
-- Use a description that helps you decide whether the CMK is appropriate for a task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckDescription :: Lens.Lens' CreateKey (Lude.Maybe Lude.Text)
ckDescription = Lens.lens (description :: CreateKey -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateKey)
{-# DEPRECATED ckDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Creates the CMK in the specified <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> and the key material in its associated AWS CloudHSM cluster. To create a CMK in a custom key store, you must also specify the @Origin@ parameter with a value of @AWS_CLOUDHSM@ . The AWS CloudHSM cluster that is associated with the custom key store must have at least two active HSMs, each in a different Availability Zone in the Region.
--
-- This parameter is valid only for symmetric CMKs. You cannot create an asymmetric CMK in a custom key store.
-- To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
-- The response includes the custom key store ID and the ID of the AWS CloudHSM cluster.
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckCustomKeyStoreId :: Lens.Lens' CreateKey (Lude.Maybe Lude.Text)
ckCustomKeyStoreId = Lens.lens (customKeyStoreId :: CreateKey -> Lude.Maybe Lude.Text) (\s a -> s {customKeyStoreId = a} :: CreateKey)
{-# DEPRECATED ckCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

-- | One or more tags. Each tag consists of a tag key and a tag value. Both the tag key and the tag value are required, but the tag value can be an empty (null) string.
--
-- When you add tags to an AWS resource, AWS generates a cost allocation report with usage and costs aggregated by tags. For information about adding, changing, deleting and listing tags for CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging Keys> .
-- Use this parameter to tag the CMK when it is created. To add tags to an existing CMK, use the 'TagResource' operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckTags :: Lens.Lens' CreateKey (Lude.Maybe [Tag])
ckTags = Lens.lens (tags :: CreateKey -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateKey)
{-# DEPRECATED ckTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateKey where
  type Rs CreateKey = CreateKeyResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateKeyResponse'
            Lude.<$> (x Lude..?> "KeyMetadata") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.CreateKey" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateKey where
  toJSON CreateKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Origin" Lude..=) Lude.<$> origin,
            ("CustomerMasterKeySpec" Lude..=) Lude.<$> customerMasterKeySpec,
            ("KeyUsage" Lude..=) Lude.<$> keyUsage,
            ("BypassPolicyLockoutSafetyCheck" Lude..=)
              Lude.<$> bypassPolicyLockoutSafetyCheck,
            ("Policy" Lude..=) Lude.<$> policy,
            ("Description" Lude..=) Lude.<$> description,
            ("CustomKeyStoreId" Lude..=) Lude.<$> customKeyStoreId,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateKey where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateKeyResponse' smart constructor.
data CreateKeyResponse = CreateKeyResponse'
  { keyMetadata ::
      Lude.Maybe KeyMetadata,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateKeyResponse' with the minimum fields required to make a request.
--
-- * 'keyMetadata' - Metadata associated with the CMK.
-- * 'responseStatus' - The response status code.
mkCreateKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateKeyResponse
mkCreateKeyResponse pResponseStatus_ =
  CreateKeyResponse'
    { keyMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metadata associated with the CMK.
--
-- /Note:/ Consider using 'keyMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckrsKeyMetadata :: Lens.Lens' CreateKeyResponse (Lude.Maybe KeyMetadata)
ckrsKeyMetadata = Lens.lens (keyMetadata :: CreateKeyResponse -> Lude.Maybe KeyMetadata) (\s a -> s {keyMetadata = a} :: CreateKeyResponse)
{-# DEPRECATED ckrsKeyMetadata "Use generic-lens or generic-optics with 'keyMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckrsResponseStatus :: Lens.Lens' CreateKeyResponse Lude.Int
ckrsResponseStatus = Lens.lens (responseStatus :: CreateKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateKeyResponse)
{-# DEPRECATED ckrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

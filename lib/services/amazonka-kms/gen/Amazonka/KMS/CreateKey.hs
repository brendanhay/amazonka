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
-- Module      : Amazonka.KMS.CreateKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a unique customer managed
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#kms-keys KMS key>
-- in your Amazon Web Services account and Region.
--
-- In addition to the required parameters, you can use the optional
-- parameters to specify a key policy, description, tags, and other useful
-- elements for any key type.
--
-- KMS is replacing the term /customer master key (CMK)/ with /KMS key/ and
-- /KMS key/. The concept has not changed. To prevent breaking changes, KMS
-- is keeping some variations of this term.
--
-- To create different types of KMS keys, use the following guidance:
--
-- [Symmetric encryption KMS key]
--     To create a symmetric encryption KMS key, you aren\'t required to
--     specify any parameters. The default value for @KeySpec@,
--     @SYMMETRIC_DEFAULT@, and the default value for @KeyUsage@,
--     @ENCRYPT_DECRYPT@, create a symmetric encryption KMS key. For
--     technical details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/asymmetric-key-specs.html#key-spec-symmetric-default SYMMETRIC_DEFAULT key spec>
--     in the /Key Management Service Developer Guide/.
--
--     If you need a key for basic encryption and decryption or you are
--     creating a KMS key to protect your resources in an Amazon Web
--     Services service, create a symmetric encryption KMS key. The key
--     material in a symmetric encryption key never leaves KMS unencrypted.
--     You can use a symmetric encryption KMS key to encrypt and decrypt
--     data up to 4,096 bytes, but they are typically used to generate data
--     keys and data keys pairs. For details, see GenerateDataKey and
--     GenerateDataKeyPair.
--
-- [Asymmetric KMS keys]
--     To create an asymmetric KMS key, use the @KeySpec@ parameter to
--     specify the type of key material in the KMS key. Then, use the
--     @KeyUsage@ parameter to determine whether the KMS key will be used
--     to encrypt and decrypt or sign and verify. You can\'t change these
--     properties after the KMS key is created.
--
--     Asymmetric KMS keys contain an RSA key pair, Elliptic Curve (ECC)
--     key pair, or an SM2 key pair (China Regions only). The private key
--     in an asymmetric KMS key never leaves KMS unencrypted. However, you
--     can use the GetPublicKey operation to download the public key so it
--     can be used outside of KMS. KMS keys with RSA or SM2 key pairs can
--     be used to encrypt or decrypt data or sign and verify messages (but
--     not both). KMS keys with ECC key pairs can be used only to sign and
--     verify messages. For information about asymmetric KMS keys, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Asymmetric KMS keys>
--     in the /Key Management Service Developer Guide/.
--
-- [HMAC KMS key]
--     To create an HMAC KMS key, set the @KeySpec@ parameter to a key spec
--     value for HMAC KMS keys. Then set the @KeyUsage@ parameter to
--     @GENERATE_VERIFY_MAC@. You must set the key usage even though
--     @GENERATE_VERIFY_MAC@ is the only valid key usage value for HMAC KMS
--     keys. You can\'t change these properties after the KMS key is
--     created.
--
--     HMAC KMS keys are symmetric keys that never leave KMS unencrypted.
--     You can use HMAC keys to generate (GenerateMac) and verify
--     (VerifyMac) HMAC codes for messages up to 4096 bytes.
--
--     HMAC KMS keys are not supported in all Amazon Web Services Regions.
--     If you try to create an HMAC KMS key in an Amazon Web Services
--     Region in which HMAC keys are not supported, the @CreateKey@
--     operation returns an @UnsupportedOperationException@. For a list of
--     Regions in which HMAC KMS keys are supported, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC keys in KMS>
--     in the /Key Management Service Developer Guide/.
--
-- [Multi-Region primary keys
-- Imported key material]
--     To create a multi-Region /primary key/ in the local Amazon Web
--     Services Region, use the @MultiRegion@ parameter with a value of
--     @True@. To create a multi-Region /replica key/, that is, a KMS key
--     with the same key ID and key material as a primary key, but in a
--     different Amazon Web Services Region, use the ReplicateKey
--     operation. To change a replica key to a primary key, and its primary
--     key to a replica key, use the UpdatePrimaryRegion operation.
--
--     You can create multi-Region KMS keys for all supported KMS key
--     types: symmetric encryption KMS keys, HMAC KMS keys, asymmetric
--     encryption KMS keys, and asymmetric signing KMS keys. You can also
--     create multi-Region keys with imported key material. However, you
--     can\'t create multi-Region keys in a custom key store.
--
--     This operation supports /multi-Region keys/, an KMS feature that
--     lets you create multiple interoperable KMS keys in different Amazon
--     Web Services Regions. Because these KMS keys have the same key ID,
--     key material, and other metadata, you can use them interchangeably
--     to encrypt data in one Amazon Web Services Region and decrypt it in
--     a different Amazon Web Services Region without re-encrypting the
--     data or making a cross-Region call. For more information about
--     multi-Region keys, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
--     in the /Key Management Service Developer Guide/.
--
--     To import your own key material, begin by creating a symmetric
--     encryption KMS key with no key material. To do this, use the
--     @Origin@ parameter of @CreateKey@ with a value of @EXTERNAL@. Next,
--     use GetParametersForImport operation to get a public key and import
--     token, and use the public key to encrypt your key material. Then,
--     use ImportKeyMaterial with your import token to import the key
--     material. For step-by-step instructions, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
--     in the //Key Management Service Developer Guide// .
--
--     This feature supports only symmetric encryption KMS keys, including
--     multi-Region symmetric encryption KMS keys. You cannot import key
--     material into any other type of KMS key.
--
--     To create a multi-Region primary key with imported key material, use
--     the @Origin@ parameter of @CreateKey@ with a value of @EXTERNAL@ and
--     the @MultiRegion@ parameter with a value of @True@. To create
--     replicas of the multi-Region primary key, use the ReplicateKey
--     operation. For more information about multi-Region keys, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
--     in the /Key Management Service Developer Guide/.
--
-- [Custom key store]
--     To create a symmetric encryption KMS key in a
--     <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
--     use the @CustomKeyStoreId@ parameter to specify the custom key
--     store. You must also use the @Origin@ parameter with a value of
--     @AWS_CLOUDHSM@. The CloudHSM cluster that is associated with the
--     custom key store must have at least two active HSMs in different
--     Availability Zones in the Amazon Web Services Region.
--
--     Custom key stores support only symmetric encryption KMS keys. You
--     cannot create an HMAC KMS key or an asymmetric KMS key in a custom
--     key store. For information about custom key stores in KMS see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom key stores in KMS>
--     in the //Key Management Service Developer Guide// .
--
-- __Cross-account use__: No. You cannot use this operation to create a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateKey>
-- (IAM policy). To use the @Tags@ parameter,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
-- (IAM policy). For examples and information about related permissions,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/iam-policies.html#iam-policy-example-create-key Allow a user to create KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   DescribeKey
--
-- -   ListKeys
--
-- -   ScheduleKeyDeletion
module Amazonka.KMS.CreateKey
  ( -- * Creating a Request
    CreateKey (..),
    newCreateKey,

    -- * Request Lenses
    createKey_tags,
    createKey_policy,
    createKey_customKeyStoreId,
    createKey_customerMasterKeySpec,
    createKey_keyUsage,
    createKey_description,
    createKey_multiRegion,
    createKey_keySpec,
    createKey_bypassPolicyLockoutSafetyCheck,
    createKey_origin,

    -- * Destructuring the Response
    CreateKeyResponse (..),
    newCreateKeyResponse,

    -- * Response Lenses
    createKeyResponse_keyMetadata,
    createKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKey' smart constructor.
data CreateKey = CreateKey'
  { -- | Assigns one or more tags to the KMS key. Use this parameter to tag the
    -- KMS key when it is created. To tag an existing KMS key, use the
    -- TagResource operation.
    --
    -- Tagging or untagging a KMS key can allow or deny permission to the KMS
    -- key. For details, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC in KMS>
    -- in the /Key Management Service Developer Guide/.
    --
    -- To use this parameter, you must have
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
    -- permission in an IAM policy.
    --
    -- Each tag consists of a tag key and a tag value. Both the tag key and the
    -- tag value are required, but the tag value can be an empty (null) string.
    -- You cannot have more than one tag on a KMS key with the same tag key. If
    -- you specify an existing tag key with a different tag value, KMS replaces
    -- the current tag value with the specified one.
    --
    -- When you add tags to an Amazon Web Services resource, Amazon Web
    -- Services generates a cost allocation report with usage and costs
    -- aggregated by tags. Tags can also be used to control access to a KMS
    -- key. For details, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging Keys>.
    tags :: Prelude.Maybe [Tag],
    -- | The key policy to attach to the KMS key. If you do not specify a key
    -- policy, KMS attaches a default key policy to the KMS key. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default key policy>
    -- in the /Key Management Service Developer Guide/.
    --
    -- If you provide a key policy, it must meet the following criteria:
    --
    -- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to @True@, the
    --     key policy must allow the principal that is making the @CreateKey@
    --     request to make a subsequent PutKeyPolicy request on the KMS key.
    --     This reduces the risk that the KMS key becomes unmanageable. For
    --     more information, refer to the scenario in the
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
    --     section of the //Key Management Service Developer Guide// .
    --
    -- -   Each statement in the key policy must contain one or more
    --     principals. The principals in the key policy must exist and be
    --     visible to KMS. When you create a new Amazon Web Services principal
    --     (for example, an IAM user or role), you might need to enforce a
    --     delay before including the new principal in a key policy because the
    --     new principal might not be immediately visible to KMS. For more
    --     information, see
    --     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
    --     in the /Amazon Web Services Identity and Access Management User
    --     Guide/.
    --
    -- A key policy document can include only the following characters:
    --
    -- -   Printable ASCII characters from the space character (@\\u0020@)
    --     through the end of the ASCII character range.
    --
    -- -   Printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@).
    --
    -- -   The tab (@\\u0009@), line feed (@\\u000A@), and carriage return
    --     (@\\u000D@) special characters
    --
    -- For information about key policies, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key policies in KMS>
    -- in the /Key Management Service Developer Guide/. For help writing and
    -- formatting a JSON policy document, see the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
    -- in the //Identity and Access Management User Guide// .
    policy :: Prelude.Maybe Prelude.Text,
    -- | Creates the KMS key in the specified
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
    -- and the key material in its associated CloudHSM cluster. To create a KMS
    -- key in a custom key store, you must also specify the @Origin@ parameter
    -- with a value of @AWS_CLOUDHSM@. The CloudHSM cluster that is associated
    -- with the custom key store must have at least two active HSMs, each in a
    -- different Availability Zone in the Region.
    --
    -- This parameter is valid only for symmetric encryption KMS keys in a
    -- single Region. You cannot create any other type of KMS key in a custom
    -- key store.
    --
    -- To find the ID of a custom key store, use the DescribeCustomKeyStores
    -- operation.
    --
    -- The response includes the custom key store ID and the ID of the CloudHSM
    -- cluster.
    --
    -- This operation is part of the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store feature>
    -- feature in KMS, which combines the convenience and extensive integration
    -- of KMS with the isolation and control of a single-tenant key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | Instead, use the @KeySpec@ parameter.
    --
    -- The @KeySpec@ and @CustomerMasterKeySpec@ parameters work the same way.
    -- Only the names differ. We recommend that you use @KeySpec@ parameter in
    -- your code. However, to avoid breaking changes, KMS will support both
    -- parameters.
    customerMasterKeySpec :: Prelude.Maybe CustomerMasterKeySpec,
    -- | Determines the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
    -- for which you can use the KMS key. The default value is
    -- @ENCRYPT_DECRYPT@. This parameter is optional when you are creating a
    -- symmetric encryption KMS key; otherwise, it is required. You can\'t
    -- change the @KeyUsage@ value after the KMS key is created.
    --
    -- Select only one valid value.
    --
    -- -   For symmetric encryption KMS keys, omit the parameter or specify
    --     @ENCRYPT_DECRYPT@.
    --
    -- -   For HMAC KMS keys (symmetric), specify @GENERATE_VERIFY_MAC@.
    --
    -- -   For asymmetric KMS keys with RSA key material, specify
    --     @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@.
    --
    -- -   For asymmetric KMS keys with ECC key material, specify
    --     @SIGN_VERIFY@.
    --
    -- -   For asymmetric KMS keys with SM2 key material (China Regions only),
    --     specify @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@.
    keyUsage :: Prelude.Maybe KeyUsageType,
    -- | A description of the KMS key.
    --
    -- Use a description that helps you decide whether the KMS key is
    -- appropriate for a task. The default value is an empty string (no
    -- description).
    --
    -- To set or change the description after the key is created, use
    -- UpdateKeyDescription.
    description :: Prelude.Maybe Prelude.Text,
    -- | Creates a multi-Region primary key that you can replicate into other
    -- Amazon Web Services Regions. You cannot change this value after you
    -- create the KMS key.
    --
    -- For a multi-Region key, set this parameter to @True@. For a
    -- single-Region KMS key, omit this parameter or set it to @False@. The
    -- default value is @False@.
    --
    -- This operation supports /multi-Region keys/, an KMS feature that lets
    -- you create multiple interoperable KMS keys in different Amazon Web
    -- Services Regions. Because these KMS keys have the same key ID, key
    -- material, and other metadata, you can use them interchangeably to
    -- encrypt data in one Amazon Web Services Region and decrypt it in a
    -- different Amazon Web Services Region without re-encrypting the data or
    -- making a cross-Region call. For more information about multi-Region
    -- keys, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
    -- in the /Key Management Service Developer Guide/.
    --
    -- This value creates a /primary key/, not a replica. To create a /replica
    -- key/, use the ReplicateKey operation.
    --
    -- You can create a multi-Region version of a symmetric encryption KMS key,
    -- an HMAC KMS key, an asymmetric KMS key, or a KMS key with imported key
    -- material. However, you cannot create a multi-Region key in a custom key
    -- store.
    multiRegion :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the type of KMS key to create. The default value,
    -- @SYMMETRIC_DEFAULT@, creates a KMS key with a 256-bit AES-GCM key that
    -- is used for encryption and decryption, except in China Regions, where it
    -- creates a 128-bit symmetric key that uses SM4 encryption. For help
    -- choosing a key spec for your KMS key, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-types.html#symm-asymm-choose Choosing a KMS key type>
    -- in the //Key Management Service Developer Guide// .
    --
    -- The @KeySpec@ determines whether the KMS key contains a symmetric key or
    -- an asymmetric key pair. It also determines the cryptographic algorithms
    -- that the KMS key supports. You can\'t change the @KeySpec@ after the KMS
    -- key is created. To further restrict the algorithms that can be used with
    -- the KMS key, use a condition key in its key policy or IAM policy. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-algorithm kms:EncryptionAlgorithm>,
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-mac-algorithm kms:MacAlgorithm>
    -- or
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-signing-algorithm kms:Signing Algorithm>
    -- in the //Key Management Service Developer Guide// .
    --
    -- <http://aws.amazon.com/kms/features/#AWS_Service_Integration Amazon Web Services services that are integrated with KMS>
    -- use symmetric encryption KMS keys to protect your data. These services
    -- do not support asymmetric KMS keys or HMAC KMS keys.
    --
    -- KMS supports the following key specs for KMS keys:
    --
    -- -   Symmetric encryption key (default)
    --
    --     -   @SYMMETRIC_DEFAULT@
    --
    -- -   HMAC keys (symmetric)
    --
    --     -   @HMAC_224@
    --
    --     -   @HMAC_256@
    --
    --     -   @HMAC_384@
    --
    --     -   @HMAC_512@
    --
    -- -   Asymmetric RSA key pairs
    --
    --     -   @RSA_2048@
    --
    --     -   @RSA_3072@
    --
    --     -   @RSA_4096@
    --
    -- -   Asymmetric NIST-recommended elliptic curve key pairs
    --
    --     -   @ECC_NIST_P256@ (secp256r1)
    --
    --     -   @ECC_NIST_P384@ (secp384r1)
    --
    --     -   @ECC_NIST_P521@ (secp521r1)
    --
    -- -   Other asymmetric elliptic curve key pairs
    --
    --     -   @ECC_SECG_P256K1@ (secp256k1), commonly used for
    --         cryptocurrencies.
    --
    -- -   SM2 key pairs (China Regions only)
    --
    --     -   @SM2@
    keySpec :: Prelude.Maybe KeySpec,
    -- | A flag to indicate whether to bypass the key policy lockout safety
    -- check.
    --
    -- Setting this value to true increases the risk that the KMS key becomes
    -- unmanageable. Do not set this value to true indiscriminately.
    --
    -- For more information, refer to the scenario in the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
    -- section in the //Key Management Service Developer Guide// .
    --
    -- Use this parameter only when you include a policy in the request and you
    -- intend to prevent the principal that is making the request from making a
    -- subsequent PutKeyPolicy request on the KMS key.
    --
    -- The default value is false.
    bypassPolicyLockoutSafetyCheck :: Prelude.Maybe Prelude.Bool,
    -- | The source of the key material for the KMS key. You cannot change the
    -- origin after you create the KMS key. The default is @AWS_KMS@, which
    -- means that KMS creates the key material.
    --
    -- To create a KMS key with no key material (for imported key material),
    -- set the value to @EXTERNAL@. For more information about importing key
    -- material into KMS, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
    -- in the /Key Management Service Developer Guide/. This value is valid
    -- only for symmetric encryption KMS keys.
    --
    -- To create a KMS key in an KMS
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
    -- and create its key material in the associated CloudHSM cluster, set this
    -- value to @AWS_CLOUDHSM@. You must also use the @CustomKeyStoreId@
    -- parameter to identify the custom key store. This value is valid only for
    -- symmetric encryption KMS keys.
    origin :: Prelude.Maybe OriginType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createKey_tags' - Assigns one or more tags to the KMS key. Use this parameter to tag the
-- KMS key when it is created. To tag an existing KMS key, use the
-- TagResource operation.
--
-- Tagging or untagging a KMS key can allow or deny permission to the KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- To use this parameter, you must have
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
-- permission in an IAM policy.
--
-- Each tag consists of a tag key and a tag value. Both the tag key and the
-- tag value are required, but the tag value can be an empty (null) string.
-- You cannot have more than one tag on a KMS key with the same tag key. If
-- you specify an existing tag key with a different tag value, KMS replaces
-- the current tag value with the specified one.
--
-- When you add tags to an Amazon Web Services resource, Amazon Web
-- Services generates a cost allocation report with usage and costs
-- aggregated by tags. Tags can also be used to control access to a KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging Keys>.
--
-- 'policy', 'createKey_policy' - The key policy to attach to the KMS key. If you do not specify a key
-- policy, KMS attaches a default key policy to the KMS key. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default key policy>
-- in the /Key Management Service Developer Guide/.
--
-- If you provide a key policy, it must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to @True@, the
--     key policy must allow the principal that is making the @CreateKey@
--     request to make a subsequent PutKeyPolicy request on the KMS key.
--     This reduces the risk that the KMS key becomes unmanageable. For
--     more information, refer to the scenario in the
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
--     section of the //Key Management Service Developer Guide// .
--
-- -   Each statement in the key policy must contain one or more
--     principals. The principals in the key policy must exist and be
--     visible to KMS. When you create a new Amazon Web Services principal
--     (for example, an IAM user or role), you might need to enforce a
--     delay before including the new principal in a key policy because the
--     new principal might not be immediately visible to KMS. For more
--     information, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
--     in the /Amazon Web Services Identity and Access Management User
--     Guide/.
--
-- A key policy document can include only the following characters:
--
-- -   Printable ASCII characters from the space character (@\\u0020@)
--     through the end of the ASCII character range.
--
-- -   Printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@).
--
-- -   The tab (@\\u0009@), line feed (@\\u000A@), and carriage return
--     (@\\u000D@) special characters
--
-- For information about key policies, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key policies in KMS>
-- in the /Key Management Service Developer Guide/. For help writing and
-- formatting a JSON policy document, see the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
-- in the //Identity and Access Management User Guide// .
--
-- 'customKeyStoreId', 'createKey_customKeyStoreId' - Creates the KMS key in the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- and the key material in its associated CloudHSM cluster. To create a KMS
-- key in a custom key store, you must also specify the @Origin@ parameter
-- with a value of @AWS_CLOUDHSM@. The CloudHSM cluster that is associated
-- with the custom key store must have at least two active HSMs, each in a
-- different Availability Zone in the Region.
--
-- This parameter is valid only for symmetric encryption KMS keys in a
-- single Region. You cannot create any other type of KMS key in a custom
-- key store.
--
-- To find the ID of a custom key store, use the DescribeCustomKeyStores
-- operation.
--
-- The response includes the custom key store ID and the ID of the CloudHSM
-- cluster.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store feature>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a single-tenant key store.
--
-- 'customerMasterKeySpec', 'createKey_customerMasterKeySpec' - Instead, use the @KeySpec@ parameter.
--
-- The @KeySpec@ and @CustomerMasterKeySpec@ parameters work the same way.
-- Only the names differ. We recommend that you use @KeySpec@ parameter in
-- your code. However, to avoid breaking changes, KMS will support both
-- parameters.
--
-- 'keyUsage', 'createKey_keyUsage' - Determines the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- for which you can use the KMS key. The default value is
-- @ENCRYPT_DECRYPT@. This parameter is optional when you are creating a
-- symmetric encryption KMS key; otherwise, it is required. You can\'t
-- change the @KeyUsage@ value after the KMS key is created.
--
-- Select only one valid value.
--
-- -   For symmetric encryption KMS keys, omit the parameter or specify
--     @ENCRYPT_DECRYPT@.
--
-- -   For HMAC KMS keys (symmetric), specify @GENERATE_VERIFY_MAC@.
--
-- -   For asymmetric KMS keys with RSA key material, specify
--     @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@.
--
-- -   For asymmetric KMS keys with ECC key material, specify
--     @SIGN_VERIFY@.
--
-- -   For asymmetric KMS keys with SM2 key material (China Regions only),
--     specify @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@.
--
-- 'description', 'createKey_description' - A description of the KMS key.
--
-- Use a description that helps you decide whether the KMS key is
-- appropriate for a task. The default value is an empty string (no
-- description).
--
-- To set or change the description after the key is created, use
-- UpdateKeyDescription.
--
-- 'multiRegion', 'createKey_multiRegion' - Creates a multi-Region primary key that you can replicate into other
-- Amazon Web Services Regions. You cannot change this value after you
-- create the KMS key.
--
-- For a multi-Region key, set this parameter to @True@. For a
-- single-Region KMS key, omit this parameter or set it to @False@. The
-- default value is @False@.
--
-- This operation supports /multi-Region keys/, an KMS feature that lets
-- you create multiple interoperable KMS keys in different Amazon Web
-- Services Regions. Because these KMS keys have the same key ID, key
-- material, and other metadata, you can use them interchangeably to
-- encrypt data in one Amazon Web Services Region and decrypt it in a
-- different Amazon Web Services Region without re-encrypting the data or
-- making a cross-Region call. For more information about multi-Region
-- keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- This value creates a /primary key/, not a replica. To create a /replica
-- key/, use the ReplicateKey operation.
--
-- You can create a multi-Region version of a symmetric encryption KMS key,
-- an HMAC KMS key, an asymmetric KMS key, or a KMS key with imported key
-- material. However, you cannot create a multi-Region key in a custom key
-- store.
--
-- 'keySpec', 'createKey_keySpec' - Specifies the type of KMS key to create. The default value,
-- @SYMMETRIC_DEFAULT@, creates a KMS key with a 256-bit AES-GCM key that
-- is used for encryption and decryption, except in China Regions, where it
-- creates a 128-bit symmetric key that uses SM4 encryption. For help
-- choosing a key spec for your KMS key, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-types.html#symm-asymm-choose Choosing a KMS key type>
-- in the //Key Management Service Developer Guide// .
--
-- The @KeySpec@ determines whether the KMS key contains a symmetric key or
-- an asymmetric key pair. It also determines the cryptographic algorithms
-- that the KMS key supports. You can\'t change the @KeySpec@ after the KMS
-- key is created. To further restrict the algorithms that can be used with
-- the KMS key, use a condition key in its key policy or IAM policy. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-algorithm kms:EncryptionAlgorithm>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-mac-algorithm kms:MacAlgorithm>
-- or
-- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-signing-algorithm kms:Signing Algorithm>
-- in the //Key Management Service Developer Guide// .
--
-- <http://aws.amazon.com/kms/features/#AWS_Service_Integration Amazon Web Services services that are integrated with KMS>
-- use symmetric encryption KMS keys to protect your data. These services
-- do not support asymmetric KMS keys or HMAC KMS keys.
--
-- KMS supports the following key specs for KMS keys:
--
-- -   Symmetric encryption key (default)
--
--     -   @SYMMETRIC_DEFAULT@
--
-- -   HMAC keys (symmetric)
--
--     -   @HMAC_224@
--
--     -   @HMAC_256@
--
--     -   @HMAC_384@
--
--     -   @HMAC_512@
--
-- -   Asymmetric RSA key pairs
--
--     -   @RSA_2048@
--
--     -   @RSA_3072@
--
--     -   @RSA_4096@
--
-- -   Asymmetric NIST-recommended elliptic curve key pairs
--
--     -   @ECC_NIST_P256@ (secp256r1)
--
--     -   @ECC_NIST_P384@ (secp384r1)
--
--     -   @ECC_NIST_P521@ (secp521r1)
--
-- -   Other asymmetric elliptic curve key pairs
--
--     -   @ECC_SECG_P256K1@ (secp256k1), commonly used for
--         cryptocurrencies.
--
-- -   SM2 key pairs (China Regions only)
--
--     -   @SM2@
--
-- 'bypassPolicyLockoutSafetyCheck', 'createKey_bypassPolicyLockoutSafetyCheck' - A flag to indicate whether to bypass the key policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the KMS key becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the //Key Management Service Developer Guide// .
--
-- Use this parameter only when you include a policy in the request and you
-- intend to prevent the principal that is making the request from making a
-- subsequent PutKeyPolicy request on the KMS key.
--
-- The default value is false.
--
-- 'origin', 'createKey_origin' - The source of the key material for the KMS key. You cannot change the
-- origin after you create the KMS key. The default is @AWS_KMS@, which
-- means that KMS creates the key material.
--
-- To create a KMS key with no key material (for imported key material),
-- set the value to @EXTERNAL@. For more information about importing key
-- material into KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
-- in the /Key Management Service Developer Guide/. This value is valid
-- only for symmetric encryption KMS keys.
--
-- To create a KMS key in an KMS
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- and create its key material in the associated CloudHSM cluster, set this
-- value to @AWS_CLOUDHSM@. You must also use the @CustomKeyStoreId@
-- parameter to identify the custom key store. This value is valid only for
-- symmetric encryption KMS keys.
newCreateKey ::
  CreateKey
newCreateKey =
  CreateKey'
    { tags = Prelude.Nothing,
      policy = Prelude.Nothing,
      customKeyStoreId = Prelude.Nothing,
      customerMasterKeySpec = Prelude.Nothing,
      keyUsage = Prelude.Nothing,
      description = Prelude.Nothing,
      multiRegion = Prelude.Nothing,
      keySpec = Prelude.Nothing,
      bypassPolicyLockoutSafetyCheck = Prelude.Nothing,
      origin = Prelude.Nothing
    }

-- | Assigns one or more tags to the KMS key. Use this parameter to tag the
-- KMS key when it is created. To tag an existing KMS key, use the
-- TagResource operation.
--
-- Tagging or untagging a KMS key can allow or deny permission to the KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- To use this parameter, you must have
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
-- permission in an IAM policy.
--
-- Each tag consists of a tag key and a tag value. Both the tag key and the
-- tag value are required, but the tag value can be an empty (null) string.
-- You cannot have more than one tag on a KMS key with the same tag key. If
-- you specify an existing tag key with a different tag value, KMS replaces
-- the current tag value with the specified one.
--
-- When you add tags to an Amazon Web Services resource, Amazon Web
-- Services generates a cost allocation report with usage and costs
-- aggregated by tags. Tags can also be used to control access to a KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging Keys>.
createKey_tags :: Lens.Lens' CreateKey (Prelude.Maybe [Tag])
createKey_tags = Lens.lens (\CreateKey' {tags} -> tags) (\s@CreateKey' {} a -> s {tags = a} :: CreateKey) Prelude.. Lens.mapping Lens.coerced

-- | The key policy to attach to the KMS key. If you do not specify a key
-- policy, KMS attaches a default key policy to the KMS key. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default key policy>
-- in the /Key Management Service Developer Guide/.
--
-- If you provide a key policy, it must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to @True@, the
--     key policy must allow the principal that is making the @CreateKey@
--     request to make a subsequent PutKeyPolicy request on the KMS key.
--     This reduces the risk that the KMS key becomes unmanageable. For
--     more information, refer to the scenario in the
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
--     section of the //Key Management Service Developer Guide// .
--
-- -   Each statement in the key policy must contain one or more
--     principals. The principals in the key policy must exist and be
--     visible to KMS. When you create a new Amazon Web Services principal
--     (for example, an IAM user or role), you might need to enforce a
--     delay before including the new principal in a key policy because the
--     new principal might not be immediately visible to KMS. For more
--     information, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
--     in the /Amazon Web Services Identity and Access Management User
--     Guide/.
--
-- A key policy document can include only the following characters:
--
-- -   Printable ASCII characters from the space character (@\\u0020@)
--     through the end of the ASCII character range.
--
-- -   Printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@).
--
-- -   The tab (@\\u0009@), line feed (@\\u000A@), and carriage return
--     (@\\u000D@) special characters
--
-- For information about key policies, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key policies in KMS>
-- in the /Key Management Service Developer Guide/. For help writing and
-- formatting a JSON policy document, see the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
-- in the //Identity and Access Management User Guide// .
createKey_policy :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Text)
createKey_policy = Lens.lens (\CreateKey' {policy} -> policy) (\s@CreateKey' {} a -> s {policy = a} :: CreateKey)

-- | Creates the KMS key in the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- and the key material in its associated CloudHSM cluster. To create a KMS
-- key in a custom key store, you must also specify the @Origin@ parameter
-- with a value of @AWS_CLOUDHSM@. The CloudHSM cluster that is associated
-- with the custom key store must have at least two active HSMs, each in a
-- different Availability Zone in the Region.
--
-- This parameter is valid only for symmetric encryption KMS keys in a
-- single Region. You cannot create any other type of KMS key in a custom
-- key store.
--
-- To find the ID of a custom key store, use the DescribeCustomKeyStores
-- operation.
--
-- The response includes the custom key store ID and the ID of the CloudHSM
-- cluster.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store feature>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a single-tenant key store.
createKey_customKeyStoreId :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Text)
createKey_customKeyStoreId = Lens.lens (\CreateKey' {customKeyStoreId} -> customKeyStoreId) (\s@CreateKey' {} a -> s {customKeyStoreId = a} :: CreateKey)

-- | Instead, use the @KeySpec@ parameter.
--
-- The @KeySpec@ and @CustomerMasterKeySpec@ parameters work the same way.
-- Only the names differ. We recommend that you use @KeySpec@ parameter in
-- your code. However, to avoid breaking changes, KMS will support both
-- parameters.
createKey_customerMasterKeySpec :: Lens.Lens' CreateKey (Prelude.Maybe CustomerMasterKeySpec)
createKey_customerMasterKeySpec = Lens.lens (\CreateKey' {customerMasterKeySpec} -> customerMasterKeySpec) (\s@CreateKey' {} a -> s {customerMasterKeySpec = a} :: CreateKey)

-- | Determines the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- for which you can use the KMS key. The default value is
-- @ENCRYPT_DECRYPT@. This parameter is optional when you are creating a
-- symmetric encryption KMS key; otherwise, it is required. You can\'t
-- change the @KeyUsage@ value after the KMS key is created.
--
-- Select only one valid value.
--
-- -   For symmetric encryption KMS keys, omit the parameter or specify
--     @ENCRYPT_DECRYPT@.
--
-- -   For HMAC KMS keys (symmetric), specify @GENERATE_VERIFY_MAC@.
--
-- -   For asymmetric KMS keys with RSA key material, specify
--     @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@.
--
-- -   For asymmetric KMS keys with ECC key material, specify
--     @SIGN_VERIFY@.
--
-- -   For asymmetric KMS keys with SM2 key material (China Regions only),
--     specify @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@.
createKey_keyUsage :: Lens.Lens' CreateKey (Prelude.Maybe KeyUsageType)
createKey_keyUsage = Lens.lens (\CreateKey' {keyUsage} -> keyUsage) (\s@CreateKey' {} a -> s {keyUsage = a} :: CreateKey)

-- | A description of the KMS key.
--
-- Use a description that helps you decide whether the KMS key is
-- appropriate for a task. The default value is an empty string (no
-- description).
--
-- To set or change the description after the key is created, use
-- UpdateKeyDescription.
createKey_description :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Text)
createKey_description = Lens.lens (\CreateKey' {description} -> description) (\s@CreateKey' {} a -> s {description = a} :: CreateKey)

-- | Creates a multi-Region primary key that you can replicate into other
-- Amazon Web Services Regions. You cannot change this value after you
-- create the KMS key.
--
-- For a multi-Region key, set this parameter to @True@. For a
-- single-Region KMS key, omit this parameter or set it to @False@. The
-- default value is @False@.
--
-- This operation supports /multi-Region keys/, an KMS feature that lets
-- you create multiple interoperable KMS keys in different Amazon Web
-- Services Regions. Because these KMS keys have the same key ID, key
-- material, and other metadata, you can use them interchangeably to
-- encrypt data in one Amazon Web Services Region and decrypt it in a
-- different Amazon Web Services Region without re-encrypting the data or
-- making a cross-Region call. For more information about multi-Region
-- keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- This value creates a /primary key/, not a replica. To create a /replica
-- key/, use the ReplicateKey operation.
--
-- You can create a multi-Region version of a symmetric encryption KMS key,
-- an HMAC KMS key, an asymmetric KMS key, or a KMS key with imported key
-- material. However, you cannot create a multi-Region key in a custom key
-- store.
createKey_multiRegion :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Bool)
createKey_multiRegion = Lens.lens (\CreateKey' {multiRegion} -> multiRegion) (\s@CreateKey' {} a -> s {multiRegion = a} :: CreateKey)

-- | Specifies the type of KMS key to create. The default value,
-- @SYMMETRIC_DEFAULT@, creates a KMS key with a 256-bit AES-GCM key that
-- is used for encryption and decryption, except in China Regions, where it
-- creates a 128-bit symmetric key that uses SM4 encryption. For help
-- choosing a key spec for your KMS key, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-types.html#symm-asymm-choose Choosing a KMS key type>
-- in the //Key Management Service Developer Guide// .
--
-- The @KeySpec@ determines whether the KMS key contains a symmetric key or
-- an asymmetric key pair. It also determines the cryptographic algorithms
-- that the KMS key supports. You can\'t change the @KeySpec@ after the KMS
-- key is created. To further restrict the algorithms that can be used with
-- the KMS key, use a condition key in its key policy or IAM policy. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-algorithm kms:EncryptionAlgorithm>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-mac-algorithm kms:MacAlgorithm>
-- or
-- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-signing-algorithm kms:Signing Algorithm>
-- in the //Key Management Service Developer Guide// .
--
-- <http://aws.amazon.com/kms/features/#AWS_Service_Integration Amazon Web Services services that are integrated with KMS>
-- use symmetric encryption KMS keys to protect your data. These services
-- do not support asymmetric KMS keys or HMAC KMS keys.
--
-- KMS supports the following key specs for KMS keys:
--
-- -   Symmetric encryption key (default)
--
--     -   @SYMMETRIC_DEFAULT@
--
-- -   HMAC keys (symmetric)
--
--     -   @HMAC_224@
--
--     -   @HMAC_256@
--
--     -   @HMAC_384@
--
--     -   @HMAC_512@
--
-- -   Asymmetric RSA key pairs
--
--     -   @RSA_2048@
--
--     -   @RSA_3072@
--
--     -   @RSA_4096@
--
-- -   Asymmetric NIST-recommended elliptic curve key pairs
--
--     -   @ECC_NIST_P256@ (secp256r1)
--
--     -   @ECC_NIST_P384@ (secp384r1)
--
--     -   @ECC_NIST_P521@ (secp521r1)
--
-- -   Other asymmetric elliptic curve key pairs
--
--     -   @ECC_SECG_P256K1@ (secp256k1), commonly used for
--         cryptocurrencies.
--
-- -   SM2 key pairs (China Regions only)
--
--     -   @SM2@
createKey_keySpec :: Lens.Lens' CreateKey (Prelude.Maybe KeySpec)
createKey_keySpec = Lens.lens (\CreateKey' {keySpec} -> keySpec) (\s@CreateKey' {} a -> s {keySpec = a} :: CreateKey)

-- | A flag to indicate whether to bypass the key policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the KMS key becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the //Key Management Service Developer Guide// .
--
-- Use this parameter only when you include a policy in the request and you
-- intend to prevent the principal that is making the request from making a
-- subsequent PutKeyPolicy request on the KMS key.
--
-- The default value is false.
createKey_bypassPolicyLockoutSafetyCheck :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Bool)
createKey_bypassPolicyLockoutSafetyCheck = Lens.lens (\CreateKey' {bypassPolicyLockoutSafetyCheck} -> bypassPolicyLockoutSafetyCheck) (\s@CreateKey' {} a -> s {bypassPolicyLockoutSafetyCheck = a} :: CreateKey)

-- | The source of the key material for the KMS key. You cannot change the
-- origin after you create the KMS key. The default is @AWS_KMS@, which
-- means that KMS creates the key material.
--
-- To create a KMS key with no key material (for imported key material),
-- set the value to @EXTERNAL@. For more information about importing key
-- material into KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
-- in the /Key Management Service Developer Guide/. This value is valid
-- only for symmetric encryption KMS keys.
--
-- To create a KMS key in an KMS
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- and create its key material in the associated CloudHSM cluster, set this
-- value to @AWS_CLOUDHSM@. You must also use the @CustomKeyStoreId@
-- parameter to identify the custom key store. This value is valid only for
-- symmetric encryption KMS keys.
createKey_origin :: Lens.Lens' CreateKey (Prelude.Maybe OriginType)
createKey_origin = Lens.lens (\CreateKey' {origin} -> origin) (\s@CreateKey' {} a -> s {origin = a} :: CreateKey)

instance Core.AWSRequest CreateKey where
  type AWSResponse CreateKey = CreateKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeyResponse'
            Prelude.<$> (x Core..?> "KeyMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKey where
  hashWithSalt _salt CreateKey' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` customKeyStoreId
      `Prelude.hashWithSalt` customerMasterKeySpec
      `Prelude.hashWithSalt` keyUsage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` multiRegion
      `Prelude.hashWithSalt` keySpec
      `Prelude.hashWithSalt` bypassPolicyLockoutSafetyCheck
      `Prelude.hashWithSalt` origin

instance Prelude.NFData CreateKey where
  rnf CreateKey' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf customerMasterKeySpec
      `Prelude.seq` Prelude.rnf keyUsage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf multiRegion
      `Prelude.seq` Prelude.rnf keySpec
      `Prelude.seq` Prelude.rnf bypassPolicyLockoutSafetyCheck
      `Prelude.seq` Prelude.rnf origin

instance Core.ToHeaders CreateKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.CreateKey" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateKey where
  toJSON CreateKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Policy" Core..=) Prelude.<$> policy,
            ("CustomKeyStoreId" Core..=)
              Prelude.<$> customKeyStoreId,
            ("CustomerMasterKeySpec" Core..=)
              Prelude.<$> customerMasterKeySpec,
            ("KeyUsage" Core..=) Prelude.<$> keyUsage,
            ("Description" Core..=) Prelude.<$> description,
            ("MultiRegion" Core..=) Prelude.<$> multiRegion,
            ("KeySpec" Core..=) Prelude.<$> keySpec,
            ("BypassPolicyLockoutSafetyCheck" Core..=)
              Prelude.<$> bypassPolicyLockoutSafetyCheck,
            ("Origin" Core..=) Prelude.<$> origin
          ]
      )

instance Core.ToPath CreateKey where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKeyResponse' smart constructor.
data CreateKeyResponse = CreateKeyResponse'
  { -- | Metadata associated with the KMS key.
    keyMetadata :: Prelude.Maybe KeyMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyMetadata', 'createKeyResponse_keyMetadata' - Metadata associated with the KMS key.
--
-- 'httpStatus', 'createKeyResponse_httpStatus' - The response's http status code.
newCreateKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKeyResponse
newCreateKeyResponse pHttpStatus_ =
  CreateKeyResponse'
    { keyMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata associated with the KMS key.
createKeyResponse_keyMetadata :: Lens.Lens' CreateKeyResponse (Prelude.Maybe KeyMetadata)
createKeyResponse_keyMetadata = Lens.lens (\CreateKeyResponse' {keyMetadata} -> keyMetadata) (\s@CreateKeyResponse' {} a -> s {keyMetadata = a} :: CreateKeyResponse)

-- | The response's http status code.
createKeyResponse_httpStatus :: Lens.Lens' CreateKeyResponse Prelude.Int
createKeyResponse_httpStatus = Lens.lens (\CreateKeyResponse' {httpStatus} -> httpStatus) (\s@CreateKeyResponse' {} a -> s {httpStatus = a} :: CreateKeyResponse)

instance Prelude.NFData CreateKeyResponse where
  rnf CreateKeyResponse' {..} =
    Prelude.rnf keyMetadata
      `Prelude.seq` Prelude.rnf httpStatus

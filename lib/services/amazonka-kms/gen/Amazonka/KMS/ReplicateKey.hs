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
-- Module      : Amazonka.KMS.ReplicateKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replicates a multi-Region key into the specified Region. This operation
-- creates a multi-Region replica key based on a multi-Region primary key
-- in a different Region of the same Amazon Web Services partition. You can
-- create multiple replicas of a primary key, but each must be in a
-- different Region. To create a multi-Region primary key, use the
-- CreateKey operation.
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
-- A /replica key/ is a fully-functional KMS key that can be used
-- independently of its primary and peer replica keys. A primary key and
-- its replica keys share properties that make them interoperable. They
-- have the same
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id key ID>
-- and key material. They also have the same
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-spec key spec>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-usage key usage>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-origin key material origin>,
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic key rotation status>.
-- KMS automatically synchronizes these shared properties among related
-- multi-Region keys. All other properties of a replica key can differ,
-- including its
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html key policy>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html tags>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html aliases>,
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>.
-- KMS pricing and quotas for KMS keys apply to each primary key and
-- replica key.
--
-- When this operation completes, the new replica key has a transient key
-- state of @Creating@. This key state changes to @Enabled@ (or
-- @PendingImport@) after a few seconds when the process of creating the
-- new replica key is complete. While the key state is @Creating@, you can
-- manage key, but you cannot yet use it in cryptographic operations. If
-- you are creating and using the replica key programmatically, retry on
-- @KMSInvalidStateException@ or call @DescribeKey@ to check its @KeyState@
-- value before using it. For details about the @Creating@ key state, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- You cannot create more than one replica of a primary key in any Region.
-- If the Region already includes a replica of the key you\'re trying to
-- replicate, @ReplicateKey@ returns an @AlreadyExistsException@ error. If
-- the key state of the existing replica is @PendingDeletion@, you can
-- cancel the scheduled key deletion (CancelKeyDeletion) or wait for the
-- key to be deleted. The new replica key you create will have the same
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html#mrk-sync-properties shared properties>
-- as the original replica key.
--
-- The CloudTrail log of a @ReplicateKey@ operation records a
-- @ReplicateKey@ operation in the primary key\'s Region and a CreateKey
-- operation in the replica key\'s Region.
--
-- If you replicate a multi-Region primary key with imported key material,
-- the replica key is created with no key material. You must import the
-- same key material that you imported into the primary key. For details,
-- see
-- <kms/latest/developerguide/multi-region-keys-import.html Importing key material into multi-Region keys>
-- in the /Key Management Service Developer Guide/.
--
-- To convert a replica key to a primary key, use the UpdatePrimaryRegion
-- operation.
--
-- @ReplicateKey@ uses different default values for the @KeyPolicy@ and
-- @Tags@ parameters than those used in the KMS console. For details, see
-- the parameter descriptions.
--
-- __Cross-account use__: No. You cannot use this operation to create a
-- replica key in a different Amazon Web Services account.
--
-- __Required permissions__:
--
-- -   @kms:ReplicateKey@ on the primary key (in the primary key\'s
--     Region). Include this permission in the primary key\'s key policy.
--
-- -   @kms:CreateKey@ in an IAM policy in the replica Region.
--
-- -   To use the @Tags@ parameter, @kms:TagResource@ in an IAM policy in
--     the replica Region.
--
-- __Related operations__
--
-- -   CreateKey
--
-- -   UpdatePrimaryRegion
module Amazonka.KMS.ReplicateKey
  ( -- * Creating a Request
    ReplicateKey (..),
    newReplicateKey,

    -- * Request Lenses
    replicateKey_bypassPolicyLockoutSafetyCheck,
    replicateKey_description,
    replicateKey_policy,
    replicateKey_tags,
    replicateKey_keyId,
    replicateKey_replicaRegion,

    -- * Destructuring the Response
    ReplicateKeyResponse (..),
    newReplicateKeyResponse,

    -- * Response Lenses
    replicateKeyResponse_replicaKeyMetadata,
    replicateKeyResponse_replicaPolicy,
    replicateKeyResponse_replicaTags,
    replicateKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReplicateKey' smart constructor.
data ReplicateKey = ReplicateKey'
  { -- | A flag to indicate whether to bypass the key policy lockout safety
    -- check.
    --
    -- Setting this value to true increases the risk that the KMS key becomes
    -- unmanageable. Do not set this value to true indiscriminately.
    --
    -- For more information, refer to the scenario in the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
    -- section in the /Key Management Service Developer Guide/.
    --
    -- Use this parameter only when you intend to prevent the principal that is
    -- making the request from making a subsequent @PutKeyPolicy@ request on
    -- the KMS key.
    --
    -- The default value is false.
    bypassPolicyLockoutSafetyCheck :: Prelude.Maybe Prelude.Bool,
    -- | A description of the KMS key. The default value is an empty string (no
    -- description).
    --
    -- The description is not a shared property of multi-Region keys. You can
    -- specify the same description or a different description for each key in
    -- a set of related multi-Region keys. KMS does not synchronize this
    -- property.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key policy to attach to the KMS key. This parameter is optional. If
    -- you do not provide a key policy, KMS attaches the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default default key policy>
    -- to the KMS key.
    --
    -- The key policy is not a shared property of multi-Region keys. You can
    -- specify the same key policy or a different key policy for each key in a
    -- set of related multi-Region keys. KMS does not synchronize this
    -- property.
    --
    -- If you provide a key policy, it must meet the following criteria:
    --
    -- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
    --     policy must give the caller @kms:PutKeyPolicy@ permission on the
    --     replica key. This reduces the risk that the KMS key becomes
    --     unmanageable. For more information, refer to the scenario in the
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
    --     in the //Identity and Access Management User Guide// .
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
    -- | Assigns one or more tags to the replica key. Use this parameter to tag
    -- the KMS key when it is created. To tag an existing KMS key, use the
    -- TagResource operation.
    --
    -- Tagging or untagging a KMS key can allow or deny permission to the KMS
    -- key. For details, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC for KMS>
    -- in the /Key Management Service Developer Guide/.
    --
    -- To use this parameter, you must have
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
    -- permission in an IAM policy.
    --
    -- Tags are not a shared property of multi-Region keys. You can specify the
    -- same tags or different tags for each key in a set of related
    -- multi-Region keys. KMS does not synchronize this property.
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
    -- | Identifies the multi-Region primary key that is being replicated. To
    -- determine whether a KMS key is a multi-Region primary key, use the
    -- DescribeKey operation to check the value of the @MultiRegionKeyType@
    -- property.
    --
    -- Specify the key ID or key ARN of a multi-Region primary key.
    --
    -- For example:
    --
    -- -   Key ID: @mrk-1234abcd12ab34cd56ef1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/mrk-1234abcd12ab34cd56ef1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text,
    -- | The Region ID of the Amazon Web Services Region for this replica key.
    --
    -- Enter the Region ID, such as @us-east-1@ or @ap-southeast-2@. For a list
    -- of Amazon Web Services Regions in which KMS is supported, see
    -- <https://docs.aws.amazon.com/general/latest/gr/kms.html#kms_region KMS service endpoints>
    -- in the /Amazon Web Services General Reference/.
    --
    -- HMAC KMS keys are not supported in all Amazon Web Services Regions. If
    -- you try to replicate an HMAC KMS key in an Amazon Web Services Region in
    -- which HMAC keys are not supported, the @ReplicateKey@ operation returns
    -- an @UnsupportedOperationException@. For a list of Regions in which HMAC
    -- KMS keys are supported, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC keys in KMS>
    -- in the /Key Management Service Developer Guide/.
    --
    -- The replica must be in a different Amazon Web Services Region than its
    -- primary key and other replicas of that primary key, but in the same
    -- Amazon Web Services partition. KMS must be available in the replica
    -- Region. If the Region is not enabled by default, the Amazon Web Services
    -- account must be enabled in the Region. For information about Amazon Web
    -- Services partitions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/. For information about
    -- enabling and disabling Regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-enable Enabling a Region>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-disable Disabling a Region>
    -- in the /Amazon Web Services General Reference/.
    replicaRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicateKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassPolicyLockoutSafetyCheck', 'replicateKey_bypassPolicyLockoutSafetyCheck' - A flag to indicate whether to bypass the key policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the KMS key becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the /Key Management Service Developer Guide/.
--
-- Use this parameter only when you intend to prevent the principal that is
-- making the request from making a subsequent @PutKeyPolicy@ request on
-- the KMS key.
--
-- The default value is false.
--
-- 'description', 'replicateKey_description' - A description of the KMS key. The default value is an empty string (no
-- description).
--
-- The description is not a shared property of multi-Region keys. You can
-- specify the same description or a different description for each key in
-- a set of related multi-Region keys. KMS does not synchronize this
-- property.
--
-- 'policy', 'replicateKey_policy' - The key policy to attach to the KMS key. This parameter is optional. If
-- you do not provide a key policy, KMS attaches the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default default key policy>
-- to the KMS key.
--
-- The key policy is not a shared property of multi-Region keys. You can
-- specify the same key policy or a different key policy for each key in a
-- set of related multi-Region keys. KMS does not synchronize this
-- property.
--
-- If you provide a key policy, it must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
--     policy must give the caller @kms:PutKeyPolicy@ permission on the
--     replica key. This reduces the risk that the KMS key becomes
--     unmanageable. For more information, refer to the scenario in the
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
--     in the //Identity and Access Management User Guide// .
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
-- 'tags', 'replicateKey_tags' - Assigns one or more tags to the replica key. Use this parameter to tag
-- the KMS key when it is created. To tag an existing KMS key, use the
-- TagResource operation.
--
-- Tagging or untagging a KMS key can allow or deny permission to the KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC for KMS>
-- in the /Key Management Service Developer Guide/.
--
-- To use this parameter, you must have
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
-- permission in an IAM policy.
--
-- Tags are not a shared property of multi-Region keys. You can specify the
-- same tags or different tags for each key in a set of related
-- multi-Region keys. KMS does not synchronize this property.
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
-- 'keyId', 'replicateKey_keyId' - Identifies the multi-Region primary key that is being replicated. To
-- determine whether a KMS key is a multi-Region primary key, use the
-- DescribeKey operation to check the value of the @MultiRegionKeyType@
-- property.
--
-- Specify the key ID or key ARN of a multi-Region primary key.
--
-- For example:
--
-- -   Key ID: @mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
--
-- 'replicaRegion', 'replicateKey_replicaRegion' - The Region ID of the Amazon Web Services Region for this replica key.
--
-- Enter the Region ID, such as @us-east-1@ or @ap-southeast-2@. For a list
-- of Amazon Web Services Regions in which KMS is supported, see
-- <https://docs.aws.amazon.com/general/latest/gr/kms.html#kms_region KMS service endpoints>
-- in the /Amazon Web Services General Reference/.
--
-- HMAC KMS keys are not supported in all Amazon Web Services Regions. If
-- you try to replicate an HMAC KMS key in an Amazon Web Services Region in
-- which HMAC keys are not supported, the @ReplicateKey@ operation returns
-- an @UnsupportedOperationException@. For a list of Regions in which HMAC
-- KMS keys are supported, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC keys in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- The replica must be in a different Amazon Web Services Region than its
-- primary key and other replicas of that primary key, but in the same
-- Amazon Web Services partition. KMS must be available in the replica
-- Region. If the Region is not enabled by default, the Amazon Web Services
-- account must be enabled in the Region. For information about Amazon Web
-- Services partitions, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For information about
-- enabling and disabling Regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-enable Enabling a Region>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-disable Disabling a Region>
-- in the /Amazon Web Services General Reference/.
newReplicateKey ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'replicaRegion'
  Prelude.Text ->
  ReplicateKey
newReplicateKey pKeyId_ pReplicaRegion_ =
  ReplicateKey'
    { bypassPolicyLockoutSafetyCheck =
        Prelude.Nothing,
      description = Prelude.Nothing,
      policy = Prelude.Nothing,
      tags = Prelude.Nothing,
      keyId = pKeyId_,
      replicaRegion = pReplicaRegion_
    }

-- | A flag to indicate whether to bypass the key policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the KMS key becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the /Key Management Service Developer Guide/.
--
-- Use this parameter only when you intend to prevent the principal that is
-- making the request from making a subsequent @PutKeyPolicy@ request on
-- the KMS key.
--
-- The default value is false.
replicateKey_bypassPolicyLockoutSafetyCheck :: Lens.Lens' ReplicateKey (Prelude.Maybe Prelude.Bool)
replicateKey_bypassPolicyLockoutSafetyCheck = Lens.lens (\ReplicateKey' {bypassPolicyLockoutSafetyCheck} -> bypassPolicyLockoutSafetyCheck) (\s@ReplicateKey' {} a -> s {bypassPolicyLockoutSafetyCheck = a} :: ReplicateKey)

-- | A description of the KMS key. The default value is an empty string (no
-- description).
--
-- The description is not a shared property of multi-Region keys. You can
-- specify the same description or a different description for each key in
-- a set of related multi-Region keys. KMS does not synchronize this
-- property.
replicateKey_description :: Lens.Lens' ReplicateKey (Prelude.Maybe Prelude.Text)
replicateKey_description = Lens.lens (\ReplicateKey' {description} -> description) (\s@ReplicateKey' {} a -> s {description = a} :: ReplicateKey)

-- | The key policy to attach to the KMS key. This parameter is optional. If
-- you do not provide a key policy, KMS attaches the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default default key policy>
-- to the KMS key.
--
-- The key policy is not a shared property of multi-Region keys. You can
-- specify the same key policy or a different key policy for each key in a
-- set of related multi-Region keys. KMS does not synchronize this
-- property.
--
-- If you provide a key policy, it must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
--     policy must give the caller @kms:PutKeyPolicy@ permission on the
--     replica key. This reduces the risk that the KMS key becomes
--     unmanageable. For more information, refer to the scenario in the
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
--     in the //Identity and Access Management User Guide// .
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
replicateKey_policy :: Lens.Lens' ReplicateKey (Prelude.Maybe Prelude.Text)
replicateKey_policy = Lens.lens (\ReplicateKey' {policy} -> policy) (\s@ReplicateKey' {} a -> s {policy = a} :: ReplicateKey)

-- | Assigns one or more tags to the replica key. Use this parameter to tag
-- the KMS key when it is created. To tag an existing KMS key, use the
-- TagResource operation.
--
-- Tagging or untagging a KMS key can allow or deny permission to the KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC for KMS>
-- in the /Key Management Service Developer Guide/.
--
-- To use this parameter, you must have
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
-- permission in an IAM policy.
--
-- Tags are not a shared property of multi-Region keys. You can specify the
-- same tags or different tags for each key in a set of related
-- multi-Region keys. KMS does not synchronize this property.
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
replicateKey_tags :: Lens.Lens' ReplicateKey (Prelude.Maybe [Tag])
replicateKey_tags = Lens.lens (\ReplicateKey' {tags} -> tags) (\s@ReplicateKey' {} a -> s {tags = a} :: ReplicateKey) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the multi-Region primary key that is being replicated. To
-- determine whether a KMS key is a multi-Region primary key, use the
-- DescribeKey operation to check the value of the @MultiRegionKeyType@
-- property.
--
-- Specify the key ID or key ARN of a multi-Region primary key.
--
-- For example:
--
-- -   Key ID: @mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
replicateKey_keyId :: Lens.Lens' ReplicateKey Prelude.Text
replicateKey_keyId = Lens.lens (\ReplicateKey' {keyId} -> keyId) (\s@ReplicateKey' {} a -> s {keyId = a} :: ReplicateKey)

-- | The Region ID of the Amazon Web Services Region for this replica key.
--
-- Enter the Region ID, such as @us-east-1@ or @ap-southeast-2@. For a list
-- of Amazon Web Services Regions in which KMS is supported, see
-- <https://docs.aws.amazon.com/general/latest/gr/kms.html#kms_region KMS service endpoints>
-- in the /Amazon Web Services General Reference/.
--
-- HMAC KMS keys are not supported in all Amazon Web Services Regions. If
-- you try to replicate an HMAC KMS key in an Amazon Web Services Region in
-- which HMAC keys are not supported, the @ReplicateKey@ operation returns
-- an @UnsupportedOperationException@. For a list of Regions in which HMAC
-- KMS keys are supported, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC keys in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- The replica must be in a different Amazon Web Services Region than its
-- primary key and other replicas of that primary key, but in the same
-- Amazon Web Services partition. KMS must be available in the replica
-- Region. If the Region is not enabled by default, the Amazon Web Services
-- account must be enabled in the Region. For information about Amazon Web
-- Services partitions, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For information about
-- enabling and disabling Regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-enable Enabling a Region>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-disable Disabling a Region>
-- in the /Amazon Web Services General Reference/.
replicateKey_replicaRegion :: Lens.Lens' ReplicateKey Prelude.Text
replicateKey_replicaRegion = Lens.lens (\ReplicateKey' {replicaRegion} -> replicaRegion) (\s@ReplicateKey' {} a -> s {replicaRegion = a} :: ReplicateKey)

instance Core.AWSRequest ReplicateKey where
  type AWSResponse ReplicateKey = ReplicateKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReplicateKeyResponse'
            Prelude.<$> (x Data..?> "ReplicaKeyMetadata")
            Prelude.<*> (x Data..?> "ReplicaPolicy")
            Prelude.<*> (x Data..?> "ReplicaTags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReplicateKey where
  hashWithSalt _salt ReplicateKey' {..} =
    _salt
      `Prelude.hashWithSalt` bypassPolicyLockoutSafetyCheck
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` replicaRegion

instance Prelude.NFData ReplicateKey where
  rnf ReplicateKey' {..} =
    Prelude.rnf bypassPolicyLockoutSafetyCheck `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf policy `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf keyId `Prelude.seq`
              Prelude.rnf replicaRegion

instance Data.ToHeaders ReplicateKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.ReplicateKey" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ReplicateKey where
  toJSON ReplicateKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" Data..=)
              Prelude.<$> bypassPolicyLockoutSafetyCheck,
            ("Description" Data..=) Prelude.<$> description,
            ("Policy" Data..=) Prelude.<$> policy,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just
              ("ReplicaRegion" Data..= replicaRegion)
          ]
      )

instance Data.ToPath ReplicateKey where
  toPath = Prelude.const "/"

instance Data.ToQuery ReplicateKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReplicateKeyResponse' smart constructor.
data ReplicateKeyResponse = ReplicateKeyResponse'
  { -- | Displays details about the new replica key, including its Amazon
    -- Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>.
    -- It also includes the ARN and Amazon Web Services Region of its primary
    -- key and other replica keys.
    replicaKeyMetadata :: Prelude.Maybe KeyMetadata,
    -- | The key policy of the new replica key. The value is a key policy
    -- document in JSON format.
    replicaPolicy :: Prelude.Maybe Prelude.Text,
    -- | The tags on the new replica key. The value is a list of tag key and tag
    -- value pairs.
    replicaTags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicateKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaKeyMetadata', 'replicateKeyResponse_replicaKeyMetadata' - Displays details about the new replica key, including its Amazon
-- Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>.
-- It also includes the ARN and Amazon Web Services Region of its primary
-- key and other replica keys.
--
-- 'replicaPolicy', 'replicateKeyResponse_replicaPolicy' - The key policy of the new replica key. The value is a key policy
-- document in JSON format.
--
-- 'replicaTags', 'replicateKeyResponse_replicaTags' - The tags on the new replica key. The value is a list of tag key and tag
-- value pairs.
--
-- 'httpStatus', 'replicateKeyResponse_httpStatus' - The response's http status code.
newReplicateKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReplicateKeyResponse
newReplicateKeyResponse pHttpStatus_ =
  ReplicateKeyResponse'
    { replicaKeyMetadata =
        Prelude.Nothing,
      replicaPolicy = Prelude.Nothing,
      replicaTags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Displays details about the new replica key, including its Amazon
-- Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>.
-- It also includes the ARN and Amazon Web Services Region of its primary
-- key and other replica keys.
replicateKeyResponse_replicaKeyMetadata :: Lens.Lens' ReplicateKeyResponse (Prelude.Maybe KeyMetadata)
replicateKeyResponse_replicaKeyMetadata = Lens.lens (\ReplicateKeyResponse' {replicaKeyMetadata} -> replicaKeyMetadata) (\s@ReplicateKeyResponse' {} a -> s {replicaKeyMetadata = a} :: ReplicateKeyResponse)

-- | The key policy of the new replica key. The value is a key policy
-- document in JSON format.
replicateKeyResponse_replicaPolicy :: Lens.Lens' ReplicateKeyResponse (Prelude.Maybe Prelude.Text)
replicateKeyResponse_replicaPolicy = Lens.lens (\ReplicateKeyResponse' {replicaPolicy} -> replicaPolicy) (\s@ReplicateKeyResponse' {} a -> s {replicaPolicy = a} :: ReplicateKeyResponse)

-- | The tags on the new replica key. The value is a list of tag key and tag
-- value pairs.
replicateKeyResponse_replicaTags :: Lens.Lens' ReplicateKeyResponse (Prelude.Maybe [Tag])
replicateKeyResponse_replicaTags = Lens.lens (\ReplicateKeyResponse' {replicaTags} -> replicaTags) (\s@ReplicateKeyResponse' {} a -> s {replicaTags = a} :: ReplicateKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
replicateKeyResponse_httpStatus :: Lens.Lens' ReplicateKeyResponse Prelude.Int
replicateKeyResponse_httpStatus = Lens.lens (\ReplicateKeyResponse' {httpStatus} -> httpStatus) (\s@ReplicateKeyResponse' {} a -> s {httpStatus = a} :: ReplicateKeyResponse)

instance Prelude.NFData ReplicateKeyResponse where
  rnf ReplicateKeyResponse' {..} =
    Prelude.rnf replicaKeyMetadata `Prelude.seq`
      Prelude.rnf replicaPolicy `Prelude.seq`
        Prelude.rnf replicaTags `Prelude.seq`
          Prelude.rnf httpStatus

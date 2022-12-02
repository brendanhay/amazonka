{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KMS.Types.KeyMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.KeyMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.CustomerMasterKeySpec
import Amazonka.KMS.Types.EncryptionAlgorithmSpec
import Amazonka.KMS.Types.ExpirationModelType
import Amazonka.KMS.Types.KeyManagerType
import Amazonka.KMS.Types.KeySpec
import Amazonka.KMS.Types.KeyState
import Amazonka.KMS.Types.KeyUsageType
import Amazonka.KMS.Types.MacAlgorithmSpec
import Amazonka.KMS.Types.MultiRegionConfiguration
import Amazonka.KMS.Types.OriginType
import Amazonka.KMS.Types.SigningAlgorithmSpec
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about a KMS key.
--
-- This data type is used as a response element for the CreateKey and
-- DescribeKey operations.
--
-- /See:/ 'newKeyMetadata' smart constructor.
data KeyMetadata = KeyMetadata'
  { -- | The twelve-digit account ID of the Amazon Web Services account that owns
    -- the KMS key.
    aWSAccountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the KMS key\'s key material expires. This value is
    -- present only when @Origin@ is @EXTERNAL@, otherwise this value is
    -- omitted.
    expirationModel :: Prelude.Maybe ExpirationModelType,
    -- | The encryption algorithms that the KMS key supports. You cannot use the
    -- KMS key with other encryption algorithms within KMS.
    --
    -- This value is present only when the @KeyUsage@ of the KMS key is
    -- @ENCRYPT_DECRYPT@.
    encryptionAlgorithms :: Prelude.Maybe [EncryptionAlgorithmSpec],
    -- | A unique identifier for the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
    -- that contains the KMS key. This value is present only when the KMS key
    -- is created in a custom key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The waiting period before the primary key in a multi-Region key is
    -- deleted. This waiting period begins when the last of its replica keys is
    -- deleted. This value is present only when the @KeyState@ of the KMS key
    -- is @PendingReplicaDeletion@. That indicates that the KMS key is the
    -- primary key in a multi-Region key, it is scheduled for deletion, and it
    -- still has existing replica keys.
    --
    -- When a single-Region KMS key or a multi-Region replica key is scheduled
    -- for deletion, its deletion date is displayed in the @DeletionDate@
    -- field. However, when the primary key in a multi-Region key is scheduled
    -- for deletion, its waiting period doesn\'t begin until all of its replica
    -- keys are deleted. This value displays that waiting period. When the last
    -- replica key in the multi-Region key is deleted, the @KeyState@ of the
    -- scheduled primary key changes from @PendingReplicaDeletion@ to
    -- @PendingDeletion@ and the deletion date appears in the @DeletionDate@
    -- field.
    pendingDeletionWindowInDays :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the KMS key. For examples, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Key Management Service (KMS)>
    -- in the Example ARNs section of the /Amazon Web Services General
    -- Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Lists the primary and replica keys in same multi-Region key. This field
    -- is present only when the value of the @MultiRegion@ field is @True@.
    --
    -- For more information about any listed KMS key, use the DescribeKey
    -- operation.
    --
    -- -   @MultiRegionKeyType@ indicates whether the KMS key is a @PRIMARY@ or
    --     @REPLICA@ key.
    --
    -- -   @PrimaryKey@ displays the key ARN and Region of the primary key.
    --     This field displays the current KMS key if it is the primary key.
    --
    -- -   @ReplicaKeys@ displays the key ARNs and Regions of all replica keys.
    --     This field includes the current KMS key if it is a replica key.
    multiRegionConfiguration :: Prelude.Maybe MultiRegionConfiguration,
    -- | The date and time when the KMS key was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | Instead, use the @KeySpec@ field.
    --
    -- The @KeySpec@ and @CustomerMasterKeySpec@ fields have the same value. We
    -- recommend that you use the @KeySpec@ field in your code. However, to
    -- avoid breaking changes, KMS will support both fields.
    customerMasterKeySpec :: Prelude.Maybe CustomerMasterKeySpec,
    -- | The
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
    -- for which you can use the KMS key.
    keyUsage :: Prelude.Maybe KeyUsageType,
    -- | The description of the KMS key.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the KMS key is a multi-Region (@True@) or regional
    -- (@False@) key. This value is @True@ for multi-Region primary and replica
    -- keys and @False@ for regional KMS keys.
    --
    -- For more information about multi-Region keys, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
    -- in the /Key Management Service Developer Guide/.
    multiRegion :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the KMS key is enabled. When @KeyState@ is @Enabled@
    -- this value is true, otherwise it is false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The manager of the KMS key. KMS keys in your Amazon Web Services account
    -- are either customer managed or Amazon Web Services managed. For more
    -- information about the difference, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#kms_keys KMS keys>
    -- in the /Key Management Service Developer Guide/.
    keyManager :: Prelude.Maybe KeyManagerType,
    -- | The cluster ID of the CloudHSM cluster that contains the key material
    -- for the KMS key. When you create a KMS key in a
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
    -- KMS creates the key material for the KMS key in the associated CloudHSM
    -- cluster. This value is present only when the KMS key is created in a
    -- custom key store.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | Describes the type of key material in the KMS key.
    keySpec :: Prelude.Maybe KeySpec,
    -- | The current status of the KMS key.
    --
    -- For more information about how key state affects the use of a KMS key,
    -- see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
    -- in the /Key Management Service Developer Guide/.
    keyState :: Prelude.Maybe KeyState,
    -- | The date and time after which KMS deletes this KMS key. This value is
    -- present only when the KMS key is scheduled for deletion, that is, when
    -- its @KeyState@ is @PendingDeletion@.
    --
    -- When the primary key in a multi-Region key is scheduled for deletion but
    -- still has replica keys, its key state is @PendingReplicaDeletion@ and
    -- the length of its waiting period is displayed in the
    -- @PendingDeletionWindowInDays@ field.
    deletionDate :: Prelude.Maybe Data.POSIX,
    -- | The source of the key material for the KMS key. When this value is
    -- @AWS_KMS@, KMS created the key material. When this value is @EXTERNAL@,
    -- the key material was imported or the KMS key doesn\'t have any key
    -- material. When this value is @AWS_CLOUDHSM@, the key material was
    -- created in the CloudHSM cluster associated with a custom key store.
    origin :: Prelude.Maybe OriginType,
    -- | The message authentication code (MAC) algorithm that the HMAC KMS key
    -- supports.
    --
    -- This value is present only when the @KeyUsage@ of the KMS key is
    -- @GENERATE_VERIFY_MAC@.
    macAlgorithms :: Prelude.Maybe [MacAlgorithmSpec],
    -- | The signing algorithms that the KMS key supports. You cannot use the KMS
    -- key with other signing algorithms within KMS.
    --
    -- This field appears only when the @KeyUsage@ of the KMS key is
    -- @SIGN_VERIFY@.
    signingAlgorithms :: Prelude.Maybe [SigningAlgorithmSpec],
    -- | The time at which the imported key material expires. When the key
    -- material expires, KMS deletes the key material and the KMS key becomes
    -- unusable. This value is present only for KMS keys whose @Origin@ is
    -- @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@,
    -- otherwise this value is omitted.
    validTo :: Prelude.Maybe Data.POSIX,
    -- | The globally unique identifier for the KMS key.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aWSAccountId', 'keyMetadata_aWSAccountId' - The twelve-digit account ID of the Amazon Web Services account that owns
-- the KMS key.
--
-- 'expirationModel', 'keyMetadata_expirationModel' - Specifies whether the KMS key\'s key material expires. This value is
-- present only when @Origin@ is @EXTERNAL@, otherwise this value is
-- omitted.
--
-- 'encryptionAlgorithms', 'keyMetadata_encryptionAlgorithms' - The encryption algorithms that the KMS key supports. You cannot use the
-- KMS key with other encryption algorithms within KMS.
--
-- This value is present only when the @KeyUsage@ of the KMS key is
-- @ENCRYPT_DECRYPT@.
--
-- 'customKeyStoreId', 'keyMetadata_customKeyStoreId' - A unique identifier for the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- that contains the KMS key. This value is present only when the KMS key
-- is created in a custom key store.
--
-- 'pendingDeletionWindowInDays', 'keyMetadata_pendingDeletionWindowInDays' - The waiting period before the primary key in a multi-Region key is
-- deleted. This waiting period begins when the last of its replica keys is
-- deleted. This value is present only when the @KeyState@ of the KMS key
-- is @PendingReplicaDeletion@. That indicates that the KMS key is the
-- primary key in a multi-Region key, it is scheduled for deletion, and it
-- still has existing replica keys.
--
-- When a single-Region KMS key or a multi-Region replica key is scheduled
-- for deletion, its deletion date is displayed in the @DeletionDate@
-- field. However, when the primary key in a multi-Region key is scheduled
-- for deletion, its waiting period doesn\'t begin until all of its replica
-- keys are deleted. This value displays that waiting period. When the last
-- replica key in the multi-Region key is deleted, the @KeyState@ of the
-- scheduled primary key changes from @PendingReplicaDeletion@ to
-- @PendingDeletion@ and the deletion date appears in the @DeletionDate@
-- field.
--
-- 'arn', 'keyMetadata_arn' - The Amazon Resource Name (ARN) of the KMS key. For examples, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Key Management Service (KMS)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
--
-- 'multiRegionConfiguration', 'keyMetadata_multiRegionConfiguration' - Lists the primary and replica keys in same multi-Region key. This field
-- is present only when the value of the @MultiRegion@ field is @True@.
--
-- For more information about any listed KMS key, use the DescribeKey
-- operation.
--
-- -   @MultiRegionKeyType@ indicates whether the KMS key is a @PRIMARY@ or
--     @REPLICA@ key.
--
-- -   @PrimaryKey@ displays the key ARN and Region of the primary key.
--     This field displays the current KMS key if it is the primary key.
--
-- -   @ReplicaKeys@ displays the key ARNs and Regions of all replica keys.
--     This field includes the current KMS key if it is a replica key.
--
-- 'creationDate', 'keyMetadata_creationDate' - The date and time when the KMS key was created.
--
-- 'customerMasterKeySpec', 'keyMetadata_customerMasterKeySpec' - Instead, use the @KeySpec@ field.
--
-- The @KeySpec@ and @CustomerMasterKeySpec@ fields have the same value. We
-- recommend that you use the @KeySpec@ field in your code. However, to
-- avoid breaking changes, KMS will support both fields.
--
-- 'keyUsage', 'keyMetadata_keyUsage' - The
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- for which you can use the KMS key.
--
-- 'description', 'keyMetadata_description' - The description of the KMS key.
--
-- 'multiRegion', 'keyMetadata_multiRegion' - Indicates whether the KMS key is a multi-Region (@True@) or regional
-- (@False@) key. This value is @True@ for multi-Region primary and replica
-- keys and @False@ for regional KMS keys.
--
-- For more information about multi-Region keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- 'enabled', 'keyMetadata_enabled' - Specifies whether the KMS key is enabled. When @KeyState@ is @Enabled@
-- this value is true, otherwise it is false.
--
-- 'keyManager', 'keyMetadata_keyManager' - The manager of the KMS key. KMS keys in your Amazon Web Services account
-- are either customer managed or Amazon Web Services managed. For more
-- information about the difference, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#kms_keys KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- 'cloudHsmClusterId', 'keyMetadata_cloudHsmClusterId' - The cluster ID of the CloudHSM cluster that contains the key material
-- for the KMS key. When you create a KMS key in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
-- KMS creates the key material for the KMS key in the associated CloudHSM
-- cluster. This value is present only when the KMS key is created in a
-- custom key store.
--
-- 'keySpec', 'keyMetadata_keySpec' - Describes the type of key material in the KMS key.
--
-- 'keyState', 'keyMetadata_keyState' - The current status of the KMS key.
--
-- For more information about how key state affects the use of a KMS key,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- 'deletionDate', 'keyMetadata_deletionDate' - The date and time after which KMS deletes this KMS key. This value is
-- present only when the KMS key is scheduled for deletion, that is, when
-- its @KeyState@ is @PendingDeletion@.
--
-- When the primary key in a multi-Region key is scheduled for deletion but
-- still has replica keys, its key state is @PendingReplicaDeletion@ and
-- the length of its waiting period is displayed in the
-- @PendingDeletionWindowInDays@ field.
--
-- 'origin', 'keyMetadata_origin' - The source of the key material for the KMS key. When this value is
-- @AWS_KMS@, KMS created the key material. When this value is @EXTERNAL@,
-- the key material was imported or the KMS key doesn\'t have any key
-- material. When this value is @AWS_CLOUDHSM@, the key material was
-- created in the CloudHSM cluster associated with a custom key store.
--
-- 'macAlgorithms', 'keyMetadata_macAlgorithms' - The message authentication code (MAC) algorithm that the HMAC KMS key
-- supports.
--
-- This value is present only when the @KeyUsage@ of the KMS key is
-- @GENERATE_VERIFY_MAC@.
--
-- 'signingAlgorithms', 'keyMetadata_signingAlgorithms' - The signing algorithms that the KMS key supports. You cannot use the KMS
-- key with other signing algorithms within KMS.
--
-- This field appears only when the @KeyUsage@ of the KMS key is
-- @SIGN_VERIFY@.
--
-- 'validTo', 'keyMetadata_validTo' - The time at which the imported key material expires. When the key
-- material expires, KMS deletes the key material and the KMS key becomes
-- unusable. This value is present only for KMS keys whose @Origin@ is
-- @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@,
-- otherwise this value is omitted.
--
-- 'keyId', 'keyMetadata_keyId' - The globally unique identifier for the KMS key.
newKeyMetadata ::
  -- | 'keyId'
  Prelude.Text ->
  KeyMetadata
newKeyMetadata pKeyId_ =
  KeyMetadata'
    { aWSAccountId = Prelude.Nothing,
      expirationModel = Prelude.Nothing,
      encryptionAlgorithms = Prelude.Nothing,
      customKeyStoreId = Prelude.Nothing,
      pendingDeletionWindowInDays = Prelude.Nothing,
      arn = Prelude.Nothing,
      multiRegionConfiguration = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      customerMasterKeySpec = Prelude.Nothing,
      keyUsage = Prelude.Nothing,
      description = Prelude.Nothing,
      multiRegion = Prelude.Nothing,
      enabled = Prelude.Nothing,
      keyManager = Prelude.Nothing,
      cloudHsmClusterId = Prelude.Nothing,
      keySpec = Prelude.Nothing,
      keyState = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      origin = Prelude.Nothing,
      macAlgorithms = Prelude.Nothing,
      signingAlgorithms = Prelude.Nothing,
      validTo = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | The twelve-digit account ID of the Amazon Web Services account that owns
-- the KMS key.
keyMetadata_aWSAccountId :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_aWSAccountId = Lens.lens (\KeyMetadata' {aWSAccountId} -> aWSAccountId) (\s@KeyMetadata' {} a -> s {aWSAccountId = a} :: KeyMetadata)

-- | Specifies whether the KMS key\'s key material expires. This value is
-- present only when @Origin@ is @EXTERNAL@, otherwise this value is
-- omitted.
keyMetadata_expirationModel :: Lens.Lens' KeyMetadata (Prelude.Maybe ExpirationModelType)
keyMetadata_expirationModel = Lens.lens (\KeyMetadata' {expirationModel} -> expirationModel) (\s@KeyMetadata' {} a -> s {expirationModel = a} :: KeyMetadata)

-- | The encryption algorithms that the KMS key supports. You cannot use the
-- KMS key with other encryption algorithms within KMS.
--
-- This value is present only when the @KeyUsage@ of the KMS key is
-- @ENCRYPT_DECRYPT@.
keyMetadata_encryptionAlgorithms :: Lens.Lens' KeyMetadata (Prelude.Maybe [EncryptionAlgorithmSpec])
keyMetadata_encryptionAlgorithms = Lens.lens (\KeyMetadata' {encryptionAlgorithms} -> encryptionAlgorithms) (\s@KeyMetadata' {} a -> s {encryptionAlgorithms = a} :: KeyMetadata) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- that contains the KMS key. This value is present only when the KMS key
-- is created in a custom key store.
keyMetadata_customKeyStoreId :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_customKeyStoreId = Lens.lens (\KeyMetadata' {customKeyStoreId} -> customKeyStoreId) (\s@KeyMetadata' {} a -> s {customKeyStoreId = a} :: KeyMetadata)

-- | The waiting period before the primary key in a multi-Region key is
-- deleted. This waiting period begins when the last of its replica keys is
-- deleted. This value is present only when the @KeyState@ of the KMS key
-- is @PendingReplicaDeletion@. That indicates that the KMS key is the
-- primary key in a multi-Region key, it is scheduled for deletion, and it
-- still has existing replica keys.
--
-- When a single-Region KMS key or a multi-Region replica key is scheduled
-- for deletion, its deletion date is displayed in the @DeletionDate@
-- field. However, when the primary key in a multi-Region key is scheduled
-- for deletion, its waiting period doesn\'t begin until all of its replica
-- keys are deleted. This value displays that waiting period. When the last
-- replica key in the multi-Region key is deleted, the @KeyState@ of the
-- scheduled primary key changes from @PendingReplicaDeletion@ to
-- @PendingDeletion@ and the deletion date appears in the @DeletionDate@
-- field.
keyMetadata_pendingDeletionWindowInDays :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Natural)
keyMetadata_pendingDeletionWindowInDays = Lens.lens (\KeyMetadata' {pendingDeletionWindowInDays} -> pendingDeletionWindowInDays) (\s@KeyMetadata' {} a -> s {pendingDeletionWindowInDays = a} :: KeyMetadata)

-- | The Amazon Resource Name (ARN) of the KMS key. For examples, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Key Management Service (KMS)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
keyMetadata_arn :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_arn = Lens.lens (\KeyMetadata' {arn} -> arn) (\s@KeyMetadata' {} a -> s {arn = a} :: KeyMetadata)

-- | Lists the primary and replica keys in same multi-Region key. This field
-- is present only when the value of the @MultiRegion@ field is @True@.
--
-- For more information about any listed KMS key, use the DescribeKey
-- operation.
--
-- -   @MultiRegionKeyType@ indicates whether the KMS key is a @PRIMARY@ or
--     @REPLICA@ key.
--
-- -   @PrimaryKey@ displays the key ARN and Region of the primary key.
--     This field displays the current KMS key if it is the primary key.
--
-- -   @ReplicaKeys@ displays the key ARNs and Regions of all replica keys.
--     This field includes the current KMS key if it is a replica key.
keyMetadata_multiRegionConfiguration :: Lens.Lens' KeyMetadata (Prelude.Maybe MultiRegionConfiguration)
keyMetadata_multiRegionConfiguration = Lens.lens (\KeyMetadata' {multiRegionConfiguration} -> multiRegionConfiguration) (\s@KeyMetadata' {} a -> s {multiRegionConfiguration = a} :: KeyMetadata)

-- | The date and time when the KMS key was created.
keyMetadata_creationDate :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.UTCTime)
keyMetadata_creationDate = Lens.lens (\KeyMetadata' {creationDate} -> creationDate) (\s@KeyMetadata' {} a -> s {creationDate = a} :: KeyMetadata) Prelude.. Lens.mapping Data._Time

-- | Instead, use the @KeySpec@ field.
--
-- The @KeySpec@ and @CustomerMasterKeySpec@ fields have the same value. We
-- recommend that you use the @KeySpec@ field in your code. However, to
-- avoid breaking changes, KMS will support both fields.
keyMetadata_customerMasterKeySpec :: Lens.Lens' KeyMetadata (Prelude.Maybe CustomerMasterKeySpec)
keyMetadata_customerMasterKeySpec = Lens.lens (\KeyMetadata' {customerMasterKeySpec} -> customerMasterKeySpec) (\s@KeyMetadata' {} a -> s {customerMasterKeySpec = a} :: KeyMetadata)

-- | The
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- for which you can use the KMS key.
keyMetadata_keyUsage :: Lens.Lens' KeyMetadata (Prelude.Maybe KeyUsageType)
keyMetadata_keyUsage = Lens.lens (\KeyMetadata' {keyUsage} -> keyUsage) (\s@KeyMetadata' {} a -> s {keyUsage = a} :: KeyMetadata)

-- | The description of the KMS key.
keyMetadata_description :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_description = Lens.lens (\KeyMetadata' {description} -> description) (\s@KeyMetadata' {} a -> s {description = a} :: KeyMetadata)

-- | Indicates whether the KMS key is a multi-Region (@True@) or regional
-- (@False@) key. This value is @True@ for multi-Region primary and replica
-- keys and @False@ for regional KMS keys.
--
-- For more information about multi-Region keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
-- in the /Key Management Service Developer Guide/.
keyMetadata_multiRegion :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Bool)
keyMetadata_multiRegion = Lens.lens (\KeyMetadata' {multiRegion} -> multiRegion) (\s@KeyMetadata' {} a -> s {multiRegion = a} :: KeyMetadata)

-- | Specifies whether the KMS key is enabled. When @KeyState@ is @Enabled@
-- this value is true, otherwise it is false.
keyMetadata_enabled :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Bool)
keyMetadata_enabled = Lens.lens (\KeyMetadata' {enabled} -> enabled) (\s@KeyMetadata' {} a -> s {enabled = a} :: KeyMetadata)

-- | The manager of the KMS key. KMS keys in your Amazon Web Services account
-- are either customer managed or Amazon Web Services managed. For more
-- information about the difference, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#kms_keys KMS keys>
-- in the /Key Management Service Developer Guide/.
keyMetadata_keyManager :: Lens.Lens' KeyMetadata (Prelude.Maybe KeyManagerType)
keyMetadata_keyManager = Lens.lens (\KeyMetadata' {keyManager} -> keyManager) (\s@KeyMetadata' {} a -> s {keyManager = a} :: KeyMetadata)

-- | The cluster ID of the CloudHSM cluster that contains the key material
-- for the KMS key. When you create a KMS key in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
-- KMS creates the key material for the KMS key in the associated CloudHSM
-- cluster. This value is present only when the KMS key is created in a
-- custom key store.
keyMetadata_cloudHsmClusterId :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_cloudHsmClusterId = Lens.lens (\KeyMetadata' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@KeyMetadata' {} a -> s {cloudHsmClusterId = a} :: KeyMetadata)

-- | Describes the type of key material in the KMS key.
keyMetadata_keySpec :: Lens.Lens' KeyMetadata (Prelude.Maybe KeySpec)
keyMetadata_keySpec = Lens.lens (\KeyMetadata' {keySpec} -> keySpec) (\s@KeyMetadata' {} a -> s {keySpec = a} :: KeyMetadata)

-- | The current status of the KMS key.
--
-- For more information about how key state affects the use of a KMS key,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
keyMetadata_keyState :: Lens.Lens' KeyMetadata (Prelude.Maybe KeyState)
keyMetadata_keyState = Lens.lens (\KeyMetadata' {keyState} -> keyState) (\s@KeyMetadata' {} a -> s {keyState = a} :: KeyMetadata)

-- | The date and time after which KMS deletes this KMS key. This value is
-- present only when the KMS key is scheduled for deletion, that is, when
-- its @KeyState@ is @PendingDeletion@.
--
-- When the primary key in a multi-Region key is scheduled for deletion but
-- still has replica keys, its key state is @PendingReplicaDeletion@ and
-- the length of its waiting period is displayed in the
-- @PendingDeletionWindowInDays@ field.
keyMetadata_deletionDate :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.UTCTime)
keyMetadata_deletionDate = Lens.lens (\KeyMetadata' {deletionDate} -> deletionDate) (\s@KeyMetadata' {} a -> s {deletionDate = a} :: KeyMetadata) Prelude.. Lens.mapping Data._Time

-- | The source of the key material for the KMS key. When this value is
-- @AWS_KMS@, KMS created the key material. When this value is @EXTERNAL@,
-- the key material was imported or the KMS key doesn\'t have any key
-- material. When this value is @AWS_CLOUDHSM@, the key material was
-- created in the CloudHSM cluster associated with a custom key store.
keyMetadata_origin :: Lens.Lens' KeyMetadata (Prelude.Maybe OriginType)
keyMetadata_origin = Lens.lens (\KeyMetadata' {origin} -> origin) (\s@KeyMetadata' {} a -> s {origin = a} :: KeyMetadata)

-- | The message authentication code (MAC) algorithm that the HMAC KMS key
-- supports.
--
-- This value is present only when the @KeyUsage@ of the KMS key is
-- @GENERATE_VERIFY_MAC@.
keyMetadata_macAlgorithms :: Lens.Lens' KeyMetadata (Prelude.Maybe [MacAlgorithmSpec])
keyMetadata_macAlgorithms = Lens.lens (\KeyMetadata' {macAlgorithms} -> macAlgorithms) (\s@KeyMetadata' {} a -> s {macAlgorithms = a} :: KeyMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The signing algorithms that the KMS key supports. You cannot use the KMS
-- key with other signing algorithms within KMS.
--
-- This field appears only when the @KeyUsage@ of the KMS key is
-- @SIGN_VERIFY@.
keyMetadata_signingAlgorithms :: Lens.Lens' KeyMetadata (Prelude.Maybe [SigningAlgorithmSpec])
keyMetadata_signingAlgorithms = Lens.lens (\KeyMetadata' {signingAlgorithms} -> signingAlgorithms) (\s@KeyMetadata' {} a -> s {signingAlgorithms = a} :: KeyMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the imported key material expires. When the key
-- material expires, KMS deletes the key material and the KMS key becomes
-- unusable. This value is present only for KMS keys whose @Origin@ is
-- @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@,
-- otherwise this value is omitted.
keyMetadata_validTo :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.UTCTime)
keyMetadata_validTo = Lens.lens (\KeyMetadata' {validTo} -> validTo) (\s@KeyMetadata' {} a -> s {validTo = a} :: KeyMetadata) Prelude.. Lens.mapping Data._Time

-- | The globally unique identifier for the KMS key.
keyMetadata_keyId :: Lens.Lens' KeyMetadata Prelude.Text
keyMetadata_keyId = Lens.lens (\KeyMetadata' {keyId} -> keyId) (\s@KeyMetadata' {} a -> s {keyId = a} :: KeyMetadata)

instance Data.FromJSON KeyMetadata where
  parseJSON =
    Data.withObject
      "KeyMetadata"
      ( \x ->
          KeyMetadata'
            Prelude.<$> (x Data..:? "AWSAccountId")
            Prelude.<*> (x Data..:? "ExpirationModel")
            Prelude.<*> ( x Data..:? "EncryptionAlgorithms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CustomKeyStoreId")
            Prelude.<*> (x Data..:? "PendingDeletionWindowInDays")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "MultiRegionConfiguration")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "CustomerMasterKeySpec")
            Prelude.<*> (x Data..:? "KeyUsage")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "MultiRegion")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "KeyManager")
            Prelude.<*> (x Data..:? "CloudHsmClusterId")
            Prelude.<*> (x Data..:? "KeySpec")
            Prelude.<*> (x Data..:? "KeyState")
            Prelude.<*> (x Data..:? "DeletionDate")
            Prelude.<*> (x Data..:? "Origin")
            Prelude.<*> (x Data..:? "MacAlgorithms" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "SigningAlgorithms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ValidTo")
            Prelude.<*> (x Data..: "KeyId")
      )

instance Prelude.Hashable KeyMetadata where
  hashWithSalt _salt KeyMetadata' {..} =
    _salt `Prelude.hashWithSalt` aWSAccountId
      `Prelude.hashWithSalt` expirationModel
      `Prelude.hashWithSalt` encryptionAlgorithms
      `Prelude.hashWithSalt` customKeyStoreId
      `Prelude.hashWithSalt` pendingDeletionWindowInDays
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` multiRegionConfiguration
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` customerMasterKeySpec
      `Prelude.hashWithSalt` keyUsage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` multiRegion
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` keyManager
      `Prelude.hashWithSalt` cloudHsmClusterId
      `Prelude.hashWithSalt` keySpec
      `Prelude.hashWithSalt` keyState
      `Prelude.hashWithSalt` deletionDate
      `Prelude.hashWithSalt` origin
      `Prelude.hashWithSalt` macAlgorithms
      `Prelude.hashWithSalt` signingAlgorithms
      `Prelude.hashWithSalt` validTo
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData KeyMetadata where
  rnf KeyMetadata' {..} =
    Prelude.rnf aWSAccountId
      `Prelude.seq` Prelude.rnf expirationModel
      `Prelude.seq` Prelude.rnf encryptionAlgorithms
      `Prelude.seq` Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf pendingDeletionWindowInDays
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf multiRegionConfiguration
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf customerMasterKeySpec
      `Prelude.seq` Prelude.rnf keyUsage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf multiRegion
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf keyManager
      `Prelude.seq` Prelude.rnf cloudHsmClusterId
      `Prelude.seq` Prelude.rnf keySpec
      `Prelude.seq` Prelude.rnf keyState
      `Prelude.seq` Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf origin
      `Prelude.seq` Prelude.rnf macAlgorithms
      `Prelude.seq` Prelude.rnf
        signingAlgorithms
      `Prelude.seq` Prelude.rnf validTo
      `Prelude.seq` Prelude.rnf keyId

{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.Types.KeyMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyMetadata where

import Network.AWS.KMS.Types.CustomerMasterKeySpec
import Network.AWS.KMS.Types.EncryptionAlgorithmSpec
import Network.AWS.KMS.Types.ExpirationModelType
import Network.AWS.KMS.Types.KeyManagerType
import Network.AWS.KMS.Types.KeyState
import Network.AWS.KMS.Types.KeyUsageType
import Network.AWS.KMS.Types.OriginType
import Network.AWS.KMS.Types.SigningAlgorithmSpec
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains metadata about a customer master key (CMK).
--
-- This data type is used as a response element for the CreateKey and
-- DescribeKey operations.
--
-- /See:/ 'newKeyMetadata' smart constructor.
data KeyMetadata = KeyMetadata'
  { -- | The signing algorithms that the CMK supports. You cannot use the CMK
    -- with other signing algorithms within AWS KMS.
    --
    -- This field appears only when the @KeyUsage@ of the CMK is @SIGN_VERIFY@.
    signingAlgorithms :: Prelude.Maybe [SigningAlgorithmSpec],
    -- | The manager of the CMK. CMKs in your AWS account are either customer
    -- managed or AWS managed. For more information about the difference, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys>
    -- in the /AWS Key Management Service Developer Guide/.
    keyManager :: Prelude.Maybe KeyManagerType,
    -- | The source of the CMK\'s key material. When this value is @AWS_KMS@, AWS
    -- KMS created the key material. When this value is @EXTERNAL@, the key
    -- material was imported from your existing key management infrastructure
    -- or the CMK lacks key material. When this value is @AWS_CLOUDHSM@, the
    -- key material was created in the AWS CloudHSM cluster associated with a
    -- custom key store.
    origin :: Prelude.Maybe OriginType,
    -- | The twelve-digit account ID of the AWS account that owns the CMK.
    aWSAccountId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
    -- that contains the CMK. This value is present only when the CMK is
    -- created in a custom key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The encryption algorithms that the CMK supports. You cannot use the CMK
    -- with other encryption algorithms within AWS KMS.
    --
    -- This field appears only when the @KeyUsage@ of the CMK is
    -- @ENCRYPT_DECRYPT@.
    encryptionAlgorithms :: Prelude.Maybe [EncryptionAlgorithmSpec],
    -- | The cluster ID of the AWS CloudHSM cluster that contains the key
    -- material for the CMK. When you create a CMK in a
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
    -- AWS KMS creates the key material for the CMK in the associated AWS
    -- CloudHSM cluster. This value is present only when the CMK is created in
    -- a custom key store.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the CMK.
    --
    -- For more information about how key state affects the use of a CMK, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your CMK>
    -- in the /AWS Key Management Service Developer Guide/.
    keyState :: Prelude.Maybe KeyState,
    -- | The Amazon Resource Name (ARN) of the CMK. For examples, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)>
    -- in the Example ARNs section of the /AWS General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the CMK was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The time at which the imported key material expires. When the key
    -- material expires, AWS KMS deletes the key material and the CMK becomes
    -- unusable. This value is present only for CMKs whose @Origin@ is
    -- @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@,
    -- otherwise this value is omitted.
    validTo :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this
    -- value is true, otherwise it is false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the CMK\'s key material expires. This value is present
    -- only when @Origin@ is @EXTERNAL@, otherwise this value is omitted.
    expirationModel :: Prelude.Maybe ExpirationModelType,
    -- | The description of the CMK.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time after which AWS KMS deletes the CMK. This value is
    -- present only when @KeyState@ is @PendingDeletion@.
    deletionDate :: Prelude.Maybe Prelude.POSIX,
    -- | The
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
    -- for which you can use the CMK.
    keyUsage :: Prelude.Maybe KeyUsageType,
    -- | Describes the type of key material in the CMK.
    customerMasterKeySpec :: Prelude.Maybe CustomerMasterKeySpec,
    -- | The globally unique identifier for the CMK.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingAlgorithms', 'keyMetadata_signingAlgorithms' - The signing algorithms that the CMK supports. You cannot use the CMK
-- with other signing algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @SIGN_VERIFY@.
--
-- 'keyManager', 'keyMetadata_keyManager' - The manager of the CMK. CMKs in your AWS account are either customer
-- managed or AWS managed. For more information about the difference, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'origin', 'keyMetadata_origin' - The source of the CMK\'s key material. When this value is @AWS_KMS@, AWS
-- KMS created the key material. When this value is @EXTERNAL@, the key
-- material was imported from your existing key management infrastructure
-- or the CMK lacks key material. When this value is @AWS_CLOUDHSM@, the
-- key material was created in the AWS CloudHSM cluster associated with a
-- custom key store.
--
-- 'aWSAccountId', 'keyMetadata_aWSAccountId' - The twelve-digit account ID of the AWS account that owns the CMK.
--
-- 'customKeyStoreId', 'keyMetadata_customKeyStoreId' - A unique identifier for the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- that contains the CMK. This value is present only when the CMK is
-- created in a custom key store.
--
-- 'encryptionAlgorithms', 'keyMetadata_encryptionAlgorithms' - The encryption algorithms that the CMK supports. You cannot use the CMK
-- with other encryption algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is
-- @ENCRYPT_DECRYPT@.
--
-- 'cloudHsmClusterId', 'keyMetadata_cloudHsmClusterId' - The cluster ID of the AWS CloudHSM cluster that contains the key
-- material for the CMK. When you create a CMK in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
-- AWS KMS creates the key material for the CMK in the associated AWS
-- CloudHSM cluster. This value is present only when the CMK is created in
-- a custom key store.
--
-- 'keyState', 'keyMetadata_keyState' - The current status of the CMK.
--
-- For more information about how key state affects the use of a CMK, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your CMK>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'arn', 'keyMetadata_arn' - The Amazon Resource Name (ARN) of the CMK. For examples, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)>
-- in the Example ARNs section of the /AWS General Reference/.
--
-- 'creationDate', 'keyMetadata_creationDate' - The date and time when the CMK was created.
--
-- 'validTo', 'keyMetadata_validTo' - The time at which the imported key material expires. When the key
-- material expires, AWS KMS deletes the key material and the CMK becomes
-- unusable. This value is present only for CMKs whose @Origin@ is
-- @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@,
-- otherwise this value is omitted.
--
-- 'enabled', 'keyMetadata_enabled' - Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this
-- value is true, otherwise it is false.
--
-- 'expirationModel', 'keyMetadata_expirationModel' - Specifies whether the CMK\'s key material expires. This value is present
-- only when @Origin@ is @EXTERNAL@, otherwise this value is omitted.
--
-- 'description', 'keyMetadata_description' - The description of the CMK.
--
-- 'deletionDate', 'keyMetadata_deletionDate' - The date and time after which AWS KMS deletes the CMK. This value is
-- present only when @KeyState@ is @PendingDeletion@.
--
-- 'keyUsage', 'keyMetadata_keyUsage' - The
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- for which you can use the CMK.
--
-- 'customerMasterKeySpec', 'keyMetadata_customerMasterKeySpec' - Describes the type of key material in the CMK.
--
-- 'keyId', 'keyMetadata_keyId' - The globally unique identifier for the CMK.
newKeyMetadata ::
  -- | 'keyId'
  Prelude.Text ->
  KeyMetadata
newKeyMetadata pKeyId_ =
  KeyMetadata'
    { signingAlgorithms = Prelude.Nothing,
      keyManager = Prelude.Nothing,
      origin = Prelude.Nothing,
      aWSAccountId = Prelude.Nothing,
      customKeyStoreId = Prelude.Nothing,
      encryptionAlgorithms = Prelude.Nothing,
      cloudHsmClusterId = Prelude.Nothing,
      keyState = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      validTo = Prelude.Nothing,
      enabled = Prelude.Nothing,
      expirationModel = Prelude.Nothing,
      description = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      keyUsage = Prelude.Nothing,
      customerMasterKeySpec = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | The signing algorithms that the CMK supports. You cannot use the CMK
-- with other signing algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @SIGN_VERIFY@.
keyMetadata_signingAlgorithms :: Lens.Lens' KeyMetadata (Prelude.Maybe [SigningAlgorithmSpec])
keyMetadata_signingAlgorithms = Lens.lens (\KeyMetadata' {signingAlgorithms} -> signingAlgorithms) (\s@KeyMetadata' {} a -> s {signingAlgorithms = a} :: KeyMetadata) Prelude.. Lens.mapping Prelude._Coerce

-- | The manager of the CMK. CMKs in your AWS account are either customer
-- managed or AWS managed. For more information about the difference, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys>
-- in the /AWS Key Management Service Developer Guide/.
keyMetadata_keyManager :: Lens.Lens' KeyMetadata (Prelude.Maybe KeyManagerType)
keyMetadata_keyManager = Lens.lens (\KeyMetadata' {keyManager} -> keyManager) (\s@KeyMetadata' {} a -> s {keyManager = a} :: KeyMetadata)

-- | The source of the CMK\'s key material. When this value is @AWS_KMS@, AWS
-- KMS created the key material. When this value is @EXTERNAL@, the key
-- material was imported from your existing key management infrastructure
-- or the CMK lacks key material. When this value is @AWS_CLOUDHSM@, the
-- key material was created in the AWS CloudHSM cluster associated with a
-- custom key store.
keyMetadata_origin :: Lens.Lens' KeyMetadata (Prelude.Maybe OriginType)
keyMetadata_origin = Lens.lens (\KeyMetadata' {origin} -> origin) (\s@KeyMetadata' {} a -> s {origin = a} :: KeyMetadata)

-- | The twelve-digit account ID of the AWS account that owns the CMK.
keyMetadata_aWSAccountId :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_aWSAccountId = Lens.lens (\KeyMetadata' {aWSAccountId} -> aWSAccountId) (\s@KeyMetadata' {} a -> s {aWSAccountId = a} :: KeyMetadata)

-- | A unique identifier for the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- that contains the CMK. This value is present only when the CMK is
-- created in a custom key store.
keyMetadata_customKeyStoreId :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_customKeyStoreId = Lens.lens (\KeyMetadata' {customKeyStoreId} -> customKeyStoreId) (\s@KeyMetadata' {} a -> s {customKeyStoreId = a} :: KeyMetadata)

-- | The encryption algorithms that the CMK supports. You cannot use the CMK
-- with other encryption algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is
-- @ENCRYPT_DECRYPT@.
keyMetadata_encryptionAlgorithms :: Lens.Lens' KeyMetadata (Prelude.Maybe [EncryptionAlgorithmSpec])
keyMetadata_encryptionAlgorithms = Lens.lens (\KeyMetadata' {encryptionAlgorithms} -> encryptionAlgorithms) (\s@KeyMetadata' {} a -> s {encryptionAlgorithms = a} :: KeyMetadata) Prelude.. Lens.mapping Prelude._Coerce

-- | The cluster ID of the AWS CloudHSM cluster that contains the key
-- material for the CMK. When you create a CMK in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
-- AWS KMS creates the key material for the CMK in the associated AWS
-- CloudHSM cluster. This value is present only when the CMK is created in
-- a custom key store.
keyMetadata_cloudHsmClusterId :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_cloudHsmClusterId = Lens.lens (\KeyMetadata' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@KeyMetadata' {} a -> s {cloudHsmClusterId = a} :: KeyMetadata)

-- | The current status of the CMK.
--
-- For more information about how key state affects the use of a CMK, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your CMK>
-- in the /AWS Key Management Service Developer Guide/.
keyMetadata_keyState :: Lens.Lens' KeyMetadata (Prelude.Maybe KeyState)
keyMetadata_keyState = Lens.lens (\KeyMetadata' {keyState} -> keyState) (\s@KeyMetadata' {} a -> s {keyState = a} :: KeyMetadata)

-- | The Amazon Resource Name (ARN) of the CMK. For examples, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)>
-- in the Example ARNs section of the /AWS General Reference/.
keyMetadata_arn :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_arn = Lens.lens (\KeyMetadata' {arn} -> arn) (\s@KeyMetadata' {} a -> s {arn = a} :: KeyMetadata)

-- | The date and time when the CMK was created.
keyMetadata_creationDate :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.UTCTime)
keyMetadata_creationDate = Lens.lens (\KeyMetadata' {creationDate} -> creationDate) (\s@KeyMetadata' {} a -> s {creationDate = a} :: KeyMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The time at which the imported key material expires. When the key
-- material expires, AWS KMS deletes the key material and the CMK becomes
-- unusable. This value is present only for CMKs whose @Origin@ is
-- @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@,
-- otherwise this value is omitted.
keyMetadata_validTo :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.UTCTime)
keyMetadata_validTo = Lens.lens (\KeyMetadata' {validTo} -> validTo) (\s@KeyMetadata' {} a -> s {validTo = a} :: KeyMetadata) Prelude.. Lens.mapping Prelude._Time

-- | Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this
-- value is true, otherwise it is false.
keyMetadata_enabled :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Bool)
keyMetadata_enabled = Lens.lens (\KeyMetadata' {enabled} -> enabled) (\s@KeyMetadata' {} a -> s {enabled = a} :: KeyMetadata)

-- | Specifies whether the CMK\'s key material expires. This value is present
-- only when @Origin@ is @EXTERNAL@, otherwise this value is omitted.
keyMetadata_expirationModel :: Lens.Lens' KeyMetadata (Prelude.Maybe ExpirationModelType)
keyMetadata_expirationModel = Lens.lens (\KeyMetadata' {expirationModel} -> expirationModel) (\s@KeyMetadata' {} a -> s {expirationModel = a} :: KeyMetadata)

-- | The description of the CMK.
keyMetadata_description :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.Text)
keyMetadata_description = Lens.lens (\KeyMetadata' {description} -> description) (\s@KeyMetadata' {} a -> s {description = a} :: KeyMetadata)

-- | The date and time after which AWS KMS deletes the CMK. This value is
-- present only when @KeyState@ is @PendingDeletion@.
keyMetadata_deletionDate :: Lens.Lens' KeyMetadata (Prelude.Maybe Prelude.UTCTime)
keyMetadata_deletionDate = Lens.lens (\KeyMetadata' {deletionDate} -> deletionDate) (\s@KeyMetadata' {} a -> s {deletionDate = a} :: KeyMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- for which you can use the CMK.
keyMetadata_keyUsage :: Lens.Lens' KeyMetadata (Prelude.Maybe KeyUsageType)
keyMetadata_keyUsage = Lens.lens (\KeyMetadata' {keyUsage} -> keyUsage) (\s@KeyMetadata' {} a -> s {keyUsage = a} :: KeyMetadata)

-- | Describes the type of key material in the CMK.
keyMetadata_customerMasterKeySpec :: Lens.Lens' KeyMetadata (Prelude.Maybe CustomerMasterKeySpec)
keyMetadata_customerMasterKeySpec = Lens.lens (\KeyMetadata' {customerMasterKeySpec} -> customerMasterKeySpec) (\s@KeyMetadata' {} a -> s {customerMasterKeySpec = a} :: KeyMetadata)

-- | The globally unique identifier for the CMK.
keyMetadata_keyId :: Lens.Lens' KeyMetadata Prelude.Text
keyMetadata_keyId = Lens.lens (\KeyMetadata' {keyId} -> keyId) (\s@KeyMetadata' {} a -> s {keyId = a} :: KeyMetadata)

instance Prelude.FromJSON KeyMetadata where
  parseJSON =
    Prelude.withObject
      "KeyMetadata"
      ( \x ->
          KeyMetadata'
            Prelude.<$> ( x Prelude..:? "SigningAlgorithms"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "KeyManager")
            Prelude.<*> (x Prelude..:? "Origin")
            Prelude.<*> (x Prelude..:? "AWSAccountId")
            Prelude.<*> (x Prelude..:? "CustomKeyStoreId")
            Prelude.<*> ( x Prelude..:? "EncryptionAlgorithms"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CloudHsmClusterId")
            Prelude.<*> (x Prelude..:? "KeyState")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "ValidTo")
            Prelude.<*> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "ExpirationModel")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "DeletionDate")
            Prelude.<*> (x Prelude..:? "KeyUsage")
            Prelude.<*> (x Prelude..:? "CustomerMasterKeySpec")
            Prelude.<*> (x Prelude..: "KeyId")
      )

instance Prelude.Hashable KeyMetadata

instance Prelude.NFData KeyMetadata

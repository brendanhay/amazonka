-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyMetadata
  ( KeyMetadata (..),

    -- * Smart constructor
    mkKeyMetadata,

    -- * Lenses
    kmOrigin,
    kmExpirationModel,
    kmKeyManager,
    kmCustomerMasterKeySpec,
    kmEnabled,
    kmValidTo,
    kmARN,
    kmKeyState,
    kmEncryptionAlgorithms,
    kmAWSAccountId,
    kmSigningAlgorithms,
    kmKeyUsage,
    kmCreationDate,
    kmDeletionDate,
    kmCloudHSMClusterId,
    kmDescription,
    kmCustomKeyStoreId,
    kmKeyId,
  )
where

import Network.AWS.KMS.Types.CustomerMasterKeySpec
import Network.AWS.KMS.Types.EncryptionAlgorithmSpec
import Network.AWS.KMS.Types.ExpirationModelType
import Network.AWS.KMS.Types.KeyManagerType
import Network.AWS.KMS.Types.KeyState
import Network.AWS.KMS.Types.KeyUsageType
import Network.AWS.KMS.Types.OriginType
import Network.AWS.KMS.Types.SigningAlgorithmSpec
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains metadata about a customer master key (CMK).
--
-- This data type is used as a response element for the 'CreateKey' and 'DescribeKey' operations.
--
-- /See:/ 'mkKeyMetadata' smart constructor.
data KeyMetadata = KeyMetadata'
  { origin :: Lude.Maybe OriginType,
    expirationModel :: Lude.Maybe ExpirationModelType,
    keyManager :: Lude.Maybe KeyManagerType,
    customerMasterKeySpec :: Lude.Maybe CustomerMasterKeySpec,
    enabled :: Lude.Maybe Lude.Bool,
    validTo :: Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    keyState :: Lude.Maybe KeyState,
    encryptionAlgorithms :: Lude.Maybe [EncryptionAlgorithmSpec],
    awsAccountId :: Lude.Maybe Lude.Text,
    signingAlgorithms :: Lude.Maybe [SigningAlgorithmSpec],
    keyUsage :: Lude.Maybe KeyUsageType,
    creationDate :: Lude.Maybe Lude.Timestamp,
    deletionDate :: Lude.Maybe Lude.Timestamp,
    cloudHSMClusterId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    customKeyStoreId :: Lude.Maybe Lude.Text,
    keyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyMetadata' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the CMK. For examples, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
-- * 'awsAccountId' - The twelve-digit account ID of the AWS account that owns the CMK.
-- * 'cloudHSMClusterId' - The cluster ID of the AWS CloudHSM cluster that contains the key material for the CMK. When you create a CMK in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , AWS KMS creates the key material for the CMK in the associated AWS CloudHSM cluster. This value is present only when the CMK is created in a custom key store.
-- * 'creationDate' - The date and time when the CMK was created.
-- * 'customKeyStoreId' - A unique identifier for the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> that contains the CMK. This value is present only when the CMK is created in a custom key store.
-- * 'customerMasterKeySpec' - Describes the type of key material in the CMK.
-- * 'deletionDate' - The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ .
-- * 'description' - The description of the CMK.
-- * 'enabled' - Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
-- * 'encryptionAlgorithms' - The encryption algorithms that the CMK supports. You cannot use the CMK with other encryption algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @ENCRYPT_DECRYPT@ .
-- * 'expirationModel' - Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
-- * 'keyId' - The globally unique identifier for the CMK.
-- * 'keyManager' - The manager of the CMK. CMKs in your AWS account are either customer managed or AWS managed. For more information about the difference, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyState' - The current status of the CMK.
--
-- For more information about how key state affects the use of a CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your CMK> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyUsage' - The <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> for which you can use the CMK.
-- * 'origin' - The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material. When this value is @AWS_CLOUDHSM@ , the key material was created in the AWS CloudHSM cluster associated with a custom key store.
-- * 'signingAlgorithms' - The signing algorithms that the CMK supports. You cannot use the CMK with other signing algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @SIGN_VERIFY@ .
-- * 'validTo' - The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
mkKeyMetadata ::
  -- | 'keyId'
  Lude.Text ->
  KeyMetadata
mkKeyMetadata pKeyId_ =
  KeyMetadata'
    { origin = Lude.Nothing,
      expirationModel = Lude.Nothing,
      keyManager = Lude.Nothing,
      customerMasterKeySpec = Lude.Nothing,
      enabled = Lude.Nothing,
      validTo = Lude.Nothing,
      arn = Lude.Nothing,
      keyState = Lude.Nothing,
      encryptionAlgorithms = Lude.Nothing,
      awsAccountId = Lude.Nothing,
      signingAlgorithms = Lude.Nothing,
      keyUsage = Lude.Nothing,
      creationDate = Lude.Nothing,
      deletionDate = Lude.Nothing,
      cloudHSMClusterId = Lude.Nothing,
      description = Lude.Nothing,
      customKeyStoreId = Lude.Nothing,
      keyId = pKeyId_
    }

-- | The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material. When this value is @AWS_CLOUDHSM@ , the key material was created in the AWS CloudHSM cluster associated with a custom key store.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmOrigin :: Lens.Lens' KeyMetadata (Lude.Maybe OriginType)
kmOrigin = Lens.lens (origin :: KeyMetadata -> Lude.Maybe OriginType) (\s a -> s {origin = a} :: KeyMetadata)
{-# DEPRECATED kmOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
--
-- /Note:/ Consider using 'expirationModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmExpirationModel :: Lens.Lens' KeyMetadata (Lude.Maybe ExpirationModelType)
kmExpirationModel = Lens.lens (expirationModel :: KeyMetadata -> Lude.Maybe ExpirationModelType) (\s a -> s {expirationModel = a} :: KeyMetadata)
{-# DEPRECATED kmExpirationModel "Use generic-lens or generic-optics with 'expirationModel' instead." #-}

-- | The manager of the CMK. CMKs in your AWS account are either customer managed or AWS managed. For more information about the difference, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'keyManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyManager :: Lens.Lens' KeyMetadata (Lude.Maybe KeyManagerType)
kmKeyManager = Lens.lens (keyManager :: KeyMetadata -> Lude.Maybe KeyManagerType) (\s a -> s {keyManager = a} :: KeyMetadata)
{-# DEPRECATED kmKeyManager "Use generic-lens or generic-optics with 'keyManager' instead." #-}

-- | Describes the type of key material in the CMK.
--
-- /Note:/ Consider using 'customerMasterKeySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCustomerMasterKeySpec :: Lens.Lens' KeyMetadata (Lude.Maybe CustomerMasterKeySpec)
kmCustomerMasterKeySpec = Lens.lens (customerMasterKeySpec :: KeyMetadata -> Lude.Maybe CustomerMasterKeySpec) (\s a -> s {customerMasterKeySpec = a} :: KeyMetadata)
{-# DEPRECATED kmCustomerMasterKeySpec "Use generic-lens or generic-optics with 'customerMasterKeySpec' instead." #-}

-- | Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmEnabled :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Bool)
kmEnabled = Lens.lens (enabled :: KeyMetadata -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: KeyMetadata)
{-# DEPRECATED kmEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
--
-- /Note:/ Consider using 'validTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmValidTo :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Timestamp)
kmValidTo = Lens.lens (validTo :: KeyMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {validTo = a} :: KeyMetadata)
{-# DEPRECATED kmValidTo "Use generic-lens or generic-optics with 'validTo' instead." #-}

-- | The Amazon Resource Name (ARN) of the CMK. For examples, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmARN :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Text)
kmARN = Lens.lens (arn :: KeyMetadata -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: KeyMetadata)
{-# DEPRECATED kmARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The current status of the CMK.
--
-- For more information about how key state affects the use of a CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your CMK> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'keyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyState :: Lens.Lens' KeyMetadata (Lude.Maybe KeyState)
kmKeyState = Lens.lens (keyState :: KeyMetadata -> Lude.Maybe KeyState) (\s a -> s {keyState = a} :: KeyMetadata)
{-# DEPRECATED kmKeyState "Use generic-lens or generic-optics with 'keyState' instead." #-}

-- | The encryption algorithms that the CMK supports. You cannot use the CMK with other encryption algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @ENCRYPT_DECRYPT@ .
--
-- /Note:/ Consider using 'encryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmEncryptionAlgorithms :: Lens.Lens' KeyMetadata (Lude.Maybe [EncryptionAlgorithmSpec])
kmEncryptionAlgorithms = Lens.lens (encryptionAlgorithms :: KeyMetadata -> Lude.Maybe [EncryptionAlgorithmSpec]) (\s a -> s {encryptionAlgorithms = a} :: KeyMetadata)
{-# DEPRECATED kmEncryptionAlgorithms "Use generic-lens or generic-optics with 'encryptionAlgorithms' instead." #-}

-- | The twelve-digit account ID of the AWS account that owns the CMK.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmAWSAccountId :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Text)
kmAWSAccountId = Lens.lens (awsAccountId :: KeyMetadata -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: KeyMetadata)
{-# DEPRECATED kmAWSAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The signing algorithms that the CMK supports. You cannot use the CMK with other signing algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @SIGN_VERIFY@ .
--
-- /Note:/ Consider using 'signingAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmSigningAlgorithms :: Lens.Lens' KeyMetadata (Lude.Maybe [SigningAlgorithmSpec])
kmSigningAlgorithms = Lens.lens (signingAlgorithms :: KeyMetadata -> Lude.Maybe [SigningAlgorithmSpec]) (\s a -> s {signingAlgorithms = a} :: KeyMetadata)
{-# DEPRECATED kmSigningAlgorithms "Use generic-lens or generic-optics with 'signingAlgorithms' instead." #-}

-- | The <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> for which you can use the CMK.
--
-- /Note:/ Consider using 'keyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyUsage :: Lens.Lens' KeyMetadata (Lude.Maybe KeyUsageType)
kmKeyUsage = Lens.lens (keyUsage :: KeyMetadata -> Lude.Maybe KeyUsageType) (\s a -> s {keyUsage = a} :: KeyMetadata)
{-# DEPRECATED kmKeyUsage "Use generic-lens or generic-optics with 'keyUsage' instead." #-}

-- | The date and time when the CMK was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCreationDate :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Timestamp)
kmCreationDate = Lens.lens (creationDate :: KeyMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: KeyMetadata)
{-# DEPRECATED kmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ .
--
-- /Note:/ Consider using 'deletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmDeletionDate :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Timestamp)
kmDeletionDate = Lens.lens (deletionDate :: KeyMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {deletionDate = a} :: KeyMetadata)
{-# DEPRECATED kmDeletionDate "Use generic-lens or generic-optics with 'deletionDate' instead." #-}

-- | The cluster ID of the AWS CloudHSM cluster that contains the key material for the CMK. When you create a CMK in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , AWS KMS creates the key material for the CMK in the associated AWS CloudHSM cluster. This value is present only when the CMK is created in a custom key store.
--
-- /Note:/ Consider using 'cloudHSMClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCloudHSMClusterId :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Text)
kmCloudHSMClusterId = Lens.lens (cloudHSMClusterId :: KeyMetadata -> Lude.Maybe Lude.Text) (\s a -> s {cloudHSMClusterId = a} :: KeyMetadata)
{-# DEPRECATED kmCloudHSMClusterId "Use generic-lens or generic-optics with 'cloudHSMClusterId' instead." #-}

-- | The description of the CMK.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmDescription :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Text)
kmDescription = Lens.lens (description :: KeyMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: KeyMetadata)
{-# DEPRECATED kmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A unique identifier for the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> that contains the CMK. This value is present only when the CMK is created in a custom key store.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCustomKeyStoreId :: Lens.Lens' KeyMetadata (Lude.Maybe Lude.Text)
kmCustomKeyStoreId = Lens.lens (customKeyStoreId :: KeyMetadata -> Lude.Maybe Lude.Text) (\s a -> s {customKeyStoreId = a} :: KeyMetadata)
{-# DEPRECATED kmCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

-- | The globally unique identifier for the CMK.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyId :: Lens.Lens' KeyMetadata Lude.Text
kmKeyId = Lens.lens (keyId :: KeyMetadata -> Lude.Text) (\s a -> s {keyId = a} :: KeyMetadata)
{-# DEPRECATED kmKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.FromJSON KeyMetadata where
  parseJSON =
    Lude.withObject
      "KeyMetadata"
      ( \x ->
          KeyMetadata'
            Lude.<$> (x Lude..:? "Origin")
            Lude.<*> (x Lude..:? "ExpirationModel")
            Lude.<*> (x Lude..:? "KeyManager")
            Lude.<*> (x Lude..:? "CustomerMasterKeySpec")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "ValidTo")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "KeyState")
            Lude.<*> (x Lude..:? "EncryptionAlgorithms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AWSAccountId")
            Lude.<*> (x Lude..:? "SigningAlgorithms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "KeyUsage")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "DeletionDate")
            Lude.<*> (x Lude..:? "CloudHsmClusterId")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "CustomKeyStoreId")
            Lude.<*> (x Lude..: "KeyId")
      )

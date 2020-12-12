{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
  ( DeliveryStreamEncryptionConfigurationInput (..),

    -- * Smart constructor
    mkDeliveryStreamEncryptionConfigurationInput,

    -- * Lenses
    dseciKeyARN,
    dseciKeyType,
  )
where

import Network.AWS.Firehose.Types.KeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the type and Amazon Resource Name (ARN) of the CMK to use for Server-Side Encryption (SSE).
--
-- /See:/ 'mkDeliveryStreamEncryptionConfigurationInput' smart constructor.
data DeliveryStreamEncryptionConfigurationInput = DeliveryStreamEncryptionConfigurationInput'
  { keyARN ::
      Lude.Maybe
        Lude.Text,
    keyType ::
      KeyType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliveryStreamEncryptionConfigurationInput' with the minimum fields required to make a request.
--
-- * 'keyARN' - If you set @KeyType@ to @CUSTOMER_MANAGED_CMK@ , you must specify the Amazon Resource Name (ARN) of the CMK. If you set @KeyType@ to @AWS_OWNED_CMK@ , Kinesis Data Firehose uses a service-account CMK.
-- * 'keyType' - Indicates the type of customer master key (CMK) to use for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> . When you invoke 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' with @KeyType@ set to CUSTOMER_MANAGED_CMK, Kinesis Data Firehose invokes the Amazon KMS operation <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant> to create a grant that allows the Kinesis Data Firehose service to use the customer managed CMK to perform encryption and decryption. Kinesis Data Firehose manages that grant.
--
-- When you invoke 'StartDeliveryStreamEncryption' to change the CMK for a delivery stream that is encrypted with a customer managed CMK, Kinesis Data Firehose schedules the grant it had on the old CMK for retirement.
-- You can use a CMK of type CUSTOMER_MANAGED_CMK to encrypt up to 500 delivery streams. If a 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' operation exceeds this limit, Kinesis Data Firehose throws a @LimitExceededException@ .
-- /Important:/ To encrypt your delivery stream, use symmetric CMKs. Kinesis Data Firehose doesn't support asymmetric CMKs. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html About Symmetric and Asymmetric CMKs> in the AWS Key Management Service developer guide.
mkDeliveryStreamEncryptionConfigurationInput ::
  -- | 'keyType'
  KeyType ->
  DeliveryStreamEncryptionConfigurationInput
mkDeliveryStreamEncryptionConfigurationInput pKeyType_ =
  DeliveryStreamEncryptionConfigurationInput'
    { keyARN =
        Lude.Nothing,
      keyType = pKeyType_
    }

-- | If you set @KeyType@ to @CUSTOMER_MANAGED_CMK@ , you must specify the Amazon Resource Name (ARN) of the CMK. If you set @KeyType@ to @AWS_OWNED_CMK@ , Kinesis Data Firehose uses a service-account CMK.
--
-- /Note:/ Consider using 'keyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseciKeyARN :: Lens.Lens' DeliveryStreamEncryptionConfigurationInput (Lude.Maybe Lude.Text)
dseciKeyARN = Lens.lens (keyARN :: DeliveryStreamEncryptionConfigurationInput -> Lude.Maybe Lude.Text) (\s a -> s {keyARN = a} :: DeliveryStreamEncryptionConfigurationInput)
{-# DEPRECATED dseciKeyARN "Use generic-lens or generic-optics with 'keyARN' instead." #-}

-- | Indicates the type of customer master key (CMK) to use for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> . When you invoke 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' with @KeyType@ set to CUSTOMER_MANAGED_CMK, Kinesis Data Firehose invokes the Amazon KMS operation <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant> to create a grant that allows the Kinesis Data Firehose service to use the customer managed CMK to perform encryption and decryption. Kinesis Data Firehose manages that grant.
--
-- When you invoke 'StartDeliveryStreamEncryption' to change the CMK for a delivery stream that is encrypted with a customer managed CMK, Kinesis Data Firehose schedules the grant it had on the old CMK for retirement.
-- You can use a CMK of type CUSTOMER_MANAGED_CMK to encrypt up to 500 delivery streams. If a 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' operation exceeds this limit, Kinesis Data Firehose throws a @LimitExceededException@ .
-- /Important:/ To encrypt your delivery stream, use symmetric CMKs. Kinesis Data Firehose doesn't support asymmetric CMKs. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html About Symmetric and Asymmetric CMKs> in the AWS Key Management Service developer guide.
--
-- /Note:/ Consider using 'keyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseciKeyType :: Lens.Lens' DeliveryStreamEncryptionConfigurationInput KeyType
dseciKeyType = Lens.lens (keyType :: DeliveryStreamEncryptionConfigurationInput -> KeyType) (\s a -> s {keyType = a} :: DeliveryStreamEncryptionConfigurationInput)
{-# DEPRECATED dseciKeyType "Use generic-lens or generic-optics with 'keyType' instead." #-}

instance Lude.ToJSON DeliveryStreamEncryptionConfigurationInput where
  toJSON DeliveryStreamEncryptionConfigurationInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyARN" Lude..=) Lude.<$> keyARN,
            Lude.Just ("KeyType" Lude..= keyType)
          ]
      )

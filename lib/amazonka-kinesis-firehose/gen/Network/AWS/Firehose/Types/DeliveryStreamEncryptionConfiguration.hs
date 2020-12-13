{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
  ( DeliveryStreamEncryptionConfiguration (..),

    -- * Smart constructor
    mkDeliveryStreamEncryptionConfiguration,

    -- * Lenses
    dsecStatus,
    dsecKeyType,
    dsecKeyARN,
    dsecFailureDescription,
  )
where

import Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.KeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the server-side encryption (SSE) status for the delivery stream, the type customer master key (CMK) in use, if any, and the ARN of the CMK. You can get @DeliveryStreamEncryptionConfiguration@ by invoking the 'DescribeDeliveryStream' operation.
--
-- /See:/ 'mkDeliveryStreamEncryptionConfiguration' smart constructor.
data DeliveryStreamEncryptionConfiguration = DeliveryStreamEncryptionConfiguration'
  { -- | This is the server-side encryption (SSE) status for the delivery stream. For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' . If this status is @ENABLING_FAILED@ or @DISABLING_FAILED@ , it is the status of the most recent attempt to enable or disable SSE, respectively.
    status :: Lude.Maybe DeliveryStreamEncryptionStatus,
    -- | Indicates the type of customer master key (CMK) that is used for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> .
    keyType :: Lude.Maybe KeyType,
    -- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@ , this field contains the ARN of the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@ , @DeliveryStreamEncryptionConfiguration@ doesn't contain a value for @KeyARN@ .
    keyARN :: Lude.Maybe Lude.Text,
    -- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
    failureDescription :: Lude.Maybe FailureDescription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliveryStreamEncryptionConfiguration' with the minimum fields required to make a request.
--
-- * 'status' - This is the server-side encryption (SSE) status for the delivery stream. For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' . If this status is @ENABLING_FAILED@ or @DISABLING_FAILED@ , it is the status of the most recent attempt to enable or disable SSE, respectively.
-- * 'keyType' - Indicates the type of customer master key (CMK) that is used for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> .
-- * 'keyARN' - If @KeyType@ is @CUSTOMER_MANAGED_CMK@ , this field contains the ARN of the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@ , @DeliveryStreamEncryptionConfiguration@ doesn't contain a value for @KeyARN@ .
-- * 'failureDescription' - Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
mkDeliveryStreamEncryptionConfiguration ::
  DeliveryStreamEncryptionConfiguration
mkDeliveryStreamEncryptionConfiguration =
  DeliveryStreamEncryptionConfiguration'
    { status = Lude.Nothing,
      keyType = Lude.Nothing,
      keyARN = Lude.Nothing,
      failureDescription = Lude.Nothing
    }

-- | This is the server-side encryption (SSE) status for the delivery stream. For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' . If this status is @ENABLING_FAILED@ or @DISABLING_FAILED@ , it is the status of the most recent attempt to enable or disable SSE, respectively.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecStatus :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Lude.Maybe DeliveryStreamEncryptionStatus)
dsecStatus = Lens.lens (status :: DeliveryStreamEncryptionConfiguration -> Lude.Maybe DeliveryStreamEncryptionStatus) (\s a -> s {status = a} :: DeliveryStreamEncryptionConfiguration)
{-# DEPRECATED dsecStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates the type of customer master key (CMK) that is used for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> .
--
-- /Note:/ Consider using 'keyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecKeyType :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Lude.Maybe KeyType)
dsecKeyType = Lens.lens (keyType :: DeliveryStreamEncryptionConfiguration -> Lude.Maybe KeyType) (\s a -> s {keyType = a} :: DeliveryStreamEncryptionConfiguration)
{-# DEPRECATED dsecKeyType "Use generic-lens or generic-optics with 'keyType' instead." #-}

-- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@ , this field contains the ARN of the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@ , @DeliveryStreamEncryptionConfiguration@ doesn't contain a value for @KeyARN@ .
--
-- /Note:/ Consider using 'keyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecKeyARN :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Lude.Maybe Lude.Text)
dsecKeyARN = Lens.lens (keyARN :: DeliveryStreamEncryptionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {keyARN = a} :: DeliveryStreamEncryptionConfiguration)
{-# DEPRECATED dsecKeyARN "Use generic-lens or generic-optics with 'keyARN' instead." #-}

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecFailureDescription :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Lude.Maybe FailureDescription)
dsecFailureDescription = Lens.lens (failureDescription :: DeliveryStreamEncryptionConfiguration -> Lude.Maybe FailureDescription) (\s a -> s {failureDescription = a} :: DeliveryStreamEncryptionConfiguration)
{-# DEPRECATED dsecFailureDescription "Use generic-lens or generic-optics with 'failureDescription' instead." #-}

instance Lude.FromJSON DeliveryStreamEncryptionConfiguration where
  parseJSON =
    Lude.withObject
      "DeliveryStreamEncryptionConfiguration"
      ( \x ->
          DeliveryStreamEncryptionConfiguration'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "KeyType")
            Lude.<*> (x Lude..:? "KeyARN")
            Lude.<*> (x Lude..:? "FailureDescription")
      )

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
    dsecFailureDescription,
    dsecKeyARN,
    dsecKeyType,
    dsecStatus,
  )
where

import qualified Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus as Types
import qualified Network.AWS.Firehose.Types.FailureDescription as Types
import qualified Network.AWS.Firehose.Types.KeyARN as Types
import qualified Network.AWS.Firehose.Types.KeyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the server-side encryption (SSE) status for the delivery stream, the type customer master key (CMK) in use, if any, and the ARN of the CMK. You can get @DeliveryStreamEncryptionConfiguration@ by invoking the 'DescribeDeliveryStream' operation.
--
-- /See:/ 'mkDeliveryStreamEncryptionConfiguration' smart constructor.
data DeliveryStreamEncryptionConfiguration = DeliveryStreamEncryptionConfiguration'
  { -- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
    failureDescription :: Core.Maybe Types.FailureDescription,
    -- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@ , this field contains the ARN of the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@ , @DeliveryStreamEncryptionConfiguration@ doesn't contain a value for @KeyARN@ .
    keyARN :: Core.Maybe Types.KeyARN,
    -- | Indicates the type of customer master key (CMK) that is used for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> .
    keyType :: Core.Maybe Types.KeyType,
    -- | This is the server-side encryption (SSE) status for the delivery stream. For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' . If this status is @ENABLING_FAILED@ or @DISABLING_FAILED@ , it is the status of the most recent attempt to enable or disable SSE, respectively.
    status :: Core.Maybe Types.DeliveryStreamEncryptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeliveryStreamEncryptionConfiguration' value with any optional fields omitted.
mkDeliveryStreamEncryptionConfiguration ::
  DeliveryStreamEncryptionConfiguration
mkDeliveryStreamEncryptionConfiguration =
  DeliveryStreamEncryptionConfiguration'
    { failureDescription =
        Core.Nothing,
      keyARN = Core.Nothing,
      keyType = Core.Nothing,
      status = Core.Nothing
    }

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecFailureDescription :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Core.Maybe Types.FailureDescription)
dsecFailureDescription = Lens.field @"failureDescription"
{-# DEPRECATED dsecFailureDescription "Use generic-lens or generic-optics with 'failureDescription' instead." #-}

-- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@ , this field contains the ARN of the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@ , @DeliveryStreamEncryptionConfiguration@ doesn't contain a value for @KeyARN@ .
--
-- /Note:/ Consider using 'keyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecKeyARN :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Core.Maybe Types.KeyARN)
dsecKeyARN = Lens.field @"keyARN"
{-# DEPRECATED dsecKeyARN "Use generic-lens or generic-optics with 'keyARN' instead." #-}

-- | Indicates the type of customer master key (CMK) that is used for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> .
--
-- /Note:/ Consider using 'keyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecKeyType :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Core.Maybe Types.KeyType)
dsecKeyType = Lens.field @"keyType"
{-# DEPRECATED dsecKeyType "Use generic-lens or generic-optics with 'keyType' instead." #-}

-- | This is the server-side encryption (SSE) status for the delivery stream. For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' . If this status is @ENABLING_FAILED@ or @DISABLING_FAILED@ , it is the status of the most recent attempt to enable or disable SSE, respectively.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsecStatus :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Core.Maybe Types.DeliveryStreamEncryptionStatus)
dsecStatus = Lens.field @"status"
{-# DEPRECATED dsecStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON DeliveryStreamEncryptionConfiguration where
  parseJSON =
    Core.withObject "DeliveryStreamEncryptionConfiguration" Core.$
      \x ->
        DeliveryStreamEncryptionConfiguration'
          Core.<$> (x Core..:? "FailureDescription")
          Core.<*> (x Core..:? "KeyARN")
          Core.<*> (x Core..:? "KeyType")
          Core.<*> (x Core..:? "Status")

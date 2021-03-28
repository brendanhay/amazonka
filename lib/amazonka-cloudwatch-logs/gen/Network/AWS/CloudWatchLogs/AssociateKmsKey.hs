{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.AssociateKmsKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified AWS Key Management Service (AWS KMS) customer master key (CMK) with the specified log group.
--
-- Associating an AWS KMS CMK with a log group overrides any existing associations between the log group and a CMK. After a CMK is associated with a log group, all newly ingested data for the log group is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.
-- /Important:/ CloudWatch Logs supports only symmetric CMKs. Do not use an associate an asymmetric CMK with your log group. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
-- It can take up to 5 minutes for this operation to take effect.
-- If you attempt to associate a CMK with a log group but the CMK does not exist or the CMK is disabled, you receive an @InvalidParameterException@ error. 
module Network.AWS.CloudWatchLogs.AssociateKmsKey
    (
    -- * Creating a request
      AssociateKmsKey (..)
    , mkAssociateKmsKey
    -- ** Request lenses
    , akkLogGroupName
    , akkKmsKeyId

    -- * Destructuring the response
    , AssociateKmsKeyResponse (..)
    , mkAssociateKmsKeyResponse
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateKmsKey' smart constructor.
data AssociateKmsKey = AssociateKmsKey'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  , kmsKeyId :: Types.KmsKeyId
    -- ^ The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. This must be a symmetric CMK. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> and <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateKmsKey' value with any optional fields omitted.
mkAssociateKmsKey
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> Types.KmsKeyId -- ^ 'kmsKeyId'
    -> AssociateKmsKey
mkAssociateKmsKey logGroupName kmsKeyId
  = AssociateKmsKey'{logGroupName, kmsKeyId}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akkLogGroupName :: Lens.Lens' AssociateKmsKey Types.LogGroupName
akkLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE akkLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. This must be a symmetric CMK. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> and <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akkKmsKeyId :: Lens.Lens' AssociateKmsKey Types.KmsKeyId
akkKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE akkKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

instance Core.ToQuery AssociateKmsKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateKmsKey where
        toHeaders AssociateKmsKey{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.AssociateKmsKey")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateKmsKey where
        toJSON AssociateKmsKey{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logGroupName" Core..= logGroupName),
                  Core.Just ("kmsKeyId" Core..= kmsKeyId)])

instance Core.AWSRequest AssociateKmsKey where
        type Rs AssociateKmsKey = AssociateKmsKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AssociateKmsKeyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateKmsKeyResponse' smart constructor.
data AssociateKmsKeyResponse = AssociateKmsKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateKmsKeyResponse' value with any optional fields omitted.
mkAssociateKmsKeyResponse
    :: AssociateKmsKeyResponse
mkAssociateKmsKeyResponse = AssociateKmsKeyResponse'

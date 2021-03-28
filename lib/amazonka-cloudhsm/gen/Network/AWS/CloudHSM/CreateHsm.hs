{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateHsm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Creates an uninitialized HSM instance.
-- There is an upfront fee charged for each HSM instance that you create with the @CreateHsm@ operation. If you accidentally provision an HSM and want to request a refund, delete the instance using the 'DeleteHsm' operation, go to the <https://console.aws.amazon.com/support/home AWS Support Center> , create a new case, and select __Account and Billing Support__ .
-- /Important:/ It can take up to 20 minutes to create and provision an HSM. You can monitor the status of the HSM with the 'DescribeHsm' operation. The HSM is ready to be initialized when the status changes to @RUNNING@ .
module Network.AWS.CloudHSM.CreateHsm
    (
    -- * Creating a request
      CreateHsm (..)
    , mkCreateHsm
    -- ** Request lenses
    , chSubnetId
    , chSshKey
    , chIamRoleArn
    , chSubscriptionType
    , chClientToken
    , chEniIp
    , chExternalId
    , chSyslogIp

    -- * Destructuring the response
    , CreateHsmResponse (..)
    , mkCreateHsmResponse
    -- ** Response lenses
    , crsHsmArn
    , crsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the @CreateHsm@ operation.
--
-- /See:/ 'mkCreateHsm' smart constructor.
data CreateHsm = CreateHsm'
  { subnetId :: Types.SubnetId
    -- ^ The identifier of the subnet in your VPC in which to place the HSM.
  , sshKey :: Types.SshKey
    -- ^ The SSH public key to install on the HSM.
  , iamRoleArn :: Types.IamRoleArn
    -- ^ The ARN of an IAM role to enable the AWS CloudHSM service to allocate an ENI on your behalf.
  , subscriptionType :: Types.SubscriptionType
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ A user-defined token to ensure idempotence. Subsequent calls to this operation with the same token will be ignored.
  , eniIp :: Core.Maybe Types.IpAddress
    -- ^ The IP address to assign to the HSM's ENI.
--
-- If an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the subnet.
  , externalId :: Core.Maybe Types.ExternalId
    -- ^ The external ID from @IamRoleArn@ , if present.
  , syslogIp :: Core.Maybe Types.IpAddress
    -- ^ The IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsm' value with any optional fields omitted.
mkCreateHsm
    :: Types.SubnetId -- ^ 'subnetId'
    -> Types.SshKey -- ^ 'sshKey'
    -> Types.IamRoleArn -- ^ 'iamRoleArn'
    -> Types.SubscriptionType -- ^ 'subscriptionType'
    -> CreateHsm
mkCreateHsm subnetId sshKey iamRoleArn subscriptionType
  = CreateHsm'{subnetId, sshKey, iamRoleArn, subscriptionType,
               clientToken = Core.Nothing, eniIp = Core.Nothing,
               externalId = Core.Nothing, syslogIp = Core.Nothing}

-- | The identifier of the subnet in your VPC in which to place the HSM.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chSubnetId :: Lens.Lens' CreateHsm Types.SubnetId
chSubnetId = Lens.field @"subnetId"
{-# INLINEABLE chSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The SSH public key to install on the HSM.
--
-- /Note:/ Consider using 'sshKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chSshKey :: Lens.Lens' CreateHsm Types.SshKey
chSshKey = Lens.field @"sshKey"
{-# INLINEABLE chSshKey #-}
{-# DEPRECATED sshKey "Use generic-lens or generic-optics with 'sshKey' instead"  #-}

-- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an ENI on your behalf.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chIamRoleArn :: Lens.Lens' CreateHsm Types.IamRoleArn
chIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE chIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chSubscriptionType :: Lens.Lens' CreateHsm Types.SubscriptionType
chSubscriptionType = Lens.field @"subscriptionType"
{-# INLINEABLE chSubscriptionType #-}
{-# DEPRECATED subscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead"  #-}

-- | A user-defined token to ensure idempotence. Subsequent calls to this operation with the same token will be ignored.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chClientToken :: Lens.Lens' CreateHsm (Core.Maybe Types.ClientToken)
chClientToken = Lens.field @"clientToken"
{-# INLINEABLE chClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The IP address to assign to the HSM's ENI.
--
-- If an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the subnet.
--
-- /Note:/ Consider using 'eniIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chEniIp :: Lens.Lens' CreateHsm (Core.Maybe Types.IpAddress)
chEniIp = Lens.field @"eniIp"
{-# INLINEABLE chEniIp #-}
{-# DEPRECATED eniIp "Use generic-lens or generic-optics with 'eniIp' instead"  #-}

-- | The external ID from @IamRoleArn@ , if present.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chExternalId :: Lens.Lens' CreateHsm (Core.Maybe Types.ExternalId)
chExternalId = Lens.field @"externalId"
{-# INLINEABLE chExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

-- | The IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
--
-- /Note:/ Consider using 'syslogIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chSyslogIp :: Lens.Lens' CreateHsm (Core.Maybe Types.IpAddress)
chSyslogIp = Lens.field @"syslogIp"
{-# INLINEABLE chSyslogIp #-}
{-# DEPRECATED syslogIp "Use generic-lens or generic-optics with 'syslogIp' instead"  #-}

instance Core.ToQuery CreateHsm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateHsm where
        toHeaders CreateHsm{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.CreateHsm")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateHsm where
        toJSON CreateHsm{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SubnetId" Core..= subnetId),
                  Core.Just ("SshKey" Core..= sshKey),
                  Core.Just ("IamRoleArn" Core..= iamRoleArn),
                  Core.Just ("SubscriptionType" Core..= subscriptionType),
                  ("ClientToken" Core..=) Core.<$> clientToken,
                  ("EniIp" Core..=) Core.<$> eniIp,
                  ("ExternalId" Core..=) Core.<$> externalId,
                  ("SyslogIp" Core..=) Core.<$> syslogIp])

instance Core.AWSRequest CreateHsm where
        type Rs CreateHsm = CreateHsmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateHsmResponse' Core.<$>
                   (x Core..:? "HsmArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the @CreateHsm@ operation.
--
-- /See:/ 'mkCreateHsmResponse' smart constructor.
data CreateHsmResponse = CreateHsmResponse'
  { hsmArn :: Core.Maybe Types.HsmArn
    -- ^ The ARN of the HSM.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsmResponse' value with any optional fields omitted.
mkCreateHsmResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateHsmResponse
mkCreateHsmResponse responseStatus
  = CreateHsmResponse'{hsmArn = Core.Nothing, responseStatus}

-- | The ARN of the HSM.
--
-- /Note:/ Consider using 'hsmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsHsmArn :: Lens.Lens' CreateHsmResponse (Core.Maybe Types.HsmArn)
crsHsmArn = Lens.field @"hsmArn"
{-# INLINEABLE crsHsmArn #-}
{-# DEPRECATED hsmArn "Use generic-lens or generic-optics with 'hsmArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateHsmResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

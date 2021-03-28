{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyHsm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Modifies an HSM.
-- /Important:/ This operation can result in the HSM being offline for up to 15 minutes while the AWS CloudHSM service is reconfigured. If you are modifying a production HSM, you should ensure that your AWS CloudHSM service is configured for high availability, and consider executing this operation during a maintenance window.
module Network.AWS.CloudHSM.ModifyHsm
    (
    -- * Creating a request
      ModifyHsm (..)
    , mkModifyHsm
    -- ** Request lenses
    , mhHsmArn
    , mhEniIp
    , mhExternalId
    , mhIamRoleArn
    , mhSubnetId
    , mhSyslogIp

    -- * Destructuring the response
    , ModifyHsmResponse (..)
    , mkModifyHsmResponse
    -- ** Response lenses
    , mhrrsHsmArn
    , mhrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'ModifyHsm' operation.
--
-- /See:/ 'mkModifyHsm' smart constructor.
data ModifyHsm = ModifyHsm'
  { hsmArn :: Types.HsmArn
    -- ^ The ARN of the HSM to modify.
  , eniIp :: Core.Maybe Types.IpAddress
    -- ^ The new IP address for the elastic network interface (ENI) attached to the HSM.
--
-- If the HSM is moved to a different subnet, and an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the new subnet.
  , externalId :: Core.Maybe Types.ExternalId
    -- ^ The new external ID.
  , iamRoleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The new IAM role ARN.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The new identifier of the subnet that the HSM is in. The new subnet must be in the same Availability Zone as the current subnet.
  , syslogIp :: Core.Maybe Types.IpAddress
    -- ^ The new IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyHsm' value with any optional fields omitted.
mkModifyHsm
    :: Types.HsmArn -- ^ 'hsmArn'
    -> ModifyHsm
mkModifyHsm hsmArn
  = ModifyHsm'{hsmArn, eniIp = Core.Nothing,
               externalId = Core.Nothing, iamRoleArn = Core.Nothing,
               subnetId = Core.Nothing, syslogIp = Core.Nothing}

-- | The ARN of the HSM to modify.
--
-- /Note:/ Consider using 'hsmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHsmArn :: Lens.Lens' ModifyHsm Types.HsmArn
mhHsmArn = Lens.field @"hsmArn"
{-# INLINEABLE mhHsmArn #-}
{-# DEPRECATED hsmArn "Use generic-lens or generic-optics with 'hsmArn' instead"  #-}

-- | The new IP address for the elastic network interface (ENI) attached to the HSM.
--
-- If the HSM is moved to a different subnet, and an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the new subnet.
--
-- /Note:/ Consider using 'eniIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhEniIp :: Lens.Lens' ModifyHsm (Core.Maybe Types.IpAddress)
mhEniIp = Lens.field @"eniIp"
{-# INLINEABLE mhEniIp #-}
{-# DEPRECATED eniIp "Use generic-lens or generic-optics with 'eniIp' instead"  #-}

-- | The new external ID.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhExternalId :: Lens.Lens' ModifyHsm (Core.Maybe Types.ExternalId)
mhExternalId = Lens.field @"externalId"
{-# INLINEABLE mhExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

-- | The new IAM role ARN.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhIamRoleArn :: Lens.Lens' ModifyHsm (Core.Maybe Types.IamRoleArn)
mhIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE mhIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | The new identifier of the subnet that the HSM is in. The new subnet must be in the same Availability Zone as the current subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhSubnetId :: Lens.Lens' ModifyHsm (Core.Maybe Types.SubnetId)
mhSubnetId = Lens.field @"subnetId"
{-# INLINEABLE mhSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The new IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
--
-- /Note:/ Consider using 'syslogIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhSyslogIp :: Lens.Lens' ModifyHsm (Core.Maybe Types.IpAddress)
mhSyslogIp = Lens.field @"syslogIp"
{-# INLINEABLE mhSyslogIp #-}
{-# DEPRECATED syslogIp "Use generic-lens or generic-optics with 'syslogIp' instead"  #-}

instance Core.ToQuery ModifyHsm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyHsm where
        toHeaders ModifyHsm{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.ModifyHsm")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyHsm where
        toJSON ModifyHsm{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HsmArn" Core..= hsmArn),
                  ("EniIp" Core..=) Core.<$> eniIp,
                  ("ExternalId" Core..=) Core.<$> externalId,
                  ("IamRoleArn" Core..=) Core.<$> iamRoleArn,
                  ("SubnetId" Core..=) Core.<$> subnetId,
                  ("SyslogIp" Core..=) Core.<$> syslogIp])

instance Core.AWSRequest ModifyHsm where
        type Rs ModifyHsm = ModifyHsmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyHsmResponse' Core.<$>
                   (x Core..:? "HsmArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the 'ModifyHsm' operation.
--
-- /See:/ 'mkModifyHsmResponse' smart constructor.
data ModifyHsmResponse = ModifyHsmResponse'
  { hsmArn :: Core.Maybe Types.HsmArn
    -- ^ The ARN of the HSM.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyHsmResponse' value with any optional fields omitted.
mkModifyHsmResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyHsmResponse
mkModifyHsmResponse responseStatus
  = ModifyHsmResponse'{hsmArn = Core.Nothing, responseStatus}

-- | The ARN of the HSM.
--
-- /Note:/ Consider using 'hsmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrrsHsmArn :: Lens.Lens' ModifyHsmResponse (Core.Maybe Types.HsmArn)
mhrrsHsmArn = Lens.field @"hsmArn"
{-# INLINEABLE mhrrsHsmArn #-}
{-# DEPRECATED hsmArn "Use generic-lens or generic-optics with 'hsmArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrrsResponseStatus :: Lens.Lens' ModifyHsmResponse Core.Int
mhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

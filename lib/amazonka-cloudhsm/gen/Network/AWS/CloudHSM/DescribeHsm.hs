{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHsm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Retrieves information about an HSM. You can identify the HSM by its ARN or its serial number.
module Network.AWS.CloudHSM.DescribeHsm
    (
    -- * Creating a request
      DescribeHsm (..)
    , mkDescribeHsm
    -- ** Request lenses
    , dHsmArn
    , dHsmSerialNumber

    -- * Destructuring the response
    , DescribeHsmResponse (..)
    , mkDescribeHsmResponse
    -- ** Response lenses
    , dhrgrsAvailabilityZone
    , dhrgrsEniId
    , dhrgrsEniIp
    , dhrgrsHsmArn
    , dhrgrsHsmType
    , dhrgrsIamRoleArn
    , dhrgrsPartitions
    , dhrgrsSerialNumber
    , dhrgrsServerCertLastUpdated
    , dhrgrsServerCertUri
    , dhrgrsSoftwareVersion
    , dhrgrsSshKeyLastUpdated
    , dhrgrsSshPublicKey
    , dhrgrsStatus
    , dhrgrsStatusDetails
    , dhrgrsSubnetId
    , dhrgrsSubscriptionEndDate
    , dhrgrsSubscriptionStartDate
    , dhrgrsSubscriptionType
    , dhrgrsVendorName
    , dhrgrsVpcId
    , dhrgrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DescribeHsm' operation.
--
-- /See:/ 'mkDescribeHsm' smart constructor.
data DescribeHsm = DescribeHsm'
  { hsmArn :: Core.Maybe Types.HsmArn
    -- ^ The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter must be specified.
  , hsmSerialNumber :: Core.Maybe Types.HsmSerialNumber
    -- ^ The serial number of the HSM. Either the @HsmArn@ or the @HsmSerialNumber@ parameter must be specified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHsm' value with any optional fields omitted.
mkDescribeHsm
    :: DescribeHsm
mkDescribeHsm
  = DescribeHsm'{hsmArn = Core.Nothing,
                 hsmSerialNumber = Core.Nothing}

-- | The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter must be specified.
--
-- /Note:/ Consider using 'hsmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHsmArn :: Lens.Lens' DescribeHsm (Core.Maybe Types.HsmArn)
dHsmArn = Lens.field @"hsmArn"
{-# INLINEABLE dHsmArn #-}
{-# DEPRECATED hsmArn "Use generic-lens or generic-optics with 'hsmArn' instead"  #-}

-- | The serial number of the HSM. Either the @HsmArn@ or the @HsmSerialNumber@ parameter must be specified.
--
-- /Note:/ Consider using 'hsmSerialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHsmSerialNumber :: Lens.Lens' DescribeHsm (Core.Maybe Types.HsmSerialNumber)
dHsmSerialNumber = Lens.field @"hsmSerialNumber"
{-# INLINEABLE dHsmSerialNumber #-}
{-# DEPRECATED hsmSerialNumber "Use generic-lens or generic-optics with 'hsmSerialNumber' instead"  #-}

instance Core.ToQuery DescribeHsm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeHsm where
        toHeaders DescribeHsm{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.DescribeHsm")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeHsm where
        toJSON DescribeHsm{..}
          = Core.object
              (Core.catMaybes
                 [("HsmArn" Core..=) Core.<$> hsmArn,
                  ("HsmSerialNumber" Core..=) Core.<$> hsmSerialNumber])

instance Core.AWSRequest DescribeHsm where
        type Rs DescribeHsm = DescribeHsmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeHsmResponse' Core.<$>
                   (x Core..:? "AvailabilityZone") Core.<*> x Core..:? "EniId"
                     Core.<*> x Core..:? "EniIp"
                     Core.<*> x Core..:? "HsmArn"
                     Core.<*> x Core..:? "HsmType"
                     Core.<*> x Core..:? "IamRoleArn"
                     Core.<*> x Core..:? "Partitions"
                     Core.<*> x Core..:? "SerialNumber"
                     Core.<*> x Core..:? "ServerCertLastUpdated"
                     Core.<*> x Core..:? "ServerCertUri"
                     Core.<*> x Core..:? "SoftwareVersion"
                     Core.<*> x Core..:? "SshKeyLastUpdated"
                     Core.<*> x Core..:? "SshPublicKey"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "StatusDetails"
                     Core.<*> x Core..:? "SubnetId"
                     Core.<*> x Core..:? "SubscriptionEndDate"
                     Core.<*> x Core..:? "SubscriptionStartDate"
                     Core.<*> x Core..:? "SubscriptionType"
                     Core.<*> x Core..:? "VendorName"
                     Core.<*> x Core..:? "VpcId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the 'DescribeHsm' operation.
--
-- /See:/ 'mkDescribeHsmResponse' smart constructor.
data DescribeHsmResponse = DescribeHsmResponse'
  { availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The Availability Zone that the HSM is in.
  , eniId :: Core.Maybe Types.EniId
    -- ^ The identifier of the elastic network interface (ENI) attached to the HSM.
  , eniIp :: Core.Maybe Types.IpAddress
    -- ^ The IP address assigned to the HSM's ENI.
  , hsmArn :: Core.Maybe Types.HsmArn
    -- ^ The ARN of the HSM.
  , hsmType :: Core.Maybe Core.Text
    -- ^ The HSM model type.
  , iamRoleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The ARN of the IAM role assigned to the HSM.
  , partitions :: Core.Maybe [Types.PartitionArn]
    -- ^ The list of partitions on the HSM.
  , serialNumber :: Core.Maybe Types.HsmSerialNumber
    -- ^ The serial number of the HSM.
  , serverCertLastUpdated :: Core.Maybe Types.ServerCertLastUpdated
    -- ^ The date and time that the server certificate was last updated.
  , serverCertUri :: Core.Maybe Core.Text
    -- ^ The URI of the certificate server.
  , softwareVersion :: Core.Maybe Core.Text
    -- ^ The HSM software version.
  , sshKeyLastUpdated :: Core.Maybe Types.SshKeyLastUpdated
    -- ^ The date and time that the SSH key was last updated.
  , sshPublicKey :: Core.Maybe Types.SshKey
    -- ^ The public SSH key.
  , status :: Core.Maybe Types.HsmStatus
    -- ^ The status of the HSM.
  , statusDetails :: Core.Maybe Core.Text
    -- ^ Contains additional information about the status of the HSM.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The identifier of the subnet that the HSM is in.
  , subscriptionEndDate :: Core.Maybe Types.SubscriptionEndDate
    -- ^ The subscription end date.
  , subscriptionStartDate :: Core.Maybe Types.SubscriptionStartDate
    -- ^ The subscription start date.
  , subscriptionType :: Core.Maybe Types.SubscriptionType
  , vendorName :: Core.Maybe Core.Text
    -- ^ The name of the HSM vendor.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The identifier of the VPC that the HSM is in.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHsmResponse' value with any optional fields omitted.
mkDescribeHsmResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeHsmResponse
mkDescribeHsmResponse responseStatus
  = DescribeHsmResponse'{availabilityZone = Core.Nothing,
                         eniId = Core.Nothing, eniIp = Core.Nothing, hsmArn = Core.Nothing,
                         hsmType = Core.Nothing, iamRoleArn = Core.Nothing,
                         partitions = Core.Nothing, serialNumber = Core.Nothing,
                         serverCertLastUpdated = Core.Nothing, serverCertUri = Core.Nothing,
                         softwareVersion = Core.Nothing, sshKeyLastUpdated = Core.Nothing,
                         sshPublicKey = Core.Nothing, status = Core.Nothing,
                         statusDetails = Core.Nothing, subnetId = Core.Nothing,
                         subscriptionEndDate = Core.Nothing,
                         subscriptionStartDate = Core.Nothing,
                         subscriptionType = Core.Nothing, vendorName = Core.Nothing,
                         vpcId = Core.Nothing, responseStatus}

-- | The Availability Zone that the HSM is in.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsAvailabilityZone :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.AvailabilityZone)
dhrgrsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE dhrgrsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The identifier of the elastic network interface (ENI) attached to the HSM.
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsEniId :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.EniId)
dhrgrsEniId = Lens.field @"eniId"
{-# INLINEABLE dhrgrsEniId #-}
{-# DEPRECATED eniId "Use generic-lens or generic-optics with 'eniId' instead"  #-}

-- | The IP address assigned to the HSM's ENI.
--
-- /Note:/ Consider using 'eniIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsEniIp :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.IpAddress)
dhrgrsEniIp = Lens.field @"eniIp"
{-# INLINEABLE dhrgrsEniIp #-}
{-# DEPRECATED eniIp "Use generic-lens or generic-optics with 'eniIp' instead"  #-}

-- | The ARN of the HSM.
--
-- /Note:/ Consider using 'hsmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsHsmArn :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.HsmArn)
dhrgrsHsmArn = Lens.field @"hsmArn"
{-# INLINEABLE dhrgrsHsmArn #-}
{-# DEPRECATED hsmArn "Use generic-lens or generic-optics with 'hsmArn' instead"  #-}

-- | The HSM model type.
--
-- /Note:/ Consider using 'hsmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsHsmType :: Lens.Lens' DescribeHsmResponse (Core.Maybe Core.Text)
dhrgrsHsmType = Lens.field @"hsmType"
{-# INLINEABLE dhrgrsHsmType #-}
{-# DEPRECATED hsmType "Use generic-lens or generic-optics with 'hsmType' instead"  #-}

-- | The ARN of the IAM role assigned to the HSM.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsIamRoleArn :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.IamRoleArn)
dhrgrsIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE dhrgrsIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | The list of partitions on the HSM.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsPartitions :: Lens.Lens' DescribeHsmResponse (Core.Maybe [Types.PartitionArn])
dhrgrsPartitions = Lens.field @"partitions"
{-# INLINEABLE dhrgrsPartitions #-}
{-# DEPRECATED partitions "Use generic-lens or generic-optics with 'partitions' instead"  #-}

-- | The serial number of the HSM.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSerialNumber :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.HsmSerialNumber)
dhrgrsSerialNumber = Lens.field @"serialNumber"
{-# INLINEABLE dhrgrsSerialNumber #-}
{-# DEPRECATED serialNumber "Use generic-lens or generic-optics with 'serialNumber' instead"  #-}

-- | The date and time that the server certificate was last updated.
--
-- /Note:/ Consider using 'serverCertLastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsServerCertLastUpdated :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.ServerCertLastUpdated)
dhrgrsServerCertLastUpdated = Lens.field @"serverCertLastUpdated"
{-# INLINEABLE dhrgrsServerCertLastUpdated #-}
{-# DEPRECATED serverCertLastUpdated "Use generic-lens or generic-optics with 'serverCertLastUpdated' instead"  #-}

-- | The URI of the certificate server.
--
-- /Note:/ Consider using 'serverCertUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsServerCertUri :: Lens.Lens' DescribeHsmResponse (Core.Maybe Core.Text)
dhrgrsServerCertUri = Lens.field @"serverCertUri"
{-# INLINEABLE dhrgrsServerCertUri #-}
{-# DEPRECATED serverCertUri "Use generic-lens or generic-optics with 'serverCertUri' instead"  #-}

-- | The HSM software version.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSoftwareVersion :: Lens.Lens' DescribeHsmResponse (Core.Maybe Core.Text)
dhrgrsSoftwareVersion = Lens.field @"softwareVersion"
{-# INLINEABLE dhrgrsSoftwareVersion #-}
{-# DEPRECATED softwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead"  #-}

-- | The date and time that the SSH key was last updated.
--
-- /Note:/ Consider using 'sshKeyLastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSshKeyLastUpdated :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.SshKeyLastUpdated)
dhrgrsSshKeyLastUpdated = Lens.field @"sshKeyLastUpdated"
{-# INLINEABLE dhrgrsSshKeyLastUpdated #-}
{-# DEPRECATED sshKeyLastUpdated "Use generic-lens or generic-optics with 'sshKeyLastUpdated' instead"  #-}

-- | The public SSH key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSshPublicKey :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.SshKey)
dhrgrsSshPublicKey = Lens.field @"sshPublicKey"
{-# INLINEABLE dhrgrsSshPublicKey #-}
{-# DEPRECATED sshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead"  #-}

-- | The status of the HSM.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsStatus :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.HsmStatus)
dhrgrsStatus = Lens.field @"status"
{-# INLINEABLE dhrgrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Contains additional information about the status of the HSM.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsStatusDetails :: Lens.Lens' DescribeHsmResponse (Core.Maybe Core.Text)
dhrgrsStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE dhrgrsStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The identifier of the subnet that the HSM is in.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSubnetId :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.SubnetId)
dhrgrsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE dhrgrsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The subscription end date.
--
-- /Note:/ Consider using 'subscriptionEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSubscriptionEndDate :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.SubscriptionEndDate)
dhrgrsSubscriptionEndDate = Lens.field @"subscriptionEndDate"
{-# INLINEABLE dhrgrsSubscriptionEndDate #-}
{-# DEPRECATED subscriptionEndDate "Use generic-lens or generic-optics with 'subscriptionEndDate' instead"  #-}

-- | The subscription start date.
--
-- /Note:/ Consider using 'subscriptionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSubscriptionStartDate :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.SubscriptionStartDate)
dhrgrsSubscriptionStartDate = Lens.field @"subscriptionStartDate"
{-# INLINEABLE dhrgrsSubscriptionStartDate #-}
{-# DEPRECATED subscriptionStartDate "Use generic-lens or generic-optics with 'subscriptionStartDate' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsSubscriptionType :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.SubscriptionType)
dhrgrsSubscriptionType = Lens.field @"subscriptionType"
{-# INLINEABLE dhrgrsSubscriptionType #-}
{-# DEPRECATED subscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead"  #-}

-- | The name of the HSM vendor.
--
-- /Note:/ Consider using 'vendorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsVendorName :: Lens.Lens' DescribeHsmResponse (Core.Maybe Core.Text)
dhrgrsVendorName = Lens.field @"vendorName"
{-# INLINEABLE dhrgrsVendorName #-}
{-# DEPRECATED vendorName "Use generic-lens or generic-optics with 'vendorName' instead"  #-}

-- | The identifier of the VPC that the HSM is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsVpcId :: Lens.Lens' DescribeHsmResponse (Core.Maybe Types.VpcId)
dhrgrsVpcId = Lens.field @"vpcId"
{-# INLINEABLE dhrgrsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrgrsResponseStatus :: Lens.Lens' DescribeHsmResponse Core.Int
dhrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

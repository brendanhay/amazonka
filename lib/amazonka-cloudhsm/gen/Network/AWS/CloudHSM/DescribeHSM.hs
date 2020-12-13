{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHSM
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
module Network.AWS.CloudHSM.DescribeHSM
  ( -- * Creating a request
    DescribeHSM (..),
    mkDescribeHSM,

    -- ** Request lenses
    dhsmHSMSerialNumber,
    dhsmHSMARN,

    -- * Destructuring the response
    DescribeHSMResponse (..),
    mkDescribeHSMResponse,

    -- ** Response lenses
    dhfrsStatus,
    dhfrsIAMRoleARN,
    dhfrsEniId,
    dhfrsVPCId,
    dhfrsSSHKeyLastUpdated,
    dhfrsSubscriptionEndDate,
    dhfrsServerCertURI,
    dhfrsSubscriptionType,
    dhfrsSSHPublicKey,
    dhfrsSubnetId,
    dhfrsStatusDetails,
    dhfrsPartitions,
    dhfrsSubscriptionStartDate,
    dhfrsAvailabilityZone,
    dhfrsServerCertLastUpdated,
    dhfrsSoftwareVersion,
    dhfrsVendorName,
    dhfrsSerialNumber,
    dhfrsHSMARN,
    dhfrsEniIP,
    dhfrsHSMType,
    dhfrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DescribeHsm' operation.
--
-- /See:/ 'mkDescribeHSM' smart constructor.
data DescribeHSM = DescribeHSM'
  { -- | The serial number of the HSM. Either the @HsmArn@ or the @HsmSerialNumber@ parameter must be specified.
    hsmSerialNumber :: Lude.Maybe Lude.Text,
    -- | The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter must be specified.
    hsmARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSM' with the minimum fields required to make a request.
--
-- * 'hsmSerialNumber' - The serial number of the HSM. Either the @HsmArn@ or the @HsmSerialNumber@ parameter must be specified.
-- * 'hsmARN' - The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter must be specified.
mkDescribeHSM ::
  DescribeHSM
mkDescribeHSM =
  DescribeHSM'
    { hsmSerialNumber = Lude.Nothing,
      hsmARN = Lude.Nothing
    }

-- | The serial number of the HSM. Either the @HsmArn@ or the @HsmSerialNumber@ parameter must be specified.
--
-- /Note:/ Consider using 'hsmSerialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhsmHSMSerialNumber :: Lens.Lens' DescribeHSM (Lude.Maybe Lude.Text)
dhsmHSMSerialNumber = Lens.lens (hsmSerialNumber :: DescribeHSM -> Lude.Maybe Lude.Text) (\s a -> s {hsmSerialNumber = a} :: DescribeHSM)
{-# DEPRECATED dhsmHSMSerialNumber "Use generic-lens or generic-optics with 'hsmSerialNumber' instead." #-}

-- | The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter must be specified.
--
-- /Note:/ Consider using 'hsmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhsmHSMARN :: Lens.Lens' DescribeHSM (Lude.Maybe Lude.Text)
dhsmHSMARN = Lens.lens (hsmARN :: DescribeHSM -> Lude.Maybe Lude.Text) (\s a -> s {hsmARN = a} :: DescribeHSM)
{-# DEPRECATED dhsmHSMARN "Use generic-lens or generic-optics with 'hsmARN' instead." #-}

instance Lude.AWSRequest DescribeHSM where
  type Rs DescribeHSM = DescribeHSMResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeHSMResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "IamRoleArn")
            Lude.<*> (x Lude..?> "EniId")
            Lude.<*> (x Lude..?> "VpcId")
            Lude.<*> (x Lude..?> "SshKeyLastUpdated")
            Lude.<*> (x Lude..?> "SubscriptionEndDate")
            Lude.<*> (x Lude..?> "ServerCertUri")
            Lude.<*> (x Lude..?> "SubscriptionType")
            Lude.<*> (x Lude..?> "SshPublicKey")
            Lude.<*> (x Lude..?> "SubnetId")
            Lude.<*> (x Lude..?> "StatusDetails")
            Lude.<*> (x Lude..?> "Partitions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "SubscriptionStartDate")
            Lude.<*> (x Lude..?> "AvailabilityZone")
            Lude.<*> (x Lude..?> "ServerCertLastUpdated")
            Lude.<*> (x Lude..?> "SoftwareVersion")
            Lude.<*> (x Lude..?> "VendorName")
            Lude.<*> (x Lude..?> "SerialNumber")
            Lude.<*> (x Lude..?> "HsmArn")
            Lude.<*> (x Lude..?> "EniIp")
            Lude.<*> (x Lude..?> "HsmType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHSM where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.DescribeHsm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeHSM where
  toJSON DescribeHSM' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HsmSerialNumber" Lude..=) Lude.<$> hsmSerialNumber,
            ("HsmArn" Lude..=) Lude.<$> hsmARN
          ]
      )

instance Lude.ToPath DescribeHSM where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHSM where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the 'DescribeHsm' operation.
--
-- /See:/ 'mkDescribeHSMResponse' smart constructor.
data DescribeHSMResponse = DescribeHSMResponse'
  { -- | The status of the HSM.
    status :: Lude.Maybe HSMStatus,
    -- | The ARN of the IAM role assigned to the HSM.
    iamRoleARN :: Lude.Maybe Lude.Text,
    -- | The identifier of the elastic network interface (ENI) attached to the HSM.
    eniId :: Lude.Maybe Lude.Text,
    -- | The identifier of the VPC that the HSM is in.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The date and time that the SSH key was last updated.
    sshKeyLastUpdated :: Lude.Maybe Lude.Text,
    -- | The subscription end date.
    subscriptionEndDate :: Lude.Maybe Lude.Text,
    -- | The URI of the certificate server.
    serverCertURI :: Lude.Maybe Lude.Text,
    subscriptionType :: Lude.Maybe SubscriptionType,
    -- | The public SSH key.
    sshPublicKey :: Lude.Maybe Lude.Text,
    -- | The identifier of the subnet that the HSM is in.
    subnetId :: Lude.Maybe Lude.Text,
    -- | Contains additional information about the status of the HSM.
    statusDetails :: Lude.Maybe Lude.Text,
    -- | The list of partitions on the HSM.
    partitions :: Lude.Maybe [Lude.Text],
    -- | The subscription start date.
    subscriptionStartDate :: Lude.Maybe Lude.Text,
    -- | The Availability Zone that the HSM is in.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The date and time that the server certificate was last updated.
    serverCertLastUpdated :: Lude.Maybe Lude.Text,
    -- | The HSM software version.
    softwareVersion :: Lude.Maybe Lude.Text,
    -- | The name of the HSM vendor.
    vendorName :: Lude.Maybe Lude.Text,
    -- | The serial number of the HSM.
    serialNumber :: Lude.Maybe Lude.Text,
    -- | The ARN of the HSM.
    hsmARN :: Lude.Maybe Lude.Text,
    -- | The IP address assigned to the HSM's ENI.
    eniIP :: Lude.Maybe Lude.Text,
    -- | The HSM model type.
    hsmType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSMResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the HSM.
-- * 'iamRoleARN' - The ARN of the IAM role assigned to the HSM.
-- * 'eniId' - The identifier of the elastic network interface (ENI) attached to the HSM.
-- * 'vpcId' - The identifier of the VPC that the HSM is in.
-- * 'sshKeyLastUpdated' - The date and time that the SSH key was last updated.
-- * 'subscriptionEndDate' - The subscription end date.
-- * 'serverCertURI' - The URI of the certificate server.
-- * 'subscriptionType' -
-- * 'sshPublicKey' - The public SSH key.
-- * 'subnetId' - The identifier of the subnet that the HSM is in.
-- * 'statusDetails' - Contains additional information about the status of the HSM.
-- * 'partitions' - The list of partitions on the HSM.
-- * 'subscriptionStartDate' - The subscription start date.
-- * 'availabilityZone' - The Availability Zone that the HSM is in.
-- * 'serverCertLastUpdated' - The date and time that the server certificate was last updated.
-- * 'softwareVersion' - The HSM software version.
-- * 'vendorName' - The name of the HSM vendor.
-- * 'serialNumber' - The serial number of the HSM.
-- * 'hsmARN' - The ARN of the HSM.
-- * 'eniIP' - The IP address assigned to the HSM's ENI.
-- * 'hsmType' - The HSM model type.
-- * 'responseStatus' - The response status code.
mkDescribeHSMResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHSMResponse
mkDescribeHSMResponse pResponseStatus_ =
  DescribeHSMResponse'
    { status = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      eniId = Lude.Nothing,
      vpcId = Lude.Nothing,
      sshKeyLastUpdated = Lude.Nothing,
      subscriptionEndDate = Lude.Nothing,
      serverCertURI = Lude.Nothing,
      subscriptionType = Lude.Nothing,
      sshPublicKey = Lude.Nothing,
      subnetId = Lude.Nothing,
      statusDetails = Lude.Nothing,
      partitions = Lude.Nothing,
      subscriptionStartDate = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      serverCertLastUpdated = Lude.Nothing,
      softwareVersion = Lude.Nothing,
      vendorName = Lude.Nothing,
      serialNumber = Lude.Nothing,
      hsmARN = Lude.Nothing,
      eniIP = Lude.Nothing,
      hsmType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the HSM.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsStatus :: Lens.Lens' DescribeHSMResponse (Lude.Maybe HSMStatus)
dhfrsStatus = Lens.lens (status :: DescribeHSMResponse -> Lude.Maybe HSMStatus) (\s a -> s {status = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the IAM role assigned to the HSM.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsIAMRoleARN :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsIAMRoleARN = Lens.lens (iamRoleARN :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The identifier of the elastic network interface (ENI) attached to the HSM.
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsEniId :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsEniId = Lens.lens (eniId :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {eniId = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsEniId "Use generic-lens or generic-optics with 'eniId' instead." #-}

-- | The identifier of the VPC that the HSM is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsVPCId :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsVPCId = Lens.lens (vpcId :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The date and time that the SSH key was last updated.
--
-- /Note:/ Consider using 'sshKeyLastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSSHKeyLastUpdated :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsSSHKeyLastUpdated = Lens.lens (sshKeyLastUpdated :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {sshKeyLastUpdated = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSSHKeyLastUpdated "Use generic-lens or generic-optics with 'sshKeyLastUpdated' instead." #-}

-- | The subscription end date.
--
-- /Note:/ Consider using 'subscriptionEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSubscriptionEndDate :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsSubscriptionEndDate = Lens.lens (subscriptionEndDate :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionEndDate = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSubscriptionEndDate "Use generic-lens or generic-optics with 'subscriptionEndDate' instead." #-}

-- | The URI of the certificate server.
--
-- /Note:/ Consider using 'serverCertURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsServerCertURI :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsServerCertURI = Lens.lens (serverCertURI :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {serverCertURI = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsServerCertURI "Use generic-lens or generic-optics with 'serverCertURI' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSubscriptionType :: Lens.Lens' DescribeHSMResponse (Lude.Maybe SubscriptionType)
dhfrsSubscriptionType = Lens.lens (subscriptionType :: DescribeHSMResponse -> Lude.Maybe SubscriptionType) (\s a -> s {subscriptionType = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSubscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead." #-}

-- | The public SSH key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSSHPublicKey :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsSSHPublicKey = Lens.lens (sshPublicKey :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The identifier of the subnet that the HSM is in.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSubnetId :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsSubnetId = Lens.lens (subnetId :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Contains additional information about the status of the HSM.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsStatusDetails :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsStatusDetails = Lens.lens (statusDetails :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The list of partitions on the HSM.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsPartitions :: Lens.Lens' DescribeHSMResponse (Lude.Maybe [Lude.Text])
dhfrsPartitions = Lens.lens (partitions :: DescribeHSMResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {partitions = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsPartitions "Use generic-lens or generic-optics with 'partitions' instead." #-}

-- | The subscription start date.
--
-- /Note:/ Consider using 'subscriptionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSubscriptionStartDate :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsSubscriptionStartDate = Lens.lens (subscriptionStartDate :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionStartDate = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSubscriptionStartDate "Use generic-lens or generic-optics with 'subscriptionStartDate' instead." #-}

-- | The Availability Zone that the HSM is in.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsAvailabilityZone :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsAvailabilityZone = Lens.lens (availabilityZone :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The date and time that the server certificate was last updated.
--
-- /Note:/ Consider using 'serverCertLastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsServerCertLastUpdated :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsServerCertLastUpdated = Lens.lens (serverCertLastUpdated :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {serverCertLastUpdated = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsServerCertLastUpdated "Use generic-lens or generic-optics with 'serverCertLastUpdated' instead." #-}

-- | The HSM software version.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSoftwareVersion :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsSoftwareVersion = Lens.lens (softwareVersion :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {softwareVersion = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

-- | The name of the HSM vendor.
--
-- /Note:/ Consider using 'vendorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsVendorName :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsVendorName = Lens.lens (vendorName :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {vendorName = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsVendorName "Use generic-lens or generic-optics with 'vendorName' instead." #-}

-- | The serial number of the HSM.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsSerialNumber :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsSerialNumber = Lens.lens (serialNumber :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The ARN of the HSM.
--
-- /Note:/ Consider using 'hsmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsHSMARN :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsHSMARN = Lens.lens (hsmARN :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {hsmARN = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsHSMARN "Use generic-lens or generic-optics with 'hsmARN' instead." #-}

-- | The IP address assigned to the HSM's ENI.
--
-- /Note:/ Consider using 'eniIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsEniIP :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsEniIP = Lens.lens (eniIP :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {eniIP = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsEniIP "Use generic-lens or generic-optics with 'eniIP' instead." #-}

-- | The HSM model type.
--
-- /Note:/ Consider using 'hsmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsHSMType :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
dhfrsHSMType = Lens.lens (hsmType :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {hsmType = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsHSMType "Use generic-lens or generic-optics with 'hsmType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhfrsResponseStatus :: Lens.Lens' DescribeHSMResponse Lude.Int
dhfrsResponseStatus = Lens.lens (responseStatus :: DescribeHSMResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHSMResponse)
{-# DEPRECATED dhfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

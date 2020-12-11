{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    desrsStatus,
    desrsIAMRoleARN,
    desrsEniId,
    desrsVPCId,
    desrsSSHKeyLastUpdated,
    desrsSubscriptionEndDate,
    desrsServerCertURI,
    desrsSubscriptionType,
    desrsSSHPublicKey,
    desrsSubnetId,
    desrsStatusDetails,
    desrsPartitions,
    desrsSubscriptionStartDate,
    desrsAvailabilityZone,
    desrsServerCertLastUpdated,
    desrsSoftwareVersion,
    desrsVendorName,
    desrsSerialNumber,
    desrsHSMARN,
    desrsEniIP,
    desrsHSMType,
    desrsResponseStatus,
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
  { hsmSerialNumber ::
      Lude.Maybe Lude.Text,
    hsmARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSM' with the minimum fields required to make a request.
--
-- * 'hsmARN' - The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter must be specified.
-- * 'hsmSerialNumber' - The serial number of the HSM. Either the @HsmArn@ or the @HsmSerialNumber@ parameter must be specified.
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
  { status ::
      Lude.Maybe HSMStatus,
    iamRoleARN :: Lude.Maybe Lude.Text,
    eniId :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    sshKeyLastUpdated :: Lude.Maybe Lude.Text,
    subscriptionEndDate :: Lude.Maybe Lude.Text,
    serverCertURI :: Lude.Maybe Lude.Text,
    subscriptionType :: Lude.Maybe SubscriptionType,
    sshPublicKey :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    statusDetails :: Lude.Maybe Lude.Text,
    partitions :: Lude.Maybe [Lude.Text],
    subscriptionStartDate :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    serverCertLastUpdated :: Lude.Maybe Lude.Text,
    softwareVersion :: Lude.Maybe Lude.Text,
    vendorName :: Lude.Maybe Lude.Text,
    serialNumber :: Lude.Maybe Lude.Text,
    hsmARN :: Lude.Maybe Lude.Text,
    eniIP :: Lude.Maybe Lude.Text,
    hsmType :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSMResponse' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone that the HSM is in.
-- * 'eniIP' - The IP address assigned to the HSM's ENI.
-- * 'eniId' - The identifier of the elastic network interface (ENI) attached to the HSM.
-- * 'hsmARN' - The ARN of the HSM.
-- * 'hsmType' - The HSM model type.
-- * 'iamRoleARN' - The ARN of the IAM role assigned to the HSM.
-- * 'partitions' - The list of partitions on the HSM.
-- * 'responseStatus' - The response status code.
-- * 'serialNumber' - The serial number of the HSM.
-- * 'serverCertLastUpdated' - The date and time that the server certificate was last updated.
-- * 'serverCertURI' - The URI of the certificate server.
-- * 'softwareVersion' - The HSM software version.
-- * 'sshKeyLastUpdated' - The date and time that the SSH key was last updated.
-- * 'sshPublicKey' - The public SSH key.
-- * 'status' - The status of the HSM.
-- * 'statusDetails' - Contains additional information about the status of the HSM.
-- * 'subnetId' - The identifier of the subnet that the HSM is in.
-- * 'subscriptionEndDate' - The subscription end date.
-- * 'subscriptionStartDate' - The subscription start date.
-- * 'subscriptionType' - Undocumented field.
-- * 'vendorName' - The name of the HSM vendor.
-- * 'vpcId' - The identifier of the VPC that the HSM is in.
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
desrsStatus :: Lens.Lens' DescribeHSMResponse (Lude.Maybe HSMStatus)
desrsStatus = Lens.lens (status :: DescribeHSMResponse -> Lude.Maybe HSMStatus) (\s a -> s {status = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the IAM role assigned to the HSM.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsIAMRoleARN :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsIAMRoleARN = Lens.lens (iamRoleARN :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The identifier of the elastic network interface (ENI) attached to the HSM.
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEniId :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsEniId = Lens.lens (eniId :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {eniId = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsEniId "Use generic-lens or generic-optics with 'eniId' instead." #-}

-- | The identifier of the VPC that the HSM is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsVPCId :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsVPCId = Lens.lens (vpcId :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The date and time that the SSH key was last updated.
--
-- /Note:/ Consider using 'sshKeyLastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSSHKeyLastUpdated :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsSSHKeyLastUpdated = Lens.lens (sshKeyLastUpdated :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {sshKeyLastUpdated = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSSHKeyLastUpdated "Use generic-lens or generic-optics with 'sshKeyLastUpdated' instead." #-}

-- | The subscription end date.
--
-- /Note:/ Consider using 'subscriptionEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSubscriptionEndDate :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsSubscriptionEndDate = Lens.lens (subscriptionEndDate :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionEndDate = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSubscriptionEndDate "Use generic-lens or generic-optics with 'subscriptionEndDate' instead." #-}

-- | The URI of the certificate server.
--
-- /Note:/ Consider using 'serverCertURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsServerCertURI :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsServerCertURI = Lens.lens (serverCertURI :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {serverCertURI = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsServerCertURI "Use generic-lens or generic-optics with 'serverCertURI' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSubscriptionType :: Lens.Lens' DescribeHSMResponse (Lude.Maybe SubscriptionType)
desrsSubscriptionType = Lens.lens (subscriptionType :: DescribeHSMResponse -> Lude.Maybe SubscriptionType) (\s a -> s {subscriptionType = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSubscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead." #-}

-- | The public SSH key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSSHPublicKey :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsSSHPublicKey = Lens.lens (sshPublicKey :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The identifier of the subnet that the HSM is in.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSubnetId :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsSubnetId = Lens.lens (subnetId :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Contains additional information about the status of the HSM.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsStatusDetails :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsStatusDetails = Lens.lens (statusDetails :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The list of partitions on the HSM.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsPartitions :: Lens.Lens' DescribeHSMResponse (Lude.Maybe [Lude.Text])
desrsPartitions = Lens.lens (partitions :: DescribeHSMResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {partitions = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsPartitions "Use generic-lens or generic-optics with 'partitions' instead." #-}

-- | The subscription start date.
--
-- /Note:/ Consider using 'subscriptionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSubscriptionStartDate :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsSubscriptionStartDate = Lens.lens (subscriptionStartDate :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionStartDate = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSubscriptionStartDate "Use generic-lens or generic-optics with 'subscriptionStartDate' instead." #-}

-- | The Availability Zone that the HSM is in.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsAvailabilityZone :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsAvailabilityZone = Lens.lens (availabilityZone :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The date and time that the server certificate was last updated.
--
-- /Note:/ Consider using 'serverCertLastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsServerCertLastUpdated :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsServerCertLastUpdated = Lens.lens (serverCertLastUpdated :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {serverCertLastUpdated = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsServerCertLastUpdated "Use generic-lens or generic-optics with 'serverCertLastUpdated' instead." #-}

-- | The HSM software version.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSoftwareVersion :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsSoftwareVersion = Lens.lens (softwareVersion :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {softwareVersion = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

-- | The name of the HSM vendor.
--
-- /Note:/ Consider using 'vendorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsVendorName :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsVendorName = Lens.lens (vendorName :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {vendorName = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsVendorName "Use generic-lens or generic-optics with 'vendorName' instead." #-}

-- | The serial number of the HSM.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSerialNumber :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsSerialNumber = Lens.lens (serialNumber :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The ARN of the HSM.
--
-- /Note:/ Consider using 'hsmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsHSMARN :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsHSMARN = Lens.lens (hsmARN :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {hsmARN = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsHSMARN "Use generic-lens or generic-optics with 'hsmARN' instead." #-}

-- | The IP address assigned to the HSM's ENI.
--
-- /Note:/ Consider using 'eniIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEniIP :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsEniIP = Lens.lens (eniIP :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {eniIP = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsEniIP "Use generic-lens or generic-optics with 'eniIP' instead." #-}

-- | The HSM model type.
--
-- /Note:/ Consider using 'hsmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsHSMType :: Lens.Lens' DescribeHSMResponse (Lude.Maybe Lude.Text)
desrsHSMType = Lens.lens (hsmType :: DescribeHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {hsmType = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsHSMType "Use generic-lens or generic-optics with 'hsmType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeHSMResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeHSMResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHSMResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

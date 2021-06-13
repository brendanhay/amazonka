{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHsm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Retrieves information about an HSM. You can identify the HSM by its ARN
-- or its serial number.
module Network.AWS.CloudHSM.DescribeHsm
  ( -- * Creating a Request
    DescribeHsm (..),
    newDescribeHsm,

    -- * Request Lenses
    describeHsm_hsmSerialNumber,
    describeHsm_hsmArn,

    -- * Destructuring the Response
    DescribeHsmResponse (..),
    newDescribeHsmResponse,

    -- * Response Lenses
    describeHsmResponse_subscriptionStartDate,
    describeHsmResponse_iamRoleArn,
    describeHsmResponse_status,
    describeHsmResponse_partitions,
    describeHsmResponse_statusDetails,
    describeHsmResponse_eniIp,
    describeHsmResponse_subscriptionType,
    describeHsmResponse_serverCertLastUpdated,
    describeHsmResponse_eniId,
    describeHsmResponse_availabilityZone,
    describeHsmResponse_sshPublicKey,
    describeHsmResponse_hsmType,
    describeHsmResponse_subnetId,
    describeHsmResponse_vendorName,
    describeHsmResponse_serverCertUri,
    describeHsmResponse_hsmArn,
    describeHsmResponse_serialNumber,
    describeHsmResponse_sshKeyLastUpdated,
    describeHsmResponse_subscriptionEndDate,
    describeHsmResponse_vpcId,
    describeHsmResponse_softwareVersion,
    describeHsmResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DescribeHsm operation.
--
-- /See:/ 'newDescribeHsm' smart constructor.
data DescribeHsm = DescribeHsm'
  { -- | The serial number of the HSM. Either the @HsmArn@ or the
    -- @HsmSerialNumber@ parameter must be specified.
    hsmSerialNumber :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter
    -- must be specified.
    hsmArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmSerialNumber', 'describeHsm_hsmSerialNumber' - The serial number of the HSM. Either the @HsmArn@ or the
-- @HsmSerialNumber@ parameter must be specified.
--
-- 'hsmArn', 'describeHsm_hsmArn' - The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter
-- must be specified.
newDescribeHsm ::
  DescribeHsm
newDescribeHsm =
  DescribeHsm'
    { hsmSerialNumber = Prelude.Nothing,
      hsmArn = Prelude.Nothing
    }

-- | The serial number of the HSM. Either the @HsmArn@ or the
-- @HsmSerialNumber@ parameter must be specified.
describeHsm_hsmSerialNumber :: Lens.Lens' DescribeHsm (Prelude.Maybe Prelude.Text)
describeHsm_hsmSerialNumber = Lens.lens (\DescribeHsm' {hsmSerialNumber} -> hsmSerialNumber) (\s@DescribeHsm' {} a -> s {hsmSerialNumber = a} :: DescribeHsm)

-- | The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter
-- must be specified.
describeHsm_hsmArn :: Lens.Lens' DescribeHsm (Prelude.Maybe Prelude.Text)
describeHsm_hsmArn = Lens.lens (\DescribeHsm' {hsmArn} -> hsmArn) (\s@DescribeHsm' {} a -> s {hsmArn = a} :: DescribeHsm)

instance Core.AWSRequest DescribeHsm where
  type AWSResponse DescribeHsm = DescribeHsmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHsmResponse'
            Prelude.<$> (x Core..?> "SubscriptionStartDate")
            Prelude.<*> (x Core..?> "IamRoleArn")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Partitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "StatusDetails")
            Prelude.<*> (x Core..?> "EniIp")
            Prelude.<*> (x Core..?> "SubscriptionType")
            Prelude.<*> (x Core..?> "ServerCertLastUpdated")
            Prelude.<*> (x Core..?> "EniId")
            Prelude.<*> (x Core..?> "AvailabilityZone")
            Prelude.<*> (x Core..?> "SshPublicKey")
            Prelude.<*> (x Core..?> "HsmType")
            Prelude.<*> (x Core..?> "SubnetId")
            Prelude.<*> (x Core..?> "VendorName")
            Prelude.<*> (x Core..?> "ServerCertUri")
            Prelude.<*> (x Core..?> "HsmArn")
            Prelude.<*> (x Core..?> "SerialNumber")
            Prelude.<*> (x Core..?> "SshKeyLastUpdated")
            Prelude.<*> (x Core..?> "SubscriptionEndDate")
            Prelude.<*> (x Core..?> "VpcId")
            Prelude.<*> (x Core..?> "SoftwareVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHsm

instance Prelude.NFData DescribeHsm

instance Core.ToHeaders DescribeHsm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.DescribeHsm" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeHsm where
  toJSON DescribeHsm' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HsmSerialNumber" Core..=)
              Prelude.<$> hsmSerialNumber,
            ("HsmArn" Core..=) Prelude.<$> hsmArn
          ]
      )

instance Core.ToPath DescribeHsm where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeHsm where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the DescribeHsm operation.
--
-- /See:/ 'newDescribeHsmResponse' smart constructor.
data DescribeHsmResponse = DescribeHsmResponse'
  { -- | The subscription start date.
    subscriptionStartDate :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role assigned to the HSM.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the HSM.
    status :: Prelude.Maybe HsmStatus,
    -- | The list of partitions on the HSM.
    partitions :: Prelude.Maybe [Prelude.Text],
    -- | Contains additional information about the status of the HSM.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the HSM\'s ENI.
    eniIp :: Prelude.Maybe Prelude.Text,
    subscriptionType :: Prelude.Maybe SubscriptionType,
    -- | The date and time that the server certificate was last updated.
    serverCertLastUpdated :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the elastic network interface (ENI) attached to the
    -- HSM.
    eniId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone that the HSM is in.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The public SSH key.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The HSM model type.
    hsmType :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet that the HSM is in.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the HSM vendor.
    vendorName :: Prelude.Maybe Prelude.Text,
    -- | The URI of the certificate server.
    serverCertUri :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the HSM.
    hsmArn :: Prelude.Maybe Prelude.Text,
    -- | The serial number of the HSM.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the SSH key was last updated.
    sshKeyLastUpdated :: Prelude.Maybe Prelude.Text,
    -- | The subscription end date.
    subscriptionEndDate :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that the HSM is in.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The HSM software version.
    softwareVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHsmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionStartDate', 'describeHsmResponse_subscriptionStartDate' - The subscription start date.
--
-- 'iamRoleArn', 'describeHsmResponse_iamRoleArn' - The ARN of the IAM role assigned to the HSM.
--
-- 'status', 'describeHsmResponse_status' - The status of the HSM.
--
-- 'partitions', 'describeHsmResponse_partitions' - The list of partitions on the HSM.
--
-- 'statusDetails', 'describeHsmResponse_statusDetails' - Contains additional information about the status of the HSM.
--
-- 'eniIp', 'describeHsmResponse_eniIp' - The IP address assigned to the HSM\'s ENI.
--
-- 'subscriptionType', 'describeHsmResponse_subscriptionType' - Undocumented member.
--
-- 'serverCertLastUpdated', 'describeHsmResponse_serverCertLastUpdated' - The date and time that the server certificate was last updated.
--
-- 'eniId', 'describeHsmResponse_eniId' - The identifier of the elastic network interface (ENI) attached to the
-- HSM.
--
-- 'availabilityZone', 'describeHsmResponse_availabilityZone' - The Availability Zone that the HSM is in.
--
-- 'sshPublicKey', 'describeHsmResponse_sshPublicKey' - The public SSH key.
--
-- 'hsmType', 'describeHsmResponse_hsmType' - The HSM model type.
--
-- 'subnetId', 'describeHsmResponse_subnetId' - The identifier of the subnet that the HSM is in.
--
-- 'vendorName', 'describeHsmResponse_vendorName' - The name of the HSM vendor.
--
-- 'serverCertUri', 'describeHsmResponse_serverCertUri' - The URI of the certificate server.
--
-- 'hsmArn', 'describeHsmResponse_hsmArn' - The ARN of the HSM.
--
-- 'serialNumber', 'describeHsmResponse_serialNumber' - The serial number of the HSM.
--
-- 'sshKeyLastUpdated', 'describeHsmResponse_sshKeyLastUpdated' - The date and time that the SSH key was last updated.
--
-- 'subscriptionEndDate', 'describeHsmResponse_subscriptionEndDate' - The subscription end date.
--
-- 'vpcId', 'describeHsmResponse_vpcId' - The identifier of the VPC that the HSM is in.
--
-- 'softwareVersion', 'describeHsmResponse_softwareVersion' - The HSM software version.
--
-- 'httpStatus', 'describeHsmResponse_httpStatus' - The response's http status code.
newDescribeHsmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHsmResponse
newDescribeHsmResponse pHttpStatus_ =
  DescribeHsmResponse'
    { subscriptionStartDate =
        Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      partitions = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      eniIp = Prelude.Nothing,
      subscriptionType = Prelude.Nothing,
      serverCertLastUpdated = Prelude.Nothing,
      eniId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      hsmType = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      vendorName = Prelude.Nothing,
      serverCertUri = Prelude.Nothing,
      hsmArn = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      sshKeyLastUpdated = Prelude.Nothing,
      subscriptionEndDate = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      softwareVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The subscription start date.
describeHsmResponse_subscriptionStartDate :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_subscriptionStartDate = Lens.lens (\DescribeHsmResponse' {subscriptionStartDate} -> subscriptionStartDate) (\s@DescribeHsmResponse' {} a -> s {subscriptionStartDate = a} :: DescribeHsmResponse)

-- | The ARN of the IAM role assigned to the HSM.
describeHsmResponse_iamRoleArn :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_iamRoleArn = Lens.lens (\DescribeHsmResponse' {iamRoleArn} -> iamRoleArn) (\s@DescribeHsmResponse' {} a -> s {iamRoleArn = a} :: DescribeHsmResponse)

-- | The status of the HSM.
describeHsmResponse_status :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe HsmStatus)
describeHsmResponse_status = Lens.lens (\DescribeHsmResponse' {status} -> status) (\s@DescribeHsmResponse' {} a -> s {status = a} :: DescribeHsmResponse)

-- | The list of partitions on the HSM.
describeHsmResponse_partitions :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe [Prelude.Text])
describeHsmResponse_partitions = Lens.lens (\DescribeHsmResponse' {partitions} -> partitions) (\s@DescribeHsmResponse' {} a -> s {partitions = a} :: DescribeHsmResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Contains additional information about the status of the HSM.
describeHsmResponse_statusDetails :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_statusDetails = Lens.lens (\DescribeHsmResponse' {statusDetails} -> statusDetails) (\s@DescribeHsmResponse' {} a -> s {statusDetails = a} :: DescribeHsmResponse)

-- | The IP address assigned to the HSM\'s ENI.
describeHsmResponse_eniIp :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_eniIp = Lens.lens (\DescribeHsmResponse' {eniIp} -> eniIp) (\s@DescribeHsmResponse' {} a -> s {eniIp = a} :: DescribeHsmResponse)

-- | Undocumented member.
describeHsmResponse_subscriptionType :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe SubscriptionType)
describeHsmResponse_subscriptionType = Lens.lens (\DescribeHsmResponse' {subscriptionType} -> subscriptionType) (\s@DescribeHsmResponse' {} a -> s {subscriptionType = a} :: DescribeHsmResponse)

-- | The date and time that the server certificate was last updated.
describeHsmResponse_serverCertLastUpdated :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_serverCertLastUpdated = Lens.lens (\DescribeHsmResponse' {serverCertLastUpdated} -> serverCertLastUpdated) (\s@DescribeHsmResponse' {} a -> s {serverCertLastUpdated = a} :: DescribeHsmResponse)

-- | The identifier of the elastic network interface (ENI) attached to the
-- HSM.
describeHsmResponse_eniId :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_eniId = Lens.lens (\DescribeHsmResponse' {eniId} -> eniId) (\s@DescribeHsmResponse' {} a -> s {eniId = a} :: DescribeHsmResponse)

-- | The Availability Zone that the HSM is in.
describeHsmResponse_availabilityZone :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_availabilityZone = Lens.lens (\DescribeHsmResponse' {availabilityZone} -> availabilityZone) (\s@DescribeHsmResponse' {} a -> s {availabilityZone = a} :: DescribeHsmResponse)

-- | The public SSH key.
describeHsmResponse_sshPublicKey :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_sshPublicKey = Lens.lens (\DescribeHsmResponse' {sshPublicKey} -> sshPublicKey) (\s@DescribeHsmResponse' {} a -> s {sshPublicKey = a} :: DescribeHsmResponse)

-- | The HSM model type.
describeHsmResponse_hsmType :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_hsmType = Lens.lens (\DescribeHsmResponse' {hsmType} -> hsmType) (\s@DescribeHsmResponse' {} a -> s {hsmType = a} :: DescribeHsmResponse)

-- | The identifier of the subnet that the HSM is in.
describeHsmResponse_subnetId :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_subnetId = Lens.lens (\DescribeHsmResponse' {subnetId} -> subnetId) (\s@DescribeHsmResponse' {} a -> s {subnetId = a} :: DescribeHsmResponse)

-- | The name of the HSM vendor.
describeHsmResponse_vendorName :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_vendorName = Lens.lens (\DescribeHsmResponse' {vendorName} -> vendorName) (\s@DescribeHsmResponse' {} a -> s {vendorName = a} :: DescribeHsmResponse)

-- | The URI of the certificate server.
describeHsmResponse_serverCertUri :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_serverCertUri = Lens.lens (\DescribeHsmResponse' {serverCertUri} -> serverCertUri) (\s@DescribeHsmResponse' {} a -> s {serverCertUri = a} :: DescribeHsmResponse)

-- | The ARN of the HSM.
describeHsmResponse_hsmArn :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_hsmArn = Lens.lens (\DescribeHsmResponse' {hsmArn} -> hsmArn) (\s@DescribeHsmResponse' {} a -> s {hsmArn = a} :: DescribeHsmResponse)

-- | The serial number of the HSM.
describeHsmResponse_serialNumber :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_serialNumber = Lens.lens (\DescribeHsmResponse' {serialNumber} -> serialNumber) (\s@DescribeHsmResponse' {} a -> s {serialNumber = a} :: DescribeHsmResponse)

-- | The date and time that the SSH key was last updated.
describeHsmResponse_sshKeyLastUpdated :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_sshKeyLastUpdated = Lens.lens (\DescribeHsmResponse' {sshKeyLastUpdated} -> sshKeyLastUpdated) (\s@DescribeHsmResponse' {} a -> s {sshKeyLastUpdated = a} :: DescribeHsmResponse)

-- | The subscription end date.
describeHsmResponse_subscriptionEndDate :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_subscriptionEndDate = Lens.lens (\DescribeHsmResponse' {subscriptionEndDate} -> subscriptionEndDate) (\s@DescribeHsmResponse' {} a -> s {subscriptionEndDate = a} :: DescribeHsmResponse)

-- | The identifier of the VPC that the HSM is in.
describeHsmResponse_vpcId :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_vpcId = Lens.lens (\DescribeHsmResponse' {vpcId} -> vpcId) (\s@DescribeHsmResponse' {} a -> s {vpcId = a} :: DescribeHsmResponse)

-- | The HSM software version.
describeHsmResponse_softwareVersion :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_softwareVersion = Lens.lens (\DescribeHsmResponse' {softwareVersion} -> softwareVersion) (\s@DescribeHsmResponse' {} a -> s {softwareVersion = a} :: DescribeHsmResponse)

-- | The response's http status code.
describeHsmResponse_httpStatus :: Lens.Lens' DescribeHsmResponse Prelude.Int
describeHsmResponse_httpStatus = Lens.lens (\DescribeHsmResponse' {httpStatus} -> httpStatus) (\s@DescribeHsmResponse' {} a -> s {httpStatus = a} :: DescribeHsmResponse)

instance Prelude.NFData DescribeHsmResponse

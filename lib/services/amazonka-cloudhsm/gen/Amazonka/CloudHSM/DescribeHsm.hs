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
-- Module      : Amazonka.CloudHSM.DescribeHsm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Retrieves information about an HSM. You can identify the HSM by its ARN
-- or its serial number.
module Amazonka.CloudHSM.DescribeHsm
  ( -- * Creating a Request
    DescribeHsm (..),
    newDescribeHsm,

    -- * Request Lenses
    describeHsm_hsmArn,
    describeHsm_hsmSerialNumber,

    -- * Destructuring the Response
    DescribeHsmResponse (..),
    newDescribeHsmResponse,

    -- * Response Lenses
    describeHsmResponse_availabilityZone,
    describeHsmResponse_eniId,
    describeHsmResponse_eniIp,
    describeHsmResponse_hsmArn,
    describeHsmResponse_hsmType,
    describeHsmResponse_iamRoleArn,
    describeHsmResponse_partitions,
    describeHsmResponse_serialNumber,
    describeHsmResponse_serverCertLastUpdated,
    describeHsmResponse_serverCertUri,
    describeHsmResponse_softwareVersion,
    describeHsmResponse_sshKeyLastUpdated,
    describeHsmResponse_sshPublicKey,
    describeHsmResponse_status,
    describeHsmResponse_statusDetails,
    describeHsmResponse_subnetId,
    describeHsmResponse_subscriptionEndDate,
    describeHsmResponse_subscriptionStartDate,
    describeHsmResponse_subscriptionType,
    describeHsmResponse_vendorName,
    describeHsmResponse_vpcId,
    describeHsmResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DescribeHsm operation.
--
-- /See:/ 'newDescribeHsm' smart constructor.
data DescribeHsm = DescribeHsm'
  { -- | The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter
    -- must be specified.
    hsmArn :: Prelude.Maybe Prelude.Text,
    -- | The serial number of the HSM. Either the @HsmArn@ or the
    -- @HsmSerialNumber@ parameter must be specified.
    hsmSerialNumber :: Prelude.Maybe Prelude.Text
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
-- 'hsmArn', 'describeHsm_hsmArn' - The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter
-- must be specified.
--
-- 'hsmSerialNumber', 'describeHsm_hsmSerialNumber' - The serial number of the HSM. Either the @HsmArn@ or the
-- @HsmSerialNumber@ parameter must be specified.
newDescribeHsm ::
  DescribeHsm
newDescribeHsm =
  DescribeHsm'
    { hsmArn = Prelude.Nothing,
      hsmSerialNumber = Prelude.Nothing
    }

-- | The ARN of the HSM. Either the @HsmArn@ or the @SerialNumber@ parameter
-- must be specified.
describeHsm_hsmArn :: Lens.Lens' DescribeHsm (Prelude.Maybe Prelude.Text)
describeHsm_hsmArn = Lens.lens (\DescribeHsm' {hsmArn} -> hsmArn) (\s@DescribeHsm' {} a -> s {hsmArn = a} :: DescribeHsm)

-- | The serial number of the HSM. Either the @HsmArn@ or the
-- @HsmSerialNumber@ parameter must be specified.
describeHsm_hsmSerialNumber :: Lens.Lens' DescribeHsm (Prelude.Maybe Prelude.Text)
describeHsm_hsmSerialNumber = Lens.lens (\DescribeHsm' {hsmSerialNumber} -> hsmSerialNumber) (\s@DescribeHsm' {} a -> s {hsmSerialNumber = a} :: DescribeHsm)

instance Core.AWSRequest DescribeHsm where
  type AWSResponse DescribeHsm = DescribeHsmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHsmResponse'
            Prelude.<$> (x Data..?> "AvailabilityZone")
            Prelude.<*> (x Data..?> "EniId")
            Prelude.<*> (x Data..?> "EniIp")
            Prelude.<*> (x Data..?> "HsmArn")
            Prelude.<*> (x Data..?> "HsmType")
            Prelude.<*> (x Data..?> "IamRoleArn")
            Prelude.<*> (x Data..?> "Partitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SerialNumber")
            Prelude.<*> (x Data..?> "ServerCertLastUpdated")
            Prelude.<*> (x Data..?> "ServerCertUri")
            Prelude.<*> (x Data..?> "SoftwareVersion")
            Prelude.<*> (x Data..?> "SshKeyLastUpdated")
            Prelude.<*> (x Data..?> "SshPublicKey")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusDetails")
            Prelude.<*> (x Data..?> "SubnetId")
            Prelude.<*> (x Data..?> "SubscriptionEndDate")
            Prelude.<*> (x Data..?> "SubscriptionStartDate")
            Prelude.<*> (x Data..?> "SubscriptionType")
            Prelude.<*> (x Data..?> "VendorName")
            Prelude.<*> (x Data..?> "VpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHsm where
  hashWithSalt _salt DescribeHsm' {..} =
    _salt `Prelude.hashWithSalt` hsmArn
      `Prelude.hashWithSalt` hsmSerialNumber

instance Prelude.NFData DescribeHsm where
  rnf DescribeHsm' {..} =
    Prelude.rnf hsmArn
      `Prelude.seq` Prelude.rnf hsmSerialNumber

instance Data.ToHeaders DescribeHsm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.DescribeHsm" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeHsm where
  toJSON DescribeHsm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HsmArn" Data..=) Prelude.<$> hsmArn,
            ("HsmSerialNumber" Data..=)
              Prelude.<$> hsmSerialNumber
          ]
      )

instance Data.ToPath DescribeHsm where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHsm where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the DescribeHsm operation.
--
-- /See:/ 'newDescribeHsmResponse' smart constructor.
data DescribeHsmResponse = DescribeHsmResponse'
  { -- | The Availability Zone that the HSM is in.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the elastic network interface (ENI) attached to the
    -- HSM.
    eniId :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the HSM\'s ENI.
    eniIp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the HSM.
    hsmArn :: Prelude.Maybe Prelude.Text,
    -- | The HSM model type.
    hsmType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role assigned to the HSM.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The list of partitions on the HSM.
    partitions :: Prelude.Maybe [Prelude.Text],
    -- | The serial number of the HSM.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the server certificate was last updated.
    serverCertLastUpdated :: Prelude.Maybe Prelude.Text,
    -- | The URI of the certificate server.
    serverCertUri :: Prelude.Maybe Prelude.Text,
    -- | The HSM software version.
    softwareVersion :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the SSH key was last updated.
    sshKeyLastUpdated :: Prelude.Maybe Prelude.Text,
    -- | The public SSH key.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The status of the HSM.
    status :: Prelude.Maybe HsmStatus,
    -- | Contains additional information about the status of the HSM.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet that the HSM is in.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The subscription end date.
    subscriptionEndDate :: Prelude.Maybe Prelude.Text,
    -- | The subscription start date.
    subscriptionStartDate :: Prelude.Maybe Prelude.Text,
    subscriptionType :: Prelude.Maybe SubscriptionType,
    -- | The name of the HSM vendor.
    vendorName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that the HSM is in.
    vpcId :: Prelude.Maybe Prelude.Text,
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
-- 'availabilityZone', 'describeHsmResponse_availabilityZone' - The Availability Zone that the HSM is in.
--
-- 'eniId', 'describeHsmResponse_eniId' - The identifier of the elastic network interface (ENI) attached to the
-- HSM.
--
-- 'eniIp', 'describeHsmResponse_eniIp' - The IP address assigned to the HSM\'s ENI.
--
-- 'hsmArn', 'describeHsmResponse_hsmArn' - The ARN of the HSM.
--
-- 'hsmType', 'describeHsmResponse_hsmType' - The HSM model type.
--
-- 'iamRoleArn', 'describeHsmResponse_iamRoleArn' - The ARN of the IAM role assigned to the HSM.
--
-- 'partitions', 'describeHsmResponse_partitions' - The list of partitions on the HSM.
--
-- 'serialNumber', 'describeHsmResponse_serialNumber' - The serial number of the HSM.
--
-- 'serverCertLastUpdated', 'describeHsmResponse_serverCertLastUpdated' - The date and time that the server certificate was last updated.
--
-- 'serverCertUri', 'describeHsmResponse_serverCertUri' - The URI of the certificate server.
--
-- 'softwareVersion', 'describeHsmResponse_softwareVersion' - The HSM software version.
--
-- 'sshKeyLastUpdated', 'describeHsmResponse_sshKeyLastUpdated' - The date and time that the SSH key was last updated.
--
-- 'sshPublicKey', 'describeHsmResponse_sshPublicKey' - The public SSH key.
--
-- 'status', 'describeHsmResponse_status' - The status of the HSM.
--
-- 'statusDetails', 'describeHsmResponse_statusDetails' - Contains additional information about the status of the HSM.
--
-- 'subnetId', 'describeHsmResponse_subnetId' - The identifier of the subnet that the HSM is in.
--
-- 'subscriptionEndDate', 'describeHsmResponse_subscriptionEndDate' - The subscription end date.
--
-- 'subscriptionStartDate', 'describeHsmResponse_subscriptionStartDate' - The subscription start date.
--
-- 'subscriptionType', 'describeHsmResponse_subscriptionType' - Undocumented member.
--
-- 'vendorName', 'describeHsmResponse_vendorName' - The name of the HSM vendor.
--
-- 'vpcId', 'describeHsmResponse_vpcId' - The identifier of the VPC that the HSM is in.
--
-- 'httpStatus', 'describeHsmResponse_httpStatus' - The response's http status code.
newDescribeHsmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHsmResponse
newDescribeHsmResponse pHttpStatus_ =
  DescribeHsmResponse'
    { availabilityZone =
        Prelude.Nothing,
      eniId = Prelude.Nothing,
      eniIp = Prelude.Nothing,
      hsmArn = Prelude.Nothing,
      hsmType = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      partitions = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      serverCertLastUpdated = Prelude.Nothing,
      serverCertUri = Prelude.Nothing,
      softwareVersion = Prelude.Nothing,
      sshKeyLastUpdated = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      subscriptionEndDate = Prelude.Nothing,
      subscriptionStartDate = Prelude.Nothing,
      subscriptionType = Prelude.Nothing,
      vendorName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Availability Zone that the HSM is in.
describeHsmResponse_availabilityZone :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_availabilityZone = Lens.lens (\DescribeHsmResponse' {availabilityZone} -> availabilityZone) (\s@DescribeHsmResponse' {} a -> s {availabilityZone = a} :: DescribeHsmResponse)

-- | The identifier of the elastic network interface (ENI) attached to the
-- HSM.
describeHsmResponse_eniId :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_eniId = Lens.lens (\DescribeHsmResponse' {eniId} -> eniId) (\s@DescribeHsmResponse' {} a -> s {eniId = a} :: DescribeHsmResponse)

-- | The IP address assigned to the HSM\'s ENI.
describeHsmResponse_eniIp :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_eniIp = Lens.lens (\DescribeHsmResponse' {eniIp} -> eniIp) (\s@DescribeHsmResponse' {} a -> s {eniIp = a} :: DescribeHsmResponse)

-- | The ARN of the HSM.
describeHsmResponse_hsmArn :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_hsmArn = Lens.lens (\DescribeHsmResponse' {hsmArn} -> hsmArn) (\s@DescribeHsmResponse' {} a -> s {hsmArn = a} :: DescribeHsmResponse)

-- | The HSM model type.
describeHsmResponse_hsmType :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_hsmType = Lens.lens (\DescribeHsmResponse' {hsmType} -> hsmType) (\s@DescribeHsmResponse' {} a -> s {hsmType = a} :: DescribeHsmResponse)

-- | The ARN of the IAM role assigned to the HSM.
describeHsmResponse_iamRoleArn :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_iamRoleArn = Lens.lens (\DescribeHsmResponse' {iamRoleArn} -> iamRoleArn) (\s@DescribeHsmResponse' {} a -> s {iamRoleArn = a} :: DescribeHsmResponse)

-- | The list of partitions on the HSM.
describeHsmResponse_partitions :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe [Prelude.Text])
describeHsmResponse_partitions = Lens.lens (\DescribeHsmResponse' {partitions} -> partitions) (\s@DescribeHsmResponse' {} a -> s {partitions = a} :: DescribeHsmResponse) Prelude.. Lens.mapping Lens.coerced

-- | The serial number of the HSM.
describeHsmResponse_serialNumber :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_serialNumber = Lens.lens (\DescribeHsmResponse' {serialNumber} -> serialNumber) (\s@DescribeHsmResponse' {} a -> s {serialNumber = a} :: DescribeHsmResponse)

-- | The date and time that the server certificate was last updated.
describeHsmResponse_serverCertLastUpdated :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_serverCertLastUpdated = Lens.lens (\DescribeHsmResponse' {serverCertLastUpdated} -> serverCertLastUpdated) (\s@DescribeHsmResponse' {} a -> s {serverCertLastUpdated = a} :: DescribeHsmResponse)

-- | The URI of the certificate server.
describeHsmResponse_serverCertUri :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_serverCertUri = Lens.lens (\DescribeHsmResponse' {serverCertUri} -> serverCertUri) (\s@DescribeHsmResponse' {} a -> s {serverCertUri = a} :: DescribeHsmResponse)

-- | The HSM software version.
describeHsmResponse_softwareVersion :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_softwareVersion = Lens.lens (\DescribeHsmResponse' {softwareVersion} -> softwareVersion) (\s@DescribeHsmResponse' {} a -> s {softwareVersion = a} :: DescribeHsmResponse)

-- | The date and time that the SSH key was last updated.
describeHsmResponse_sshKeyLastUpdated :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_sshKeyLastUpdated = Lens.lens (\DescribeHsmResponse' {sshKeyLastUpdated} -> sshKeyLastUpdated) (\s@DescribeHsmResponse' {} a -> s {sshKeyLastUpdated = a} :: DescribeHsmResponse)

-- | The public SSH key.
describeHsmResponse_sshPublicKey :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_sshPublicKey = Lens.lens (\DescribeHsmResponse' {sshPublicKey} -> sshPublicKey) (\s@DescribeHsmResponse' {} a -> s {sshPublicKey = a} :: DescribeHsmResponse)

-- | The status of the HSM.
describeHsmResponse_status :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe HsmStatus)
describeHsmResponse_status = Lens.lens (\DescribeHsmResponse' {status} -> status) (\s@DescribeHsmResponse' {} a -> s {status = a} :: DescribeHsmResponse)

-- | Contains additional information about the status of the HSM.
describeHsmResponse_statusDetails :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_statusDetails = Lens.lens (\DescribeHsmResponse' {statusDetails} -> statusDetails) (\s@DescribeHsmResponse' {} a -> s {statusDetails = a} :: DescribeHsmResponse)

-- | The identifier of the subnet that the HSM is in.
describeHsmResponse_subnetId :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_subnetId = Lens.lens (\DescribeHsmResponse' {subnetId} -> subnetId) (\s@DescribeHsmResponse' {} a -> s {subnetId = a} :: DescribeHsmResponse)

-- | The subscription end date.
describeHsmResponse_subscriptionEndDate :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_subscriptionEndDate = Lens.lens (\DescribeHsmResponse' {subscriptionEndDate} -> subscriptionEndDate) (\s@DescribeHsmResponse' {} a -> s {subscriptionEndDate = a} :: DescribeHsmResponse)

-- | The subscription start date.
describeHsmResponse_subscriptionStartDate :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_subscriptionStartDate = Lens.lens (\DescribeHsmResponse' {subscriptionStartDate} -> subscriptionStartDate) (\s@DescribeHsmResponse' {} a -> s {subscriptionStartDate = a} :: DescribeHsmResponse)

-- | Undocumented member.
describeHsmResponse_subscriptionType :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe SubscriptionType)
describeHsmResponse_subscriptionType = Lens.lens (\DescribeHsmResponse' {subscriptionType} -> subscriptionType) (\s@DescribeHsmResponse' {} a -> s {subscriptionType = a} :: DescribeHsmResponse)

-- | The name of the HSM vendor.
describeHsmResponse_vendorName :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_vendorName = Lens.lens (\DescribeHsmResponse' {vendorName} -> vendorName) (\s@DescribeHsmResponse' {} a -> s {vendorName = a} :: DescribeHsmResponse)

-- | The identifier of the VPC that the HSM is in.
describeHsmResponse_vpcId :: Lens.Lens' DescribeHsmResponse (Prelude.Maybe Prelude.Text)
describeHsmResponse_vpcId = Lens.lens (\DescribeHsmResponse' {vpcId} -> vpcId) (\s@DescribeHsmResponse' {} a -> s {vpcId = a} :: DescribeHsmResponse)

-- | The response's http status code.
describeHsmResponse_httpStatus :: Lens.Lens' DescribeHsmResponse Prelude.Int
describeHsmResponse_httpStatus = Lens.lens (\DescribeHsmResponse' {httpStatus} -> httpStatus) (\s@DescribeHsmResponse' {} a -> s {httpStatus = a} :: DescribeHsmResponse)

instance Prelude.NFData DescribeHsmResponse where
  rnf DescribeHsmResponse' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf eniId
      `Prelude.seq` Prelude.rnf eniIp
      `Prelude.seq` Prelude.rnf hsmArn
      `Prelude.seq` Prelude.rnf hsmType
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf partitions
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf serverCertLastUpdated
      `Prelude.seq` Prelude.rnf serverCertUri
      `Prelude.seq` Prelude.rnf softwareVersion
      `Prelude.seq` Prelude.rnf sshKeyLastUpdated
      `Prelude.seq` Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf subscriptionEndDate
      `Prelude.seq` Prelude.rnf subscriptionStartDate
      `Prelude.seq` Prelude.rnf subscriptionType
      `Prelude.seq` Prelude.rnf vendorName
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus

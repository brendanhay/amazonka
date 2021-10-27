{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecurityHub.Types.AwsEc2InstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEc2InstanceDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails

-- | The details of an EC2 instance.
--
-- /See:/ 'newAwsEc2InstanceDetails' smart constructor.
data AwsEc2InstanceDetails = AwsEc2InstanceDetails'
  { -- | The IAM profile ARN of the instance.
    iamInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that the instance was launched in.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The key name associated with the instance.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the network interfaces for the EC2 instance. The
    -- details for each network interface are in a corresponding
    -- @AwsEc2NetworkInterfacesDetails@ object.
    networkInterfaces :: Prelude.Maybe [AwsEc2InstanceNetworkInterfacesDetails],
    -- | The IPv4 addresses associated with the instance.
    ipV4Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the subnet that the instance was launched in.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Machine Image (AMI) ID of the instance.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The instance type of the instance.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 addresses associated with the instance.
    ipV6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | Indicates when the instance was launched.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    launchedAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2InstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamInstanceProfileArn', 'awsEc2InstanceDetails_iamInstanceProfileArn' - The IAM profile ARN of the instance.
--
-- 'vpcId', 'awsEc2InstanceDetails_vpcId' - The identifier of the VPC that the instance was launched in.
--
-- 'keyName', 'awsEc2InstanceDetails_keyName' - The key name associated with the instance.
--
-- 'networkInterfaces', 'awsEc2InstanceDetails_networkInterfaces' - The identifiers of the network interfaces for the EC2 instance. The
-- details for each network interface are in a corresponding
-- @AwsEc2NetworkInterfacesDetails@ object.
--
-- 'ipV4Addresses', 'awsEc2InstanceDetails_ipV4Addresses' - The IPv4 addresses associated with the instance.
--
-- 'subnetId', 'awsEc2InstanceDetails_subnetId' - The identifier of the subnet that the instance was launched in.
--
-- 'imageId', 'awsEc2InstanceDetails_imageId' - The Amazon Machine Image (AMI) ID of the instance.
--
-- 'type'', 'awsEc2InstanceDetails_type' - The instance type of the instance.
--
-- 'ipV6Addresses', 'awsEc2InstanceDetails_ipV6Addresses' - The IPv6 addresses associated with the instance.
--
-- 'launchedAt', 'awsEc2InstanceDetails_launchedAt' - Indicates when the instance was launched.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newAwsEc2InstanceDetails ::
  AwsEc2InstanceDetails
newAwsEc2InstanceDetails =
  AwsEc2InstanceDetails'
    { iamInstanceProfileArn =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      ipV4Addresses = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      imageId = Prelude.Nothing,
      type' = Prelude.Nothing,
      ipV6Addresses = Prelude.Nothing,
      launchedAt = Prelude.Nothing
    }

-- | The IAM profile ARN of the instance.
awsEc2InstanceDetails_iamInstanceProfileArn :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_iamInstanceProfileArn = Lens.lens (\AwsEc2InstanceDetails' {iamInstanceProfileArn} -> iamInstanceProfileArn) (\s@AwsEc2InstanceDetails' {} a -> s {iamInstanceProfileArn = a} :: AwsEc2InstanceDetails)

-- | The identifier of the VPC that the instance was launched in.
awsEc2InstanceDetails_vpcId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_vpcId = Lens.lens (\AwsEc2InstanceDetails' {vpcId} -> vpcId) (\s@AwsEc2InstanceDetails' {} a -> s {vpcId = a} :: AwsEc2InstanceDetails)

-- | The key name associated with the instance.
awsEc2InstanceDetails_keyName :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_keyName = Lens.lens (\AwsEc2InstanceDetails' {keyName} -> keyName) (\s@AwsEc2InstanceDetails' {} a -> s {keyName = a} :: AwsEc2InstanceDetails)

-- | The identifiers of the network interfaces for the EC2 instance. The
-- details for each network interface are in a corresponding
-- @AwsEc2NetworkInterfacesDetails@ object.
awsEc2InstanceDetails_networkInterfaces :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [AwsEc2InstanceNetworkInterfacesDetails])
awsEc2InstanceDetails_networkInterfaces = Lens.lens (\AwsEc2InstanceDetails' {networkInterfaces} -> networkInterfaces) (\s@AwsEc2InstanceDetails' {} a -> s {networkInterfaces = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The IPv4 addresses associated with the instance.
awsEc2InstanceDetails_ipV4Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV4Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV4Addresses} -> ipV4Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV4Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the subnet that the instance was launched in.
awsEc2InstanceDetails_subnetId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_subnetId = Lens.lens (\AwsEc2InstanceDetails' {subnetId} -> subnetId) (\s@AwsEc2InstanceDetails' {} a -> s {subnetId = a} :: AwsEc2InstanceDetails)

-- | The Amazon Machine Image (AMI) ID of the instance.
awsEc2InstanceDetails_imageId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_imageId = Lens.lens (\AwsEc2InstanceDetails' {imageId} -> imageId) (\s@AwsEc2InstanceDetails' {} a -> s {imageId = a} :: AwsEc2InstanceDetails)

-- | The instance type of the instance.
awsEc2InstanceDetails_type :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_type = Lens.lens (\AwsEc2InstanceDetails' {type'} -> type') (\s@AwsEc2InstanceDetails' {} a -> s {type' = a} :: AwsEc2InstanceDetails)

-- | The IPv6 addresses associated with the instance.
awsEc2InstanceDetails_ipV6Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV6Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV6Addresses} -> ipV6Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV6Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the instance was launched.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEc2InstanceDetails_launchedAt :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_launchedAt = Lens.lens (\AwsEc2InstanceDetails' {launchedAt} -> launchedAt) (\s@AwsEc2InstanceDetails' {} a -> s {launchedAt = a} :: AwsEc2InstanceDetails)

instance Core.FromJSON AwsEc2InstanceDetails where
  parseJSON =
    Core.withObject
      "AwsEc2InstanceDetails"
      ( \x ->
          AwsEc2InstanceDetails'
            Prelude.<$> (x Core..:? "IamInstanceProfileArn")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "KeyName")
            Prelude.<*> ( x Core..:? "NetworkInterfaces"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "IpV4Addresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "ImageId")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "IpV6Addresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LaunchedAt")
      )

instance Prelude.Hashable AwsEc2InstanceDetails

instance Prelude.NFData AwsEc2InstanceDetails

instance Core.ToJSON AwsEc2InstanceDetails where
  toJSON AwsEc2InstanceDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IamInstanceProfileArn" Core..=)
              Prelude.<$> iamInstanceProfileArn,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("KeyName" Core..=) Prelude.<$> keyName,
            ("NetworkInterfaces" Core..=)
              Prelude.<$> networkInterfaces,
            ("IpV4Addresses" Core..=) Prelude.<$> ipV4Addresses,
            ("SubnetId" Core..=) Prelude.<$> subnetId,
            ("ImageId" Core..=) Prelude.<$> imageId,
            ("Type" Core..=) Prelude.<$> type',
            ("IpV6Addresses" Core..=) Prelude.<$> ipV6Addresses,
            ("LaunchedAt" Core..=) Prelude.<$> launchedAt
          ]
      )

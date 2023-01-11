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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2InstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2InstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2InstanceMetadataOptions
import Amazonka.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails

-- | The details of an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2InstanceDetails' smart constructor.
data AwsEc2InstanceDetails = AwsEc2InstanceDetails'
  { -- | The IAM profile ARN of the instance.
    iamInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Machine Image (AMI) ID of the instance.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 addresses associated with the instance.
    ipV4Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The IPv6 addresses associated with the instance.
    ipV6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The key name associated with the instance.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the instance was launched.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    launchedAt :: Prelude.Maybe Prelude.Text,
    -- | Details about the metadata options for the Amazon EC2 instance.
    metadataOptions :: Prelude.Maybe AwsEc2InstanceMetadataOptions,
    -- | The identifiers of the network interfaces for the EC2 instance. The
    -- details for each network interface are in a corresponding
    -- @AwsEc2NetworkInterfacesDetails@ object.
    networkInterfaces :: Prelude.Maybe [AwsEc2InstanceNetworkInterfacesDetails],
    -- | The identifier of the subnet that the instance was launched in.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The instance type of the instance.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The virtualization type of the Amazon Machine Image (AMI) required to
    -- launch the instance.
    virtualizationType :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that the instance was launched in.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'imageId', 'awsEc2InstanceDetails_imageId' - The Amazon Machine Image (AMI) ID of the instance.
--
-- 'ipV4Addresses', 'awsEc2InstanceDetails_ipV4Addresses' - The IPv4 addresses associated with the instance.
--
-- 'ipV6Addresses', 'awsEc2InstanceDetails_ipV6Addresses' - The IPv6 addresses associated with the instance.
--
-- 'keyName', 'awsEc2InstanceDetails_keyName' - The key name associated with the instance.
--
-- 'launchedAt', 'awsEc2InstanceDetails_launchedAt' - Indicates when the instance was launched.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'metadataOptions', 'awsEc2InstanceDetails_metadataOptions' - Details about the metadata options for the Amazon EC2 instance.
--
-- 'networkInterfaces', 'awsEc2InstanceDetails_networkInterfaces' - The identifiers of the network interfaces for the EC2 instance. The
-- details for each network interface are in a corresponding
-- @AwsEc2NetworkInterfacesDetails@ object.
--
-- 'subnetId', 'awsEc2InstanceDetails_subnetId' - The identifier of the subnet that the instance was launched in.
--
-- 'type'', 'awsEc2InstanceDetails_type' - The instance type of the instance.
--
-- 'virtualizationType', 'awsEc2InstanceDetails_virtualizationType' - The virtualization type of the Amazon Machine Image (AMI) required to
-- launch the instance.
--
-- 'vpcId', 'awsEc2InstanceDetails_vpcId' - The identifier of the VPC that the instance was launched in.
newAwsEc2InstanceDetails ::
  AwsEc2InstanceDetails
newAwsEc2InstanceDetails =
  AwsEc2InstanceDetails'
    { iamInstanceProfileArn =
        Prelude.Nothing,
      imageId = Prelude.Nothing,
      ipV4Addresses = Prelude.Nothing,
      ipV6Addresses = Prelude.Nothing,
      keyName = Prelude.Nothing,
      launchedAt = Prelude.Nothing,
      metadataOptions = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      type' = Prelude.Nothing,
      virtualizationType = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The IAM profile ARN of the instance.
awsEc2InstanceDetails_iamInstanceProfileArn :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_iamInstanceProfileArn = Lens.lens (\AwsEc2InstanceDetails' {iamInstanceProfileArn} -> iamInstanceProfileArn) (\s@AwsEc2InstanceDetails' {} a -> s {iamInstanceProfileArn = a} :: AwsEc2InstanceDetails)

-- | The Amazon Machine Image (AMI) ID of the instance.
awsEc2InstanceDetails_imageId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_imageId = Lens.lens (\AwsEc2InstanceDetails' {imageId} -> imageId) (\s@AwsEc2InstanceDetails' {} a -> s {imageId = a} :: AwsEc2InstanceDetails)

-- | The IPv4 addresses associated with the instance.
awsEc2InstanceDetails_ipV4Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV4Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV4Addresses} -> ipV4Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV4Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 addresses associated with the instance.
awsEc2InstanceDetails_ipV6Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV6Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV6Addresses} -> ipV6Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV6Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The key name associated with the instance.
awsEc2InstanceDetails_keyName :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_keyName = Lens.lens (\AwsEc2InstanceDetails' {keyName} -> keyName) (\s@AwsEc2InstanceDetails' {} a -> s {keyName = a} :: AwsEc2InstanceDetails)

-- | Indicates when the instance was launched.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEc2InstanceDetails_launchedAt :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_launchedAt = Lens.lens (\AwsEc2InstanceDetails' {launchedAt} -> launchedAt) (\s@AwsEc2InstanceDetails' {} a -> s {launchedAt = a} :: AwsEc2InstanceDetails)

-- | Details about the metadata options for the Amazon EC2 instance.
awsEc2InstanceDetails_metadataOptions :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe AwsEc2InstanceMetadataOptions)
awsEc2InstanceDetails_metadataOptions = Lens.lens (\AwsEc2InstanceDetails' {metadataOptions} -> metadataOptions) (\s@AwsEc2InstanceDetails' {} a -> s {metadataOptions = a} :: AwsEc2InstanceDetails)

-- | The identifiers of the network interfaces for the EC2 instance. The
-- details for each network interface are in a corresponding
-- @AwsEc2NetworkInterfacesDetails@ object.
awsEc2InstanceDetails_networkInterfaces :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [AwsEc2InstanceNetworkInterfacesDetails])
awsEc2InstanceDetails_networkInterfaces = Lens.lens (\AwsEc2InstanceDetails' {networkInterfaces} -> networkInterfaces) (\s@AwsEc2InstanceDetails' {} a -> s {networkInterfaces = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the subnet that the instance was launched in.
awsEc2InstanceDetails_subnetId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_subnetId = Lens.lens (\AwsEc2InstanceDetails' {subnetId} -> subnetId) (\s@AwsEc2InstanceDetails' {} a -> s {subnetId = a} :: AwsEc2InstanceDetails)

-- | The instance type of the instance.
awsEc2InstanceDetails_type :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_type = Lens.lens (\AwsEc2InstanceDetails' {type'} -> type') (\s@AwsEc2InstanceDetails' {} a -> s {type' = a} :: AwsEc2InstanceDetails)

-- | The virtualization type of the Amazon Machine Image (AMI) required to
-- launch the instance.
awsEc2InstanceDetails_virtualizationType :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_virtualizationType = Lens.lens (\AwsEc2InstanceDetails' {virtualizationType} -> virtualizationType) (\s@AwsEc2InstanceDetails' {} a -> s {virtualizationType = a} :: AwsEc2InstanceDetails)

-- | The identifier of the VPC that the instance was launched in.
awsEc2InstanceDetails_vpcId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_vpcId = Lens.lens (\AwsEc2InstanceDetails' {vpcId} -> vpcId) (\s@AwsEc2InstanceDetails' {} a -> s {vpcId = a} :: AwsEc2InstanceDetails)

instance Data.FromJSON AwsEc2InstanceDetails where
  parseJSON =
    Data.withObject
      "AwsEc2InstanceDetails"
      ( \x ->
          AwsEc2InstanceDetails'
            Prelude.<$> (x Data..:? "IamInstanceProfileArn")
            Prelude.<*> (x Data..:? "ImageId")
            Prelude.<*> (x Data..:? "IpV4Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IpV6Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "KeyName")
            Prelude.<*> (x Data..:? "LaunchedAt")
            Prelude.<*> (x Data..:? "MetadataOptions")
            Prelude.<*> ( x Data..:? "NetworkInterfaces"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "VirtualizationType")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsEc2InstanceDetails where
  hashWithSalt _salt AwsEc2InstanceDetails' {..} =
    _salt `Prelude.hashWithSalt` iamInstanceProfileArn
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` ipV4Addresses
      `Prelude.hashWithSalt` ipV6Addresses
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` launchedAt
      `Prelude.hashWithSalt` metadataOptions
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` virtualizationType
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsEc2InstanceDetails where
  rnf AwsEc2InstanceDetails' {..} =
    Prelude.rnf iamInstanceProfileArn
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf ipV4Addresses
      `Prelude.seq` Prelude.rnf ipV6Addresses
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf launchedAt
      `Prelude.seq` Prelude.rnf metadataOptions
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf virtualizationType
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON AwsEc2InstanceDetails where
  toJSON AwsEc2InstanceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IamInstanceProfileArn" Data..=)
              Prelude.<$> iamInstanceProfileArn,
            ("ImageId" Data..=) Prelude.<$> imageId,
            ("IpV4Addresses" Data..=) Prelude.<$> ipV4Addresses,
            ("IpV6Addresses" Data..=) Prelude.<$> ipV6Addresses,
            ("KeyName" Data..=) Prelude.<$> keyName,
            ("LaunchedAt" Data..=) Prelude.<$> launchedAt,
            ("MetadataOptions" Data..=)
              Prelude.<$> metadataOptions,
            ("NetworkInterfaces" Data..=)
              Prelude.<$> networkInterfaces,
            ("SubnetId" Data..=) Prelude.<$> subnetId,
            ("Type" Data..=) Prelude.<$> type',
            ("VirtualizationType" Data..=)
              Prelude.<$> virtualizationType,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )

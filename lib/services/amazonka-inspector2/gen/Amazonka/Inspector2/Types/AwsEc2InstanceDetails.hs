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
-- Module      : Amazonka.Inspector2.Types.AwsEc2InstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AwsEc2InstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the Amazon EC2 instance involved in a finding.
--
-- /See:/ 'newAwsEc2InstanceDetails' smart constructor.
data AwsEc2InstanceDetails = AwsEc2InstanceDetails'
  { -- | The IAM instance profile ARN of the Amazon EC2 instance.
    iamInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The image ID of the Amazon EC2 instance.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 addresses of the Amazon EC2 instance.
    ipV4Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The IPv6 addresses of the Amazon EC2 instance.
    ipV6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The name of the key pair used to launch the Amazon EC2 instance.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Amazon EC2 instance was launched at.
    launchedAt :: Prelude.Maybe Data.POSIX,
    -- | The platform of the Amazon EC2 instance.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The subnet ID of the Amazon EC2 instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The type of the Amazon EC2 instance.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The VPC ID of the Amazon EC2 instance.
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
-- 'iamInstanceProfileArn', 'awsEc2InstanceDetails_iamInstanceProfileArn' - The IAM instance profile ARN of the Amazon EC2 instance.
--
-- 'imageId', 'awsEc2InstanceDetails_imageId' - The image ID of the Amazon EC2 instance.
--
-- 'ipV4Addresses', 'awsEc2InstanceDetails_ipV4Addresses' - The IPv4 addresses of the Amazon EC2 instance.
--
-- 'ipV6Addresses', 'awsEc2InstanceDetails_ipV6Addresses' - The IPv6 addresses of the Amazon EC2 instance.
--
-- 'keyName', 'awsEc2InstanceDetails_keyName' - The name of the key pair used to launch the Amazon EC2 instance.
--
-- 'launchedAt', 'awsEc2InstanceDetails_launchedAt' - The date and time the Amazon EC2 instance was launched at.
--
-- 'platform', 'awsEc2InstanceDetails_platform' - The platform of the Amazon EC2 instance.
--
-- 'subnetId', 'awsEc2InstanceDetails_subnetId' - The subnet ID of the Amazon EC2 instance.
--
-- 'type'', 'awsEc2InstanceDetails_type' - The type of the Amazon EC2 instance.
--
-- 'vpcId', 'awsEc2InstanceDetails_vpcId' - The VPC ID of the Amazon EC2 instance.
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
      platform = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The IAM instance profile ARN of the Amazon EC2 instance.
awsEc2InstanceDetails_iamInstanceProfileArn :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_iamInstanceProfileArn = Lens.lens (\AwsEc2InstanceDetails' {iamInstanceProfileArn} -> iamInstanceProfileArn) (\s@AwsEc2InstanceDetails' {} a -> s {iamInstanceProfileArn = a} :: AwsEc2InstanceDetails)

-- | The image ID of the Amazon EC2 instance.
awsEc2InstanceDetails_imageId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_imageId = Lens.lens (\AwsEc2InstanceDetails' {imageId} -> imageId) (\s@AwsEc2InstanceDetails' {} a -> s {imageId = a} :: AwsEc2InstanceDetails)

-- | The IPv4 addresses of the Amazon EC2 instance.
awsEc2InstanceDetails_ipV4Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV4Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV4Addresses} -> ipV4Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV4Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 addresses of the Amazon EC2 instance.
awsEc2InstanceDetails_ipV6Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV6Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV6Addresses} -> ipV6Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV6Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the key pair used to launch the Amazon EC2 instance.
awsEc2InstanceDetails_keyName :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_keyName = Lens.lens (\AwsEc2InstanceDetails' {keyName} -> keyName) (\s@AwsEc2InstanceDetails' {} a -> s {keyName = a} :: AwsEc2InstanceDetails)

-- | The date and time the Amazon EC2 instance was launched at.
awsEc2InstanceDetails_launchedAt :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.UTCTime)
awsEc2InstanceDetails_launchedAt = Lens.lens (\AwsEc2InstanceDetails' {launchedAt} -> launchedAt) (\s@AwsEc2InstanceDetails' {} a -> s {launchedAt = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Data._Time

-- | The platform of the Amazon EC2 instance.
awsEc2InstanceDetails_platform :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_platform = Lens.lens (\AwsEc2InstanceDetails' {platform} -> platform) (\s@AwsEc2InstanceDetails' {} a -> s {platform = a} :: AwsEc2InstanceDetails)

-- | The subnet ID of the Amazon EC2 instance.
awsEc2InstanceDetails_subnetId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_subnetId = Lens.lens (\AwsEc2InstanceDetails' {subnetId} -> subnetId) (\s@AwsEc2InstanceDetails' {} a -> s {subnetId = a} :: AwsEc2InstanceDetails)

-- | The type of the Amazon EC2 instance.
awsEc2InstanceDetails_type :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_type = Lens.lens (\AwsEc2InstanceDetails' {type'} -> type') (\s@AwsEc2InstanceDetails' {} a -> s {type' = a} :: AwsEc2InstanceDetails)

-- | The VPC ID of the Amazon EC2 instance.
awsEc2InstanceDetails_vpcId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_vpcId = Lens.lens (\AwsEc2InstanceDetails' {vpcId} -> vpcId) (\s@AwsEc2InstanceDetails' {} a -> s {vpcId = a} :: AwsEc2InstanceDetails)

instance Data.FromJSON AwsEc2InstanceDetails where
  parseJSON =
    Data.withObject
      "AwsEc2InstanceDetails"
      ( \x ->
          AwsEc2InstanceDetails'
            Prelude.<$> (x Data..:? "iamInstanceProfileArn")
            Prelude.<*> (x Data..:? "imageId")
            Prelude.<*> (x Data..:? "ipV4Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ipV6Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "keyName")
            Prelude.<*> (x Data..:? "launchedAt")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "subnetId")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable AwsEc2InstanceDetails where
  hashWithSalt _salt AwsEc2InstanceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` iamInstanceProfileArn
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` ipV4Addresses
      `Prelude.hashWithSalt` ipV6Addresses
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` launchedAt
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsEc2InstanceDetails where
  rnf AwsEc2InstanceDetails' {..} =
    Prelude.rnf iamInstanceProfileArn
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf ipV4Addresses
      `Prelude.seq` Prelude.rnf ipV6Addresses
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf launchedAt
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf vpcId

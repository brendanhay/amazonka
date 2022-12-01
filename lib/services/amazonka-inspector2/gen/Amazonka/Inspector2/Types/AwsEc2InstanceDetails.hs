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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AwsEc2InstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the Amazon EC2 instance involved in a finding.
--
-- /See:/ 'newAwsEc2InstanceDetails' smart constructor.
data AwsEc2InstanceDetails = AwsEc2InstanceDetails'
  { -- | The type of the Amazon EC2 instance.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 addresses of the Amazon EC2 instance.
    ipV4Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The subnet ID of the Amazon EC2 instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The IAM instance profile ARN of the Amazon EC2 instance.
    iamInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The platform of the Amazon EC2 instance.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair used to launch the Amazon EC2 instance.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Amazon EC2 instance was launched at.
    launchedAt :: Prelude.Maybe Core.POSIX,
    -- | The VPC ID of the Amazon EC2 instance.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 addresses of the Amazon EC2 instance.
    ipV6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The image ID of the Amazon EC2 instance.
    imageId :: Prelude.Maybe Prelude.Text
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
-- 'type'', 'awsEc2InstanceDetails_type' - The type of the Amazon EC2 instance.
--
-- 'ipV4Addresses', 'awsEc2InstanceDetails_ipV4Addresses' - The IPv4 addresses of the Amazon EC2 instance.
--
-- 'subnetId', 'awsEc2InstanceDetails_subnetId' - The subnet ID of the Amazon EC2 instance.
--
-- 'iamInstanceProfileArn', 'awsEc2InstanceDetails_iamInstanceProfileArn' - The IAM instance profile ARN of the Amazon EC2 instance.
--
-- 'platform', 'awsEc2InstanceDetails_platform' - The platform of the Amazon EC2 instance.
--
-- 'keyName', 'awsEc2InstanceDetails_keyName' - The name of the key pair used to launch the Amazon EC2 instance.
--
-- 'launchedAt', 'awsEc2InstanceDetails_launchedAt' - The date and time the Amazon EC2 instance was launched at.
--
-- 'vpcId', 'awsEc2InstanceDetails_vpcId' - The VPC ID of the Amazon EC2 instance.
--
-- 'ipV6Addresses', 'awsEc2InstanceDetails_ipV6Addresses' - The IPv6 addresses of the Amazon EC2 instance.
--
-- 'imageId', 'awsEc2InstanceDetails_imageId' - The image ID of the Amazon EC2 instance.
newAwsEc2InstanceDetails ::
  AwsEc2InstanceDetails
newAwsEc2InstanceDetails =
  AwsEc2InstanceDetails'
    { type' = Prelude.Nothing,
      ipV4Addresses = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      iamInstanceProfileArn = Prelude.Nothing,
      platform = Prelude.Nothing,
      keyName = Prelude.Nothing,
      launchedAt = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      ipV6Addresses = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | The type of the Amazon EC2 instance.
awsEc2InstanceDetails_type :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_type = Lens.lens (\AwsEc2InstanceDetails' {type'} -> type') (\s@AwsEc2InstanceDetails' {} a -> s {type' = a} :: AwsEc2InstanceDetails)

-- | The IPv4 addresses of the Amazon EC2 instance.
awsEc2InstanceDetails_ipV4Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV4Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV4Addresses} -> ipV4Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV4Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The subnet ID of the Amazon EC2 instance.
awsEc2InstanceDetails_subnetId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_subnetId = Lens.lens (\AwsEc2InstanceDetails' {subnetId} -> subnetId) (\s@AwsEc2InstanceDetails' {} a -> s {subnetId = a} :: AwsEc2InstanceDetails)

-- | The IAM instance profile ARN of the Amazon EC2 instance.
awsEc2InstanceDetails_iamInstanceProfileArn :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_iamInstanceProfileArn = Lens.lens (\AwsEc2InstanceDetails' {iamInstanceProfileArn} -> iamInstanceProfileArn) (\s@AwsEc2InstanceDetails' {} a -> s {iamInstanceProfileArn = a} :: AwsEc2InstanceDetails)

-- | The platform of the Amazon EC2 instance.
awsEc2InstanceDetails_platform :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_platform = Lens.lens (\AwsEc2InstanceDetails' {platform} -> platform) (\s@AwsEc2InstanceDetails' {} a -> s {platform = a} :: AwsEc2InstanceDetails)

-- | The name of the key pair used to launch the Amazon EC2 instance.
awsEc2InstanceDetails_keyName :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_keyName = Lens.lens (\AwsEc2InstanceDetails' {keyName} -> keyName) (\s@AwsEc2InstanceDetails' {} a -> s {keyName = a} :: AwsEc2InstanceDetails)

-- | The date and time the Amazon EC2 instance was launched at.
awsEc2InstanceDetails_launchedAt :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.UTCTime)
awsEc2InstanceDetails_launchedAt = Lens.lens (\AwsEc2InstanceDetails' {launchedAt} -> launchedAt) (\s@AwsEc2InstanceDetails' {} a -> s {launchedAt = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Core._Time

-- | The VPC ID of the Amazon EC2 instance.
awsEc2InstanceDetails_vpcId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_vpcId = Lens.lens (\AwsEc2InstanceDetails' {vpcId} -> vpcId) (\s@AwsEc2InstanceDetails' {} a -> s {vpcId = a} :: AwsEc2InstanceDetails)

-- | The IPv6 addresses of the Amazon EC2 instance.
awsEc2InstanceDetails_ipV6Addresses :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe [Prelude.Text])
awsEc2InstanceDetails_ipV6Addresses = Lens.lens (\AwsEc2InstanceDetails' {ipV6Addresses} -> ipV6Addresses) (\s@AwsEc2InstanceDetails' {} a -> s {ipV6Addresses = a} :: AwsEc2InstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The image ID of the Amazon EC2 instance.
awsEc2InstanceDetails_imageId :: Lens.Lens' AwsEc2InstanceDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceDetails_imageId = Lens.lens (\AwsEc2InstanceDetails' {imageId} -> imageId) (\s@AwsEc2InstanceDetails' {} a -> s {imageId = a} :: AwsEc2InstanceDetails)

instance Core.FromJSON AwsEc2InstanceDetails where
  parseJSON =
    Core.withObject
      "AwsEc2InstanceDetails"
      ( \x ->
          AwsEc2InstanceDetails'
            Prelude.<$> (x Core..:? "type")
            Prelude.<*> (x Core..:? "ipV4Addresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subnetId")
            Prelude.<*> (x Core..:? "iamInstanceProfileArn")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "keyName")
            Prelude.<*> (x Core..:? "launchedAt")
            Prelude.<*> (x Core..:? "vpcId")
            Prelude.<*> (x Core..:? "ipV6Addresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "imageId")
      )

instance Prelude.Hashable AwsEc2InstanceDetails where
  hashWithSalt _salt AwsEc2InstanceDetails' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` ipV4Addresses
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` iamInstanceProfileArn
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` launchedAt
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` ipV6Addresses
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData AwsEc2InstanceDetails where
  rnf AwsEc2InstanceDetails' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf ipV4Addresses
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf iamInstanceProfileArn
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf launchedAt
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf ipV6Addresses
      `Prelude.seq` Prelude.rnf imageId

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
-- Module      : Network.AWS.GuardDuty.Types.InstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.InstanceDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.IamInstanceProfile
import Network.AWS.GuardDuty.Types.NetworkInterface
import Network.AWS.GuardDuty.Types.ProductCode
import Network.AWS.GuardDuty.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Contains information about the details of an instance.
--
-- /See:/ 'newInstanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { -- | The platform of the EC2 instance.
    platform :: Core.Maybe Core.Text,
    -- | The ID of the EC2 instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The type of the EC2 instance.
    instanceType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Outpost. Only applicable to
    -- AWS Outposts instances.
    outpostArn :: Core.Maybe Core.Text,
    -- | The launch time of the EC2 instance.
    launchTime :: Core.Maybe Core.Text,
    -- | The product code of the EC2 instance.
    productCodes :: Core.Maybe [ProductCode],
    -- | The image ID of the EC2 instance.
    imageId :: Core.Maybe Core.Text,
    -- | The profile information of the EC2 instance.
    iamInstanceProfile :: Core.Maybe IamInstanceProfile,
    -- | The Availability Zone of the EC2 instance.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The tags of the EC2 instance.
    tags :: Core.Maybe [Tag],
    -- | The image description of the EC2 instance.
    imageDescription :: Core.Maybe Core.Text,
    -- | The state of the EC2 instance.
    instanceState :: Core.Maybe Core.Text,
    -- | The elastic network interface information of the EC2 instance.
    networkInterfaces :: Core.Maybe [NetworkInterface]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'instanceDetails_platform' - The platform of the EC2 instance.
--
-- 'instanceId', 'instanceDetails_instanceId' - The ID of the EC2 instance.
--
-- 'instanceType', 'instanceDetails_instanceType' - The type of the EC2 instance.
--
-- 'outpostArn', 'instanceDetails_outpostArn' - The Amazon Resource Name (ARN) of the AWS Outpost. Only applicable to
-- AWS Outposts instances.
--
-- 'launchTime', 'instanceDetails_launchTime' - The launch time of the EC2 instance.
--
-- 'productCodes', 'instanceDetails_productCodes' - The product code of the EC2 instance.
--
-- 'imageId', 'instanceDetails_imageId' - The image ID of the EC2 instance.
--
-- 'iamInstanceProfile', 'instanceDetails_iamInstanceProfile' - The profile information of the EC2 instance.
--
-- 'availabilityZone', 'instanceDetails_availabilityZone' - The Availability Zone of the EC2 instance.
--
-- 'tags', 'instanceDetails_tags' - The tags of the EC2 instance.
--
-- 'imageDescription', 'instanceDetails_imageDescription' - The image description of the EC2 instance.
--
-- 'instanceState', 'instanceDetails_instanceState' - The state of the EC2 instance.
--
-- 'networkInterfaces', 'instanceDetails_networkInterfaces' - The elastic network interface information of the EC2 instance.
newInstanceDetails ::
  InstanceDetails
newInstanceDetails =
  InstanceDetails'
    { platform = Core.Nothing,
      instanceId = Core.Nothing,
      instanceType = Core.Nothing,
      outpostArn = Core.Nothing,
      launchTime = Core.Nothing,
      productCodes = Core.Nothing,
      imageId = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      availabilityZone = Core.Nothing,
      tags = Core.Nothing,
      imageDescription = Core.Nothing,
      instanceState = Core.Nothing,
      networkInterfaces = Core.Nothing
    }

-- | The platform of the EC2 instance.
instanceDetails_platform :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_platform = Lens.lens (\InstanceDetails' {platform} -> platform) (\s@InstanceDetails' {} a -> s {platform = a} :: InstanceDetails)

-- | The ID of the EC2 instance.
instanceDetails_instanceId :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_instanceId = Lens.lens (\InstanceDetails' {instanceId} -> instanceId) (\s@InstanceDetails' {} a -> s {instanceId = a} :: InstanceDetails)

-- | The type of the EC2 instance.
instanceDetails_instanceType :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_instanceType = Lens.lens (\InstanceDetails' {instanceType} -> instanceType) (\s@InstanceDetails' {} a -> s {instanceType = a} :: InstanceDetails)

-- | The Amazon Resource Name (ARN) of the AWS Outpost. Only applicable to
-- AWS Outposts instances.
instanceDetails_outpostArn :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_outpostArn = Lens.lens (\InstanceDetails' {outpostArn} -> outpostArn) (\s@InstanceDetails' {} a -> s {outpostArn = a} :: InstanceDetails)

-- | The launch time of the EC2 instance.
instanceDetails_launchTime :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_launchTime = Lens.lens (\InstanceDetails' {launchTime} -> launchTime) (\s@InstanceDetails' {} a -> s {launchTime = a} :: InstanceDetails)

-- | The product code of the EC2 instance.
instanceDetails_productCodes :: Lens.Lens' InstanceDetails (Core.Maybe [ProductCode])
instanceDetails_productCodes = Lens.lens (\InstanceDetails' {productCodes} -> productCodes) (\s@InstanceDetails' {} a -> s {productCodes = a} :: InstanceDetails) Core.. Lens.mapping Lens._Coerce

-- | The image ID of the EC2 instance.
instanceDetails_imageId :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_imageId = Lens.lens (\InstanceDetails' {imageId} -> imageId) (\s@InstanceDetails' {} a -> s {imageId = a} :: InstanceDetails)

-- | The profile information of the EC2 instance.
instanceDetails_iamInstanceProfile :: Lens.Lens' InstanceDetails (Core.Maybe IamInstanceProfile)
instanceDetails_iamInstanceProfile = Lens.lens (\InstanceDetails' {iamInstanceProfile} -> iamInstanceProfile) (\s@InstanceDetails' {} a -> s {iamInstanceProfile = a} :: InstanceDetails)

-- | The Availability Zone of the EC2 instance.
instanceDetails_availabilityZone :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_availabilityZone = Lens.lens (\InstanceDetails' {availabilityZone} -> availabilityZone) (\s@InstanceDetails' {} a -> s {availabilityZone = a} :: InstanceDetails)

-- | The tags of the EC2 instance.
instanceDetails_tags :: Lens.Lens' InstanceDetails (Core.Maybe [Tag])
instanceDetails_tags = Lens.lens (\InstanceDetails' {tags} -> tags) (\s@InstanceDetails' {} a -> s {tags = a} :: InstanceDetails) Core.. Lens.mapping Lens._Coerce

-- | The image description of the EC2 instance.
instanceDetails_imageDescription :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_imageDescription = Lens.lens (\InstanceDetails' {imageDescription} -> imageDescription) (\s@InstanceDetails' {} a -> s {imageDescription = a} :: InstanceDetails)

-- | The state of the EC2 instance.
instanceDetails_instanceState :: Lens.Lens' InstanceDetails (Core.Maybe Core.Text)
instanceDetails_instanceState = Lens.lens (\InstanceDetails' {instanceState} -> instanceState) (\s@InstanceDetails' {} a -> s {instanceState = a} :: InstanceDetails)

-- | The elastic network interface information of the EC2 instance.
instanceDetails_networkInterfaces :: Lens.Lens' InstanceDetails (Core.Maybe [NetworkInterface])
instanceDetails_networkInterfaces = Lens.lens (\InstanceDetails' {networkInterfaces} -> networkInterfaces) (\s@InstanceDetails' {} a -> s {networkInterfaces = a} :: InstanceDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON InstanceDetails where
  parseJSON =
    Core.withObject
      "InstanceDetails"
      ( \x ->
          InstanceDetails'
            Core.<$> (x Core..:? "platform")
            Core.<*> (x Core..:? "instanceId")
            Core.<*> (x Core..:? "instanceType")
            Core.<*> (x Core..:? "outpostArn")
            Core.<*> (x Core..:? "launchTime")
            Core.<*> (x Core..:? "productCodes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "imageId")
            Core.<*> (x Core..:? "iamInstanceProfile")
            Core.<*> (x Core..:? "availabilityZone")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "imageDescription")
            Core.<*> (x Core..:? "instanceState")
            Core.<*> ( x Core..:? "networkInterfaces"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable InstanceDetails

instance Core.NFData InstanceDetails

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.InstanceDetails
  ( InstanceDetails (..),

    -- * Smart constructor
    mkInstanceDetails,

    -- * Lenses
    idInstanceId,
    idPlatform,
    idLaunchTime,
    idNetworkInterfaces,
    idOutpostARN,
    idInstanceType,
    idAvailabilityZone,
    idIAMInstanceProfile,
    idImageId,
    idProductCodes,
    idInstanceState,
    idTags,
    idImageDescription,
  )
where

import Network.AWS.GuardDuty.Types.IAMInstanceProfile
import Network.AWS.GuardDuty.Types.NetworkInterface
import Network.AWS.GuardDuty.Types.ProductCode
import Network.AWS.GuardDuty.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the details of an instance.
--
-- /See:/ 'mkInstanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { instanceId ::
      Lude.Maybe Lude.Text,
    platform :: Lude.Maybe Lude.Text,
    launchTime :: Lude.Maybe Lude.Text,
    networkInterfaces :: Lude.Maybe [NetworkInterface],
    outpostARN :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    iamInstanceProfile :: Lude.Maybe IAMInstanceProfile,
    imageId :: Lude.Maybe Lude.Text,
    productCodes :: Lude.Maybe [ProductCode],
    instanceState :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    imageDescription :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceDetails' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone of the EC2 instance.
-- * 'iamInstanceProfile' - The profile information of the EC2 instance.
-- * 'imageDescription' - The image description of the EC2 instance.
-- * 'imageId' - The image ID of the EC2 instance.
-- * 'instanceId' - The ID of the EC2 instance.
-- * 'instanceState' - The state of the EC2 instance.
-- * 'instanceType' - The type of the EC2 instance.
-- * 'launchTime' - The launch time of the EC2 instance.
-- * 'networkInterfaces' - The elastic network interface information of the EC2 instance.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the AWS Outpost. Only applicable to AWS Outposts instances.
-- * 'platform' - The platform of the EC2 instance.
-- * 'productCodes' - The product code of the EC2 instance.
-- * 'tags' - The tags of the EC2 instance.
mkInstanceDetails ::
  InstanceDetails
mkInstanceDetails =
  InstanceDetails'
    { instanceId = Lude.Nothing,
      platform = Lude.Nothing,
      launchTime = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      outpostARN = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      imageId = Lude.Nothing,
      productCodes = Lude.Nothing,
      instanceState = Lude.Nothing,
      tags = Lude.Nothing,
      imageDescription = Lude.Nothing
    }

-- | The ID of the EC2 instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceId :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idInstanceId = Lens.lens (instanceId :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceDetails)
{-# DEPRECATED idInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The platform of the EC2 instance.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idPlatform :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idPlatform = Lens.lens (platform :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: InstanceDetails)
{-# DEPRECATED idPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The launch time of the EC2 instance.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idLaunchTime :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idLaunchTime = Lens.lens (launchTime :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {launchTime = a} :: InstanceDetails)
{-# DEPRECATED idLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The elastic network interface information of the EC2 instance.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idNetworkInterfaces :: Lens.Lens' InstanceDetails (Lude.Maybe [NetworkInterface])
idNetworkInterfaces = Lens.lens (networkInterfaces :: InstanceDetails -> Lude.Maybe [NetworkInterface]) (\s a -> s {networkInterfaces = a} :: InstanceDetails)
{-# DEPRECATED idNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Outpost. Only applicable to AWS Outposts instances.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idOutpostARN :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idOutpostARN = Lens.lens (outpostARN :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: InstanceDetails)
{-# DEPRECATED idOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | The type of the EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceType :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idInstanceType = Lens.lens (instanceType :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: InstanceDetails)
{-# DEPRECATED idInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone of the EC2 instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idAvailabilityZone :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idAvailabilityZone = Lens.lens (availabilityZone :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: InstanceDetails)
{-# DEPRECATED idAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The profile information of the EC2 instance.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idIAMInstanceProfile :: Lens.Lens' InstanceDetails (Lude.Maybe IAMInstanceProfile)
idIAMInstanceProfile = Lens.lens (iamInstanceProfile :: InstanceDetails -> Lude.Maybe IAMInstanceProfile) (\s a -> s {iamInstanceProfile = a} :: InstanceDetails)
{-# DEPRECATED idIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The image ID of the EC2 instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageId :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idImageId = Lens.lens (imageId :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: InstanceDetails)
{-# DEPRECATED idImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The product code of the EC2 instance.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idProductCodes :: Lens.Lens' InstanceDetails (Lude.Maybe [ProductCode])
idProductCodes = Lens.lens (productCodes :: InstanceDetails -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: InstanceDetails)
{-# DEPRECATED idProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The state of the EC2 instance.
--
-- /Note:/ Consider using 'instanceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceState :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idInstanceState = Lens.lens (instanceState :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceState = a} :: InstanceDetails)
{-# DEPRECATED idInstanceState "Use generic-lens or generic-optics with 'instanceState' instead." #-}

-- | The tags of the EC2 instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idTags :: Lens.Lens' InstanceDetails (Lude.Maybe [Tag])
idTags = Lens.lens (tags :: InstanceDetails -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: InstanceDetails)
{-# DEPRECATED idTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The image description of the EC2 instance.
--
-- /Note:/ Consider using 'imageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageDescription :: Lens.Lens' InstanceDetails (Lude.Maybe Lude.Text)
idImageDescription = Lens.lens (imageDescription :: InstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {imageDescription = a} :: InstanceDetails)
{-# DEPRECATED idImageDescription "Use generic-lens or generic-optics with 'imageDescription' instead." #-}

instance Lude.FromJSON InstanceDetails where
  parseJSON =
    Lude.withObject
      "InstanceDetails"
      ( \x ->
          InstanceDetails'
            Lude.<$> (x Lude..:? "instanceId")
            Lude.<*> (x Lude..:? "platform")
            Lude.<*> (x Lude..:? "launchTime")
            Lude.<*> (x Lude..:? "networkInterfaces" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "outpostArn")
            Lude.<*> (x Lude..:? "instanceType")
            Lude.<*> (x Lude..:? "availabilityZone")
            Lude.<*> (x Lude..:? "iamInstanceProfile")
            Lude.<*> (x Lude..:? "imageId")
            Lude.<*> (x Lude..:? "productCodes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "instanceState")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "imageDescription")
      )

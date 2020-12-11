-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterMetadata
  ( ClusterMetadata (..),

    -- * Smart constructor
    mkClusterMetadata,

    -- * Lenses
    cmJobType,
    cmKMSKeyARN,
    cmClusterState,
    cmNotification,
    cmForwardingAddressId,
    cmAddressId,
    cmSnowballType,
    cmShippingOption,
    cmResources,
    cmClusterId,
    cmCreationDate,
    cmDescription,
    cmTaxDocuments,
    cmRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.ClusterState
import Network.AWS.Snowball.Types.JobResource
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.ShippingOption
import Network.AWS.Snowball.Types.SnowballType
import Network.AWS.Snowball.Types.TaxDocuments

-- | Contains metadata about a specific cluster.
--
-- /See:/ 'mkClusterMetadata' smart constructor.
data ClusterMetadata = ClusterMetadata'
  { jobType ::
      Lude.Maybe JobType,
    kmsKeyARN :: Lude.Maybe Lude.Text,
    clusterState :: Lude.Maybe ClusterState,
    notification :: Lude.Maybe Notification,
    forwardingAddressId :: Lude.Maybe Lude.Text,
    addressId :: Lude.Maybe Lude.Text,
    snowballType :: Lude.Maybe SnowballType,
    shippingOption :: Lude.Maybe ShippingOption,
    resources :: Lude.Maybe JobResource,
    clusterId :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    taxDocuments :: Lude.Maybe TaxDocuments,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterMetadata' with the minimum fields required to make a request.
--
-- * 'addressId' - The automatically generated ID for a specific address.
-- * 'clusterId' - The automatically generated ID for a cluster.
-- * 'clusterState' - The current status of the cluster.
-- * 'creationDate' - The creation date for this cluster.
-- * 'description' - The optional description of the cluster.
-- * 'forwardingAddressId' - The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
-- * 'jobType' - The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
-- * 'kmsKeyARN' - The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
-- * 'notification' - The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
-- * 'resources' - The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
-- * 'roleARN' - The role ARN associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
-- * 'shippingOption' - The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow devices are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
-- * 'snowballType' - The type of AWS Snow device to use for this cluster.
-- * 'taxDocuments' - The tax documents required in your AWS Region.
mkClusterMetadata ::
  ClusterMetadata
mkClusterMetadata =
  ClusterMetadata'
    { jobType = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      clusterState = Lude.Nothing,
      notification = Lude.Nothing,
      forwardingAddressId = Lude.Nothing,
      addressId = Lude.Nothing,
      snowballType = Lude.Nothing,
      shippingOption = Lude.Nothing,
      resources = Lude.Nothing,
      clusterId = Lude.Nothing,
      creationDate = Lude.Nothing,
      description = Lude.Nothing,
      taxDocuments = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmJobType :: Lens.Lens' ClusterMetadata (Lude.Maybe JobType)
cmJobType = Lens.lens (jobType :: ClusterMetadata -> Lude.Maybe JobType) (\s a -> s {jobType = a} :: ClusterMetadata)
{-# DEPRECATED cmJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmKMSKeyARN :: Lens.Lens' ClusterMetadata (Lude.Maybe Lude.Text)
cmKMSKeyARN = Lens.lens (kmsKeyARN :: ClusterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: ClusterMetadata)
{-# DEPRECATED cmKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The current status of the cluster.
--
-- /Note:/ Consider using 'clusterState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmClusterState :: Lens.Lens' ClusterMetadata (Lude.Maybe ClusterState)
cmClusterState = Lens.lens (clusterState :: ClusterMetadata -> Lude.Maybe ClusterState) (\s a -> s {clusterState = a} :: ClusterMetadata)
{-# DEPRECATED cmClusterState "Use generic-lens or generic-optics with 'clusterState' instead." #-}

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNotification :: Lens.Lens' ClusterMetadata (Lude.Maybe Notification)
cmNotification = Lens.lens (notification :: ClusterMetadata -> Lude.Maybe Notification) (\s a -> s {notification = a} :: ClusterMetadata)
{-# DEPRECATED cmNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmForwardingAddressId :: Lens.Lens' ClusterMetadata (Lude.Maybe Lude.Text)
cmForwardingAddressId = Lens.lens (forwardingAddressId :: ClusterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {forwardingAddressId = a} :: ClusterMetadata)
{-# DEPRECATED cmForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The automatically generated ID for a specific address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmAddressId :: Lens.Lens' ClusterMetadata (Lude.Maybe Lude.Text)
cmAddressId = Lens.lens (addressId :: ClusterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {addressId = a} :: ClusterMetadata)
{-# DEPRECATED cmAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The type of AWS Snow device to use for this cluster.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmSnowballType :: Lens.Lens' ClusterMetadata (Lude.Maybe SnowballType)
cmSnowballType = Lens.lens (snowballType :: ClusterMetadata -> Lude.Maybe SnowballType) (\s a -> s {snowballType = a} :: ClusterMetadata)
{-# DEPRECATED cmSnowballType "Use generic-lens or generic-optics with 'snowballType' instead." #-}

-- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow devices are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmShippingOption :: Lens.Lens' ClusterMetadata (Lude.Maybe ShippingOption)
cmShippingOption = Lens.lens (shippingOption :: ClusterMetadata -> Lude.Maybe ShippingOption) (\s a -> s {shippingOption = a} :: ClusterMetadata)
{-# DEPRECATED cmShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmResources :: Lens.Lens' ClusterMetadata (Lude.Maybe JobResource)
cmResources = Lens.lens (resources :: ClusterMetadata -> Lude.Maybe JobResource) (\s a -> s {resources = a} :: ClusterMetadata)
{-# DEPRECATED cmResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The automatically generated ID for a cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmClusterId :: Lens.Lens' ClusterMetadata (Lude.Maybe Lude.Text)
cmClusterId = Lens.lens (clusterId :: ClusterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: ClusterMetadata)
{-# DEPRECATED cmClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The creation date for this cluster.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCreationDate :: Lens.Lens' ClusterMetadata (Lude.Maybe Lude.Timestamp)
cmCreationDate = Lens.lens (creationDate :: ClusterMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ClusterMetadata)
{-# DEPRECATED cmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The optional description of the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmDescription :: Lens.Lens' ClusterMetadata (Lude.Maybe Lude.Text)
cmDescription = Lens.lens (description :: ClusterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClusterMetadata)
{-# DEPRECATED cmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTaxDocuments :: Lens.Lens' ClusterMetadata (Lude.Maybe TaxDocuments)
cmTaxDocuments = Lens.lens (taxDocuments :: ClusterMetadata -> Lude.Maybe TaxDocuments) (\s a -> s {taxDocuments = a} :: ClusterMetadata)
{-# DEPRECATED cmTaxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead." #-}

-- | The role ARN associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRoleARN :: Lens.Lens' ClusterMetadata (Lude.Maybe Lude.Text)
cmRoleARN = Lens.lens (roleARN :: ClusterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ClusterMetadata)
{-# DEPRECATED cmRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ClusterMetadata where
  parseJSON =
    Lude.withObject
      "ClusterMetadata"
      ( \x ->
          ClusterMetadata'
            Lude.<$> (x Lude..:? "JobType")
            Lude.<*> (x Lude..:? "KmsKeyARN")
            Lude.<*> (x Lude..:? "ClusterState")
            Lude.<*> (x Lude..:? "Notification")
            Lude.<*> (x Lude..:? "ForwardingAddressId")
            Lude.<*> (x Lude..:? "AddressId")
            Lude.<*> (x Lude..:? "SnowballType")
            Lude.<*> (x Lude..:? "ShippingOption")
            Lude.<*> (x Lude..:? "Resources")
            Lude.<*> (x Lude..:? "ClusterId")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "TaxDocuments")
            Lude.<*> (x Lude..:? "RoleARN")
      )

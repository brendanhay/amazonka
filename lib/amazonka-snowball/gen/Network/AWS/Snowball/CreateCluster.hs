{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty cluster. Each cluster supports five nodes. You use the 'CreateJob' action separately to create the jobs for each of these nodes. The cluster does not ship until these five node jobs have been created.
module Network.AWS.Snowball.CreateCluster
  ( -- * Creating a request
    CreateCluster (..),
    mkCreateCluster,

    -- ** Request lenses
    ccJobType,
    ccKMSKeyARN,
    ccNotification,
    ccForwardingAddressId,
    ccAddressId,
    ccSnowballType,
    ccShippingOption,
    ccResources,
    ccDescription,
    ccTaxDocuments,
    ccRoleARN,

    -- * Destructuring the response
    CreateClusterResponse (..),
    mkCreateClusterResponse,

    -- ** Response lenses
    crsClusterId,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
    jobType :: JobType,
    -- | The @KmsKeyARN@ value that you want to associate with this cluster. @KmsKeyARN@ values are created by using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
    kmsKeyARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
    notification :: Lude.Maybe Notification,
    -- | The forwarding address ID for a cluster. This field is not supported in most regions.
    forwardingAddressId :: Lude.Maybe Lude.Text,
    -- | The ID for the address that you want the cluster shipped to.
    addressId :: Lude.Text,
    -- | The type of AWS Snow Family device to use for this cluster.
    snowballType :: Lude.Maybe SnowballType,
    -- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:
    --
    --
    --     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
    --
    --
    --     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
    --
    --
    --     * In India, Snow device are delivered in one to seven days.
    --
    --
    --     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
    --
    --
    --
    --     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
    --
    --
    --     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
    --
    --
    --     * In India, Snow device are delivered in one to seven days.
    --
    --
    --     * In the US, you have access to one-day shipping and two-day shipping.
    shippingOption :: ShippingOption,
    -- | The resources associated with the cluster job. These resources include Amazon S3 buckets and optional AWS Lambda functions written in the Python language.
    resources :: JobResource,
    -- | An optional description of this specific cluster, for example @Environmental Data Cluster-01@ .
    description :: Lude.Maybe Lude.Text,
    -- | The tax documents required in your AWS Region.
    taxDocuments :: Lude.Maybe TaxDocuments,
    -- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@ values are created by using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- * 'jobType' - The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
-- * 'kmsKeyARN' - The @KmsKeyARN@ value that you want to associate with this cluster. @KmsKeyARN@ values are created by using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
-- * 'notification' - The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
-- * 'forwardingAddressId' - The forwarding address ID for a cluster. This field is not supported in most regions.
-- * 'addressId' - The ID for the address that you want the cluster shipped to.
-- * 'snowballType' - The type of AWS Snow Family device to use for this cluster.
-- * 'shippingOption' - The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
-- * 'resources' - The resources associated with the cluster job. These resources include Amazon S3 buckets and optional AWS Lambda functions written in the Python language.
-- * 'description' - An optional description of this specific cluster, for example @Environmental Data Cluster-01@ .
-- * 'taxDocuments' - The tax documents required in your AWS Region.
-- * 'roleARN' - The @RoleARN@ that you want to associate with this cluster. @RoleArn@ values are created by using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
mkCreateCluster ::
  -- | 'jobType'
  JobType ->
  -- | 'addressId'
  Lude.Text ->
  -- | 'shippingOption'
  ShippingOption ->
  -- | 'resources'
  JobResource ->
  -- | 'roleARN'
  Lude.Text ->
  CreateCluster
mkCreateCluster
  pJobType_
  pAddressId_
  pShippingOption_
  pResources_
  pRoleARN_ =
    CreateCluster'
      { jobType = pJobType_,
        kmsKeyARN = Lude.Nothing,
        notification = Lude.Nothing,
        forwardingAddressId = Lude.Nothing,
        addressId = pAddressId_,
        snowballType = Lude.Nothing,
        shippingOption = pShippingOption_,
        resources = pResources_,
        description = Lude.Nothing,
        taxDocuments = Lude.Nothing,
        roleARN = pRoleARN_
      }

-- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccJobType :: Lens.Lens' CreateCluster JobType
ccJobType = Lens.lens (jobType :: CreateCluster -> JobType) (\s a -> s {jobType = a} :: CreateCluster)
{-# DEPRECATED ccJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | The @KmsKeyARN@ value that you want to associate with this cluster. @KmsKeyARN@ values are created by using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKMSKeyARN :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccKMSKeyARN = Lens.lens (kmsKeyARN :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: CreateCluster)
{-# DEPRECATED ccKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotification :: Lens.Lens' CreateCluster (Lude.Maybe Notification)
ccNotification = Lens.lens (notification :: CreateCluster -> Lude.Maybe Notification) (\s a -> s {notification = a} :: CreateCluster)
{-# DEPRECATED ccNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The forwarding address ID for a cluster. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccForwardingAddressId :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccForwardingAddressId = Lens.lens (forwardingAddressId :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {forwardingAddressId = a} :: CreateCluster)
{-# DEPRECATED ccForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The ID for the address that you want the cluster shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAddressId :: Lens.Lens' CreateCluster Lude.Text
ccAddressId = Lens.lens (addressId :: CreateCluster -> Lude.Text) (\s a -> s {addressId = a} :: CreateCluster)
{-# DEPRECATED ccAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The type of AWS Snow Family device to use for this cluster.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnowballType :: Lens.Lens' CreateCluster (Lude.Maybe SnowballType)
ccSnowballType = Lens.lens (snowballType :: CreateCluster -> Lude.Maybe SnowballType) (\s a -> s {snowballType = a} :: CreateCluster)
{-# DEPRECATED ccSnowballType "Use generic-lens or generic-optics with 'snowballType' instead." #-}

-- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccShippingOption :: Lens.Lens' CreateCluster ShippingOption
ccShippingOption = Lens.lens (shippingOption :: CreateCluster -> ShippingOption) (\s a -> s {shippingOption = a} :: CreateCluster)
{-# DEPRECATED ccShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | The resources associated with the cluster job. These resources include Amazon S3 buckets and optional AWS Lambda functions written in the Python language.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccResources :: Lens.Lens' CreateCluster JobResource
ccResources = Lens.lens (resources :: CreateCluster -> JobResource) (\s a -> s {resources = a} :: CreateCluster)
{-# DEPRECATED ccResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | An optional description of this specific cluster, for example @Environmental Data Cluster-01@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccDescription = Lens.lens (description :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateCluster)
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTaxDocuments :: Lens.Lens' CreateCluster (Lude.Maybe TaxDocuments)
ccTaxDocuments = Lens.lens (taxDocuments :: CreateCluster -> Lude.Maybe TaxDocuments) (\s a -> s {taxDocuments = a} :: CreateCluster)
{-# DEPRECATED ccTaxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead." #-}

-- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@ values are created by using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRoleARN :: Lens.Lens' CreateCluster Lude.Text
ccRoleARN = Lens.lens (roleARN :: CreateCluster -> Lude.Text) (\s a -> s {roleARN = a} :: CreateCluster)
{-# DEPRECATED ccRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Lude.<$> (x Lude..?> "ClusterId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.CreateCluster" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobType" Lude..= jobType),
            ("KmsKeyARN" Lude..=) Lude.<$> kmsKeyARN,
            ("Notification" Lude..=) Lude.<$> notification,
            ("ForwardingAddressId" Lude..=) Lude.<$> forwardingAddressId,
            Lude.Just ("AddressId" Lude..= addressId),
            ("SnowballType" Lude..=) Lude.<$> snowballType,
            Lude.Just ("ShippingOption" Lude..= shippingOption),
            Lude.Just ("Resources" Lude..= resources),
            ("Description" Lude..=) Lude.<$> description,
            ("TaxDocuments" Lude..=) Lude.<$> taxDocuments,
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The automatically generated ID for a cluster.
    clusterId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- * 'clusterId' - The automatically generated ID for a cluster.
-- * 'responseStatus' - The response status code.
mkCreateClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterResponse
mkCreateClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    { clusterId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The automatically generated ID for a cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsClusterId :: Lens.Lens' CreateClusterResponse (Lude.Maybe Lude.Text)
crsClusterId = Lens.lens (clusterId :: CreateClusterResponse -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: CreateClusterResponse)
{-# DEPRECATED crsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateClusterResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

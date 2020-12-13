{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint.
module Network.AWS.SageMaker.DescribeEndpoint
  ( -- * Creating a request
    DescribeEndpoint (..),
    mkDescribeEndpoint,

    -- ** Request lenses
    dEndpointName,

    -- * Destructuring the response
    DescribeEndpointResponse (..),
    mkDescribeEndpointResponse,

    -- ** Response lenses
    degrsCreationTime,
    degrsFailureReason,
    degrsEndpointName,
    degrsProductionVariants,
    degrsLastModifiedTime,
    degrsEndpointStatus,
    degrsEndpointConfigName,
    degrsEndpointARN,
    degrsDataCaptureConfig,
    degrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { -- | The name of the endpoint.
    endpointName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name of the endpoint.
mkDescribeEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  DescribeEndpoint
mkDescribeEndpoint pEndpointName_ =
  DescribeEndpoint' {endpointName = pEndpointName_}

-- | The name of the endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndpointName :: Lens.Lens' DescribeEndpoint Lude.Text
dEndpointName = Lens.lens (endpointName :: DescribeEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: DescribeEndpoint)
{-# DEPRECATED dEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

instance Lude.AWSRequest DescribeEndpoint where
  type Rs DescribeEndpoint = DescribeEndpointResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..:> "EndpointName")
            Lude.<*> (x Lude..?> "ProductionVariants")
            Lude.<*> (x Lude..:> "LastModifiedTime")
            Lude.<*> (x Lude..:> "EndpointStatus")
            Lude.<*> (x Lude..:> "EndpointConfigName")
            Lude.<*> (x Lude..:> "EndpointArn")
            Lude.<*> (x Lude..?> "DataCaptureConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EndpointName" Lude..= endpointName)])

instance Lude.ToPath DescribeEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | A timestamp that shows when the endpoint was created.
    creationTime :: Lude.Timestamp,
    -- | If the status of the endpoint is @Failed@ , the reason why it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | Name of the endpoint.
    endpointName :: Lude.Text,
    -- | An array of 'ProductionVariantSummary' objects, one for each model hosted behind this endpoint.
    productionVariants :: Lude.Maybe (Lude.NonEmpty ProductionVariantSummary),
    -- | A timestamp that shows when the endpoint was last modified.
    lastModifiedTime :: Lude.Timestamp,
    -- | The status of the endpoint.
    --
    --
    --     * @OutOfService@ : Endpoint is not available to take incoming requests.
    --
    --
    --     * @Creating@ : 'CreateEndpoint' is executing.
    --
    --
    --     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.
    --
    --
    --     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.
    --
    --
    --     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.
    --
    --
    --     * @InService@ : Endpoint is available to process incoming requests.
    --
    --
    --     * @Deleting@ : 'DeleteEndpoint' is executing.
    --
    --
    --     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint.
    endpointStatus :: EndpointStatus,
    -- | The name of the endpoint configuration associated with this endpoint.
    endpointConfigName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointARN :: Lude.Text,
    dataCaptureConfig :: Lude.Maybe DataCaptureConfigSummary,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the endpoint was created.
-- * 'failureReason' - If the status of the endpoint is @Failed@ , the reason why it failed.
-- * 'endpointName' - Name of the endpoint.
-- * 'productionVariants' - An array of 'ProductionVariantSummary' objects, one for each model hosted behind this endpoint.
-- * 'lastModifiedTime' - A timestamp that shows when the endpoint was last modified.
-- * 'endpointStatus' - The status of the endpoint.
--
--
--     * @OutOfService@ : Endpoint is not available to take incoming requests.
--
--
--     * @Creating@ : 'CreateEndpoint' is executing.
--
--
--     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.
--
--
--     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.
--
--
--     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.
--
--
--     * @InService@ : Endpoint is available to process incoming requests.
--
--
--     * @Deleting@ : 'DeleteEndpoint' is executing.
--
--
--     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint.
--
--
-- * 'endpointConfigName' - The name of the endpoint configuration associated with this endpoint.
-- * 'endpointARN' - The Amazon Resource Name (ARN) of the endpoint.
-- * 'dataCaptureConfig' -
-- * 'responseStatus' - The response status code.
mkDescribeEndpointResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'endpointName'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'endpointStatus'
  EndpointStatus ->
  -- | 'endpointConfigName'
  Lude.Text ->
  -- | 'endpointARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointResponse
mkDescribeEndpointResponse
  pCreationTime_
  pEndpointName_
  pLastModifiedTime_
  pEndpointStatus_
  pEndpointConfigName_
  pEndpointARN_
  pResponseStatus_ =
    DescribeEndpointResponse'
      { creationTime = pCreationTime_,
        failureReason = Lude.Nothing,
        endpointName = pEndpointName_,
        productionVariants = Lude.Nothing,
        lastModifiedTime = pLastModifiedTime_,
        endpointStatus = pEndpointStatus_,
        endpointConfigName = pEndpointConfigName_,
        endpointARN = pEndpointARN_,
        dataCaptureConfig = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | A timestamp that shows when the endpoint was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsCreationTime :: Lens.Lens' DescribeEndpointResponse Lude.Timestamp
degrsCreationTime = Lens.lens (creationTime :: DescribeEndpointResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | If the status of the endpoint is @Failed@ , the reason why it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsFailureReason :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe Lude.Text)
degrsFailureReason = Lens.lens (failureReason :: DescribeEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Name of the endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsEndpointName :: Lens.Lens' DescribeEndpointResponse Lude.Text
degrsEndpointName = Lens.lens (endpointName :: DescribeEndpointResponse -> Lude.Text) (\s a -> s {endpointName = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | An array of 'ProductionVariantSummary' objects, one for each model hosted behind this endpoint.
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsProductionVariants :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe (Lude.NonEmpty ProductionVariantSummary))
degrsProductionVariants = Lens.lens (productionVariants :: DescribeEndpointResponse -> Lude.Maybe (Lude.NonEmpty ProductionVariantSummary)) (\s a -> s {productionVariants = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsProductionVariants "Use generic-lens or generic-optics with 'productionVariants' instead." #-}

-- | A timestamp that shows when the endpoint was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsLastModifiedTime :: Lens.Lens' DescribeEndpointResponse Lude.Timestamp
degrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeEndpointResponse -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of the endpoint.
--
--
--     * @OutOfService@ : Endpoint is not available to take incoming requests.
--
--
--     * @Creating@ : 'CreateEndpoint' is executing.
--
--
--     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.
--
--
--     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.
--
--
--     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.
--
--
--     * @InService@ : Endpoint is available to process incoming requests.
--
--
--     * @Deleting@ : 'DeleteEndpoint' is executing.
--
--
--     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint.
--
--
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsEndpointStatus :: Lens.Lens' DescribeEndpointResponse EndpointStatus
degrsEndpointStatus = Lens.lens (endpointStatus :: DescribeEndpointResponse -> EndpointStatus) (\s a -> s {endpointStatus = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | The name of the endpoint configuration associated with this endpoint.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsEndpointConfigName :: Lens.Lens' DescribeEndpointResponse Lude.Text
degrsEndpointConfigName = Lens.lens (endpointConfigName :: DescribeEndpointResponse -> Lude.Text) (\s a -> s {endpointConfigName = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsEndpointARN :: Lens.Lens' DescribeEndpointResponse Lude.Text
degrsEndpointARN = Lens.lens (endpointARN :: DescribeEndpointResponse -> Lude.Text) (\s a -> s {endpointARN = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsDataCaptureConfig :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe DataCaptureConfigSummary)
degrsDataCaptureConfig = Lens.lens (dataCaptureConfig :: DescribeEndpointResponse -> Lude.Maybe DataCaptureConfigSummary) (\s a -> s {dataCaptureConfig = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsDataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsResponseStatus :: Lens.Lens' DescribeEndpointResponse Lude.Int
degrsResponseStatus = Lens.lens (responseStatus :: DescribeEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointResponse)
{-# DEPRECATED degrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dersFailureReason,
    dersProductionVariants,
    dersDataCaptureConfig,
    dersResponseStatus,
    dersEndpointName,
    dersEndpointARN,
    dersEndpointConfigName,
    dersEndpointStatus,
    dersCreationTime,
    dersLastModifiedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { endpointName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
            Lude.<$> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "ProductionVariants")
            Lude.<*> (x Lude..?> "DataCaptureConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "EndpointName")
            Lude.<*> (x Lude..:> "EndpointArn")
            Lude.<*> (x Lude..:> "EndpointConfigName")
            Lude.<*> (x Lude..:> "EndpointStatus")
            Lude.<*> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "LastModifiedTime")
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
  { failureReason ::
      Lude.Maybe Lude.Text,
    productionVariants ::
      Lude.Maybe
        ( Lude.NonEmpty
            ProductionVariantSummary
        ),
    dataCaptureConfig ::
      Lude.Maybe DataCaptureConfigSummary,
    responseStatus :: Lude.Int,
    endpointName :: Lude.Text,
    endpointARN :: Lude.Text,
    endpointConfigName :: Lude.Text,
    endpointStatus :: EndpointStatus,
    creationTime :: Lude.Timestamp,
    lastModifiedTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the endpoint was created.
-- * 'dataCaptureConfig' - Undocumented field.
-- * 'endpointARN' - The Amazon Resource Name (ARN) of the endpoint.
-- * 'endpointConfigName' - The name of the endpoint configuration associated with this endpoint.
-- * 'endpointName' - Name of the endpoint.
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
-- * 'failureReason' - If the status of the endpoint is @Failed@ , the reason why it failed.
-- * 'lastModifiedTime' - A timestamp that shows when the endpoint was last modified.
-- * 'productionVariants' - An array of 'ProductionVariantSummary' objects, one for each model hosted behind this endpoint.
-- * 'responseStatus' - The response status code.
mkDescribeEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'endpointName'
  Lude.Text ->
  -- | 'endpointARN'
  Lude.Text ->
  -- | 'endpointConfigName'
  Lude.Text ->
  -- | 'endpointStatus'
  EndpointStatus ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  DescribeEndpointResponse
mkDescribeEndpointResponse
  pResponseStatus_
  pEndpointName_
  pEndpointARN_
  pEndpointConfigName_
  pEndpointStatus_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeEndpointResponse'
      { failureReason = Lude.Nothing,
        productionVariants = Lude.Nothing,
        dataCaptureConfig = Lude.Nothing,
        responseStatus = pResponseStatus_,
        endpointName = pEndpointName_,
        endpointARN = pEndpointARN_,
        endpointConfigName = pEndpointConfigName_,
        endpointStatus = pEndpointStatus_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_
      }

-- | If the status of the endpoint is @Failed@ , the reason why it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersFailureReason :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe Lude.Text)
dersFailureReason = Lens.lens (failureReason :: DescribeEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | An array of 'ProductionVariantSummary' objects, one for each model hosted behind this endpoint.
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersProductionVariants :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe (Lude.NonEmpty ProductionVariantSummary))
dersProductionVariants = Lens.lens (productionVariants :: DescribeEndpointResponse -> Lude.Maybe (Lude.NonEmpty ProductionVariantSummary)) (\s a -> s {productionVariants = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersProductionVariants "Use generic-lens or generic-optics with 'productionVariants' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersDataCaptureConfig :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe DataCaptureConfigSummary)
dersDataCaptureConfig = Lens.lens (dataCaptureConfig :: DescribeEndpointResponse -> Lude.Maybe DataCaptureConfigSummary) (\s a -> s {dataCaptureConfig = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersDataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEndpointResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Name of the endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpointName :: Lens.Lens' DescribeEndpointResponse Lude.Text
dersEndpointName = Lens.lens (endpointName :: DescribeEndpointResponse -> Lude.Text) (\s a -> s {endpointName = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpointARN :: Lens.Lens' DescribeEndpointResponse Lude.Text
dersEndpointARN = Lens.lens (endpointARN :: DescribeEndpointResponse -> Lude.Text) (\s a -> s {endpointARN = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The name of the endpoint configuration associated with this endpoint.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpointConfigName :: Lens.Lens' DescribeEndpointResponse Lude.Text
dersEndpointConfigName = Lens.lens (endpointConfigName :: DescribeEndpointResponse -> Lude.Text) (\s a -> s {endpointConfigName = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

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
dersEndpointStatus :: Lens.Lens' DescribeEndpointResponse EndpointStatus
dersEndpointStatus = Lens.lens (endpointStatus :: DescribeEndpointResponse -> EndpointStatus) (\s a -> s {endpointStatus = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | A timestamp that shows when the endpoint was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersCreationTime :: Lens.Lens' DescribeEndpointResponse Lude.Timestamp
dersCreationTime = Lens.lens (creationTime :: DescribeEndpointResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that shows when the endpoint was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersLastModifiedTime :: Lens.Lens' DescribeEndpointResponse Lude.Timestamp
dersLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeEndpointResponse -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

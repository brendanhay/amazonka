-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointSummary
  ( EndpointSummary (..),

    -- * Smart constructor
    mkEndpointSummary,

    -- * Lenses
    esEndpointName,
    esEndpointARN,
    esCreationTime,
    esLastModifiedTime,
    esEndpointStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.EndpointStatus

-- | Provides summary information for an endpoint.
--
-- /See:/ 'mkEndpointSummary' smart constructor.
data EndpointSummary = EndpointSummary'
  { endpointName :: Lude.Text,
    endpointARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    lastModifiedTime :: Lude.Timestamp,
    endpointStatus :: EndpointStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the endpoint was created.
-- * 'endpointARN' - The Amazon Resource Name (ARN) of the endpoint.
-- * 'endpointName' - The name of the endpoint.
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
-- To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
-- * 'lastModifiedTime' - A timestamp that shows when the endpoint was last modified.
mkEndpointSummary ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'endpointARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'endpointStatus'
  EndpointStatus ->
  EndpointSummary
mkEndpointSummary
  pEndpointName_
  pEndpointARN_
  pCreationTime_
  pLastModifiedTime_
  pEndpointStatus_ =
    EndpointSummary'
      { endpointName = pEndpointName_,
        endpointARN = pEndpointARN_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_,
        endpointStatus = pEndpointStatus_
      }

-- | The name of the endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointName :: Lens.Lens' EndpointSummary Lude.Text
esEndpointName = Lens.lens (endpointName :: EndpointSummary -> Lude.Text) (\s a -> s {endpointName = a} :: EndpointSummary)
{-# DEPRECATED esEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointARN :: Lens.Lens' EndpointSummary Lude.Text
esEndpointARN = Lens.lens (endpointARN :: EndpointSummary -> Lude.Text) (\s a -> s {endpointARN = a} :: EndpointSummary)
{-# DEPRECATED esEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | A timestamp that shows when the endpoint was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCreationTime :: Lens.Lens' EndpointSummary Lude.Timestamp
esCreationTime = Lens.lens (creationTime :: EndpointSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: EndpointSummary)
{-# DEPRECATED esCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that shows when the endpoint was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLastModifiedTime :: Lens.Lens' EndpointSummary Lude.Timestamp
esLastModifiedTime = Lens.lens (lastModifiedTime :: EndpointSummary -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: EndpointSummary)
{-# DEPRECATED esLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

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
-- To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointStatus :: Lens.Lens' EndpointSummary EndpointStatus
esEndpointStatus = Lens.lens (endpointStatus :: EndpointSummary -> EndpointStatus) (\s a -> s {endpointStatus = a} :: EndpointSummary)
{-# DEPRECATED esEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

instance Lude.FromJSON EndpointSummary where
  parseJSON =
    Lude.withObject
      "EndpointSummary"
      ( \x ->
          EndpointSummary'
            Lude.<$> (x Lude..: "EndpointName")
            Lude.<*> (x Lude..: "EndpointArn")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LastModifiedTime")
            Lude.<*> (x Lude..: "EndpointStatus")
      )

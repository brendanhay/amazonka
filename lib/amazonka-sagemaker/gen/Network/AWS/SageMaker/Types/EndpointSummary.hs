{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.EndpointStatus

-- | Provides summary information for an endpoint.
--
--
--
-- /See:/ 'endpointSummary' smart constructor.
data EndpointSummary = EndpointSummary'
  { _esEndpointName :: !Text,
    _esEndpointARN :: !Text,
    _esCreationTime :: !POSIX,
    _esLastModifiedTime :: !POSIX,
    _esEndpointStatus :: !EndpointStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esEndpointName' - The name of the endpoint.
--
-- * 'esEndpointARN' - The Amazon Resource Name (ARN) of the endpoint.
--
-- * 'esCreationTime' - A timestamp that shows when the endpoint was created.
--
-- * 'esLastModifiedTime' - A timestamp that shows when the endpoint was last modified.
--
-- * 'esEndpointStatus' - The status of the endpoint.     * @OutOfService@ : Endpoint is not available to take incoming requests.     * @Creating@ : 'CreateEndpoint' is executing.     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.     * @InService@ : Endpoint is available to process incoming requests.     * @Deleting@ : 'DeleteEndpoint' is executing.     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint. To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
endpointSummary ::
  -- | 'esEndpointName'
  Text ->
  -- | 'esEndpointARN'
  Text ->
  -- | 'esCreationTime'
  UTCTime ->
  -- | 'esLastModifiedTime'
  UTCTime ->
  -- | 'esEndpointStatus'
  EndpointStatus ->
  EndpointSummary
endpointSummary
  pEndpointName_
  pEndpointARN_
  pCreationTime_
  pLastModifiedTime_
  pEndpointStatus_ =
    EndpointSummary'
      { _esEndpointName = pEndpointName_,
        _esEndpointARN = pEndpointARN_,
        _esCreationTime = _Time # pCreationTime_,
        _esLastModifiedTime = _Time # pLastModifiedTime_,
        _esEndpointStatus = pEndpointStatus_
      }

-- | The name of the endpoint.
esEndpointName :: Lens' EndpointSummary Text
esEndpointName = lens _esEndpointName (\s a -> s {_esEndpointName = a})

-- | The Amazon Resource Name (ARN) of the endpoint.
esEndpointARN :: Lens' EndpointSummary Text
esEndpointARN = lens _esEndpointARN (\s a -> s {_esEndpointARN = a})

-- | A timestamp that shows when the endpoint was created.
esCreationTime :: Lens' EndpointSummary UTCTime
esCreationTime = lens _esCreationTime (\s a -> s {_esCreationTime = a}) . _Time

-- | A timestamp that shows when the endpoint was last modified.
esLastModifiedTime :: Lens' EndpointSummary UTCTime
esLastModifiedTime = lens _esLastModifiedTime (\s a -> s {_esLastModifiedTime = a}) . _Time

-- | The status of the endpoint.     * @OutOfService@ : Endpoint is not available to take incoming requests.     * @Creating@ : 'CreateEndpoint' is executing.     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.     * @InService@ : Endpoint is available to process incoming requests.     * @Deleting@ : 'DeleteEndpoint' is executing.     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint. To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
esEndpointStatus :: Lens' EndpointSummary EndpointStatus
esEndpointStatus = lens _esEndpointStatus (\s a -> s {_esEndpointStatus = a})

instance FromJSON EndpointSummary where
  parseJSON =
    withObject
      "EndpointSummary"
      ( \x ->
          EndpointSummary'
            <$> (x .: "EndpointName")
            <*> (x .: "EndpointArn")
            <*> (x .: "CreationTime")
            <*> (x .: "LastModifiedTime")
            <*> (x .: "EndpointStatus")
      )

instance Hashable EndpointSummary

instance NFData EndpointSummary

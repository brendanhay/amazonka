{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint configuration created using the @CreateEndpointConfig@ API.
module Network.AWS.SageMaker.DescribeEndpointConfig
  ( -- * Creating a request
    DescribeEndpointConfig (..),
    mkDescribeEndpointConfig,

    -- ** Request lenses
    dEndpointConfigName,

    -- * Destructuring the response
    DescribeEndpointConfigResponse (..),
    mkDescribeEndpointConfigResponse,

    -- ** Response lenses
    decrsEndpointConfigARN,
    decrsCreationTime,
    decrsProductionVariants,
    decrsKMSKeyId,
    decrsEndpointConfigName,
    decrsDataCaptureConfig,
    decrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeEndpointConfig' smart constructor.
newtype DescribeEndpointConfig = DescribeEndpointConfig'
  { -- | The name of the endpoint configuration.
    endpointConfigName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointConfig' with the minimum fields required to make a request.
--
-- * 'endpointConfigName' - The name of the endpoint configuration.
mkDescribeEndpointConfig ::
  -- | 'endpointConfigName'
  Lude.Text ->
  DescribeEndpointConfig
mkDescribeEndpointConfig pEndpointConfigName_ =
  DescribeEndpointConfig'
    { endpointConfigName =
        pEndpointConfigName_
    }

-- | The name of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndpointConfigName :: Lens.Lens' DescribeEndpointConfig Lude.Text
dEndpointConfigName = Lens.lens (endpointConfigName :: DescribeEndpointConfig -> Lude.Text) (\s a -> s {endpointConfigName = a} :: DescribeEndpointConfig)
{-# DEPRECATED dEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

instance Lude.AWSRequest DescribeEndpointConfig where
  type Rs DescribeEndpointConfig = DescribeEndpointConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointConfigResponse'
            Lude.<$> (x Lude..:> "EndpointConfigArn")
            Lude.<*> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "ProductionVariants")
            Lude.<*> (x Lude..?> "KmsKeyId")
            Lude.<*> (x Lude..:> "EndpointConfigName")
            Lude.<*> (x Lude..?> "DataCaptureConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEndpointConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeEndpointConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEndpointConfig where
  toJSON DescribeEndpointConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EndpointConfigName" Lude..= endpointConfigName)]
      )

instance Lude.ToPath DescribeEndpointConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEndpointConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEndpointConfigResponse' smart constructor.
data DescribeEndpointConfigResponse = DescribeEndpointConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the endpoint configuration.
    endpointConfigARN :: Lude.Text,
    -- | A timestamp that shows when the endpoint configuration was created.
    creationTime :: Lude.Timestamp,
    -- | An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
    productionVariants :: Lude.NonEmpty ProductionVariant,
    -- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Name of the Amazon SageMaker endpoint configuration.
    endpointConfigName :: Lude.Text,
    dataCaptureConfig :: Lude.Maybe DataCaptureConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointConfigResponse' with the minimum fields required to make a request.
--
-- * 'endpointConfigARN' - The Amazon Resource Name (ARN) of the endpoint configuration.
-- * 'creationTime' - A timestamp that shows when the endpoint configuration was created.
-- * 'productionVariants' - An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
-- * 'kmsKeyId' - AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
-- * 'endpointConfigName' - Name of the Amazon SageMaker endpoint configuration.
-- * 'dataCaptureConfig' -
-- * 'responseStatus' - The response status code.
mkDescribeEndpointConfigResponse ::
  -- | 'endpointConfigARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'productionVariants'
  Lude.NonEmpty ProductionVariant ->
  -- | 'endpointConfigName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointConfigResponse
mkDescribeEndpointConfigResponse
  pEndpointConfigARN_
  pCreationTime_
  pProductionVariants_
  pEndpointConfigName_
  pResponseStatus_ =
    DescribeEndpointConfigResponse'
      { endpointConfigARN =
          pEndpointConfigARN_,
        creationTime = pCreationTime_,
        productionVariants = pProductionVariants_,
        kmsKeyId = Lude.Nothing,
        endpointConfigName = pEndpointConfigName_,
        dataCaptureConfig = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsEndpointConfigARN :: Lens.Lens' DescribeEndpointConfigResponse Lude.Text
decrsEndpointConfigARN = Lens.lens (endpointConfigARN :: DescribeEndpointConfigResponse -> Lude.Text) (\s a -> s {endpointConfigARN = a} :: DescribeEndpointConfigResponse)
{-# DEPRECATED decrsEndpointConfigARN "Use generic-lens or generic-optics with 'endpointConfigARN' instead." #-}

-- | A timestamp that shows when the endpoint configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsCreationTime :: Lens.Lens' DescribeEndpointConfigResponse Lude.Timestamp
decrsCreationTime = Lens.lens (creationTime :: DescribeEndpointConfigResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeEndpointConfigResponse)
{-# DEPRECATED decrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsProductionVariants :: Lens.Lens' DescribeEndpointConfigResponse (Lude.NonEmpty ProductionVariant)
decrsProductionVariants = Lens.lens (productionVariants :: DescribeEndpointConfigResponse -> Lude.NonEmpty ProductionVariant) (\s a -> s {productionVariants = a} :: DescribeEndpointConfigResponse)
{-# DEPRECATED decrsProductionVariants "Use generic-lens or generic-optics with 'productionVariants' instead." #-}

-- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsKMSKeyId :: Lens.Lens' DescribeEndpointConfigResponse (Lude.Maybe Lude.Text)
decrsKMSKeyId = Lens.lens (kmsKeyId :: DescribeEndpointConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DescribeEndpointConfigResponse)
{-# DEPRECATED decrsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Name of the Amazon SageMaker endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsEndpointConfigName :: Lens.Lens' DescribeEndpointConfigResponse Lude.Text
decrsEndpointConfigName = Lens.lens (endpointConfigName :: DescribeEndpointConfigResponse -> Lude.Text) (\s a -> s {endpointConfigName = a} :: DescribeEndpointConfigResponse)
{-# DEPRECATED decrsEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsDataCaptureConfig :: Lens.Lens' DescribeEndpointConfigResponse (Lude.Maybe DataCaptureConfig)
decrsDataCaptureConfig = Lens.lens (dataCaptureConfig :: DescribeEndpointConfigResponse -> Lude.Maybe DataCaptureConfig) (\s a -> s {dataCaptureConfig = a} :: DescribeEndpointConfigResponse)
{-# DEPRECATED decrsDataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsResponseStatus :: Lens.Lens' DescribeEndpointConfigResponse Lude.Int
decrsResponseStatus = Lens.lens (responseStatus :: DescribeEndpointConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointConfigResponse)
{-# DEPRECATED decrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

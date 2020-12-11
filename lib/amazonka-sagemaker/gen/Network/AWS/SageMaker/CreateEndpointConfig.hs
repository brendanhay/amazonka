{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint configuration that Amazon SageMaker hosting services uses to deploy models. In the configuration, you identify one or more models, created using the @CreateModel@ API, to deploy and the resources that you want Amazon SageMaker to provision. Then you call the 'CreateEndpoint' API.
--
-- In the request, you define a @ProductionVariant@ , for each model that you want to deploy. Each @ProductionVariant@ parameter also describes the resources that you want Amazon SageMaker to provision. This includes the number and type of ML compute instances to deploy.
-- If you are hosting multiple models, you also assign a @VariantWeight@ to specify how much traffic you want to allocate to each model. For example, suppose that you want to host two models, A and B, and you assign traffic weight 2 for model A and 1 for model B. Amazon SageMaker distributes two-thirds of the traffic to Model A, and one-third to model B.
-- For an example that calls this method when deploying a model to Amazon SageMaker hosting services, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html#ex1-deploy-model-boto Deploy the Model to Amazon SageMaker Hosting Services (AWS SDK for Python (Boto 3)).>
module Network.AWS.SageMaker.CreateEndpointConfig
  ( -- * Creating a request
    CreateEndpointConfig (..),
    mkCreateEndpointConfig,

    -- ** Request lenses
    cecKMSKeyId,
    cecDataCaptureConfig,
    cecTags,
    cecEndpointConfigName,
    cecProductionVariants,

    -- * Destructuring the response
    CreateEndpointConfigResponse (..),
    mkCreateEndpointConfigResponse,

    -- ** Response lenses
    cecrsResponseStatus,
    cecrsEndpointConfigARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateEndpointConfig' smart constructor.
data CreateEndpointConfig = CreateEndpointConfig'
  { kmsKeyId ::
      Lude.Maybe Lude.Text,
    dataCaptureConfig :: Lude.Maybe DataCaptureConfig,
    tags :: Lude.Maybe [Tag],
    endpointConfigName :: Lude.Text,
    productionVariants ::
      Lude.NonEmpty ProductionVariant
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpointConfig' with the minimum fields required to make a request.
--
-- * 'dataCaptureConfig' - Undocumented field.
-- * 'endpointConfigName' - The name of the endpoint configuration. You specify this name in a 'CreateEndpoint' request.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance that hosts the endpoint.
--
-- The KmsKeyId can be any of the following formats:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@
--
--
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateEndpoint@ , @UpdateEndpoint@ requests. For more information, refer to the AWS Key Management Service section<https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS >
-- * 'productionVariants' - An list of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
-- * 'tags' - A list of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateEndpointConfig ::
  -- | 'endpointConfigName'
  Lude.Text ->
  -- | 'productionVariants'
  Lude.NonEmpty ProductionVariant ->
  CreateEndpointConfig
mkCreateEndpointConfig pEndpointConfigName_ pProductionVariants_ =
  CreateEndpointConfig'
    { kmsKeyId = Lude.Nothing,
      dataCaptureConfig = Lude.Nothing,
      tags = Lude.Nothing,
      endpointConfigName = pEndpointConfigName_,
      productionVariants = pProductionVariants_
    }

-- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance that hosts the endpoint.
--
-- The KmsKeyId can be any of the following formats:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@
--
--
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateEndpoint@ , @UpdateEndpoint@ requests. For more information, refer to the AWS Key Management Service section<https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS >
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecKMSKeyId :: Lens.Lens' CreateEndpointConfig (Lude.Maybe Lude.Text)
cecKMSKeyId = Lens.lens (kmsKeyId :: CreateEndpointConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateEndpointConfig)
{-# DEPRECATED cecKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecDataCaptureConfig :: Lens.Lens' CreateEndpointConfig (Lude.Maybe DataCaptureConfig)
cecDataCaptureConfig = Lens.lens (dataCaptureConfig :: CreateEndpointConfig -> Lude.Maybe DataCaptureConfig) (\s a -> s {dataCaptureConfig = a} :: CreateEndpointConfig)
{-# DEPRECATED cecDataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead." #-}

-- | A list of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecTags :: Lens.Lens' CreateEndpointConfig (Lude.Maybe [Tag])
cecTags = Lens.lens (tags :: CreateEndpointConfig -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEndpointConfig)
{-# DEPRECATED cecTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the endpoint configuration. You specify this name in a 'CreateEndpoint' request.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecEndpointConfigName :: Lens.Lens' CreateEndpointConfig Lude.Text
cecEndpointConfigName = Lens.lens (endpointConfigName :: CreateEndpointConfig -> Lude.Text) (\s a -> s {endpointConfigName = a} :: CreateEndpointConfig)
{-# DEPRECATED cecEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | An list of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecProductionVariants :: Lens.Lens' CreateEndpointConfig (Lude.NonEmpty ProductionVariant)
cecProductionVariants = Lens.lens (productionVariants :: CreateEndpointConfig -> Lude.NonEmpty ProductionVariant) (\s a -> s {productionVariants = a} :: CreateEndpointConfig)
{-# DEPRECATED cecProductionVariants "Use generic-lens or generic-optics with 'productionVariants' instead." #-}

instance Lude.AWSRequest CreateEndpointConfig where
  type Rs CreateEndpointConfig = CreateEndpointConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEndpointConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "EndpointConfigArn")
      )

instance Lude.ToHeaders CreateEndpointConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateEndpointConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEndpointConfig where
  toJSON CreateEndpointConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("DataCaptureConfig" Lude..=) Lude.<$> dataCaptureConfig,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("EndpointConfigName" Lude..= endpointConfigName),
            Lude.Just ("ProductionVariants" Lude..= productionVariants)
          ]
      )

instance Lude.ToPath CreateEndpointConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEndpointConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEndpointConfigResponse' smart constructor.
data CreateEndpointConfigResponse = CreateEndpointConfigResponse'
  { responseStatus ::
      Lude.Int,
    endpointConfigARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpointConfigResponse' with the minimum fields required to make a request.
--
-- * 'endpointConfigARN' - The Amazon Resource Name (ARN) of the endpoint configuration.
-- * 'responseStatus' - The response status code.
mkCreateEndpointConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'endpointConfigARN'
  Lude.Text ->
  CreateEndpointConfigResponse
mkCreateEndpointConfigResponse pResponseStatus_ pEndpointConfigARN_ =
  CreateEndpointConfigResponse'
    { responseStatus = pResponseStatus_,
      endpointConfigARN = pEndpointConfigARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecrsResponseStatus :: Lens.Lens' CreateEndpointConfigResponse Lude.Int
cecrsResponseStatus = Lens.lens (responseStatus :: CreateEndpointConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEndpointConfigResponse)
{-# DEPRECATED cecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecrsEndpointConfigARN :: Lens.Lens' CreateEndpointConfigResponse Lude.Text
cecrsEndpointConfigARN = Lens.lens (endpointConfigARN :: CreateEndpointConfigResponse -> Lude.Text) (\s a -> s {endpointConfigARN = a} :: CreateEndpointConfigResponse)
{-# DEPRECATED cecrsEndpointConfigARN "Use generic-lens or generic-optics with 'endpointConfigARN' instead." #-}

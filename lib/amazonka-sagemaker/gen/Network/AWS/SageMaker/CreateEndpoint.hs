{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the endpoint configuration specified in the request. Amazon SageMaker uses the endpoint to provision resources and deploy models. You create the endpoint configuration with the 'CreateEndpointConfig' API.
--
-- Use this API to deploy models using Amazon SageMaker hosting services.
-- For an example that calls this method when deploying a model to Amazon SageMaker hosting services, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html#ex1-deploy-model-boto Deploy the Model to Amazon SageMaker Hosting Services (AWS SDK for Python (Boto 3)).>
-- The endpoint name must be unique within an AWS Region in your AWS account.
-- When it receives the request, Amazon SageMaker creates the endpoint, launches the resources (ML compute instances), and deploys the model(s) on them.
-- When Amazon SageMaker receives the request, it sets the endpoint status to @Creating@ . After it creates the endpoint, it sets the status to @InService@ . Amazon SageMaker can then process incoming requests for inferences. To check the status of an endpoint, use the 'DescribeEndpoint' API.
-- If any of the models hosted at this endpoint get model data from an Amazon S3 location, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provided. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
module Network.AWS.SageMaker.CreateEndpoint
  ( -- * Creating a request
    CreateEndpoint (..),
    mkCreateEndpoint,

    -- ** Request lenses
    ceEndpointName,
    ceEndpointConfigName,
    ceTags,

    -- * Destructuring the response
    CreateEndpointResponse (..),
    mkCreateEndpointResponse,

    -- ** Response lenses
    cefrsEndpointARN,
    cefrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | The name of the endpoint.The name must be unique within an AWS Region in your AWS account. The name is case-insensitive in @CreateEndpoint@ , but the case is preserved and must be matched in .
    endpointName :: Lude.Text,
    -- | The name of an endpoint configuration. For more information, see 'CreateEndpointConfig' .
    endpointConfigName :: Lude.Text,
    -- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name of the endpoint.The name must be unique within an AWS Region in your AWS account. The name is case-insensitive in @CreateEndpoint@ , but the case is preserved and must be matched in .
-- * 'endpointConfigName' - The name of an endpoint configuration. For more information, see 'CreateEndpointConfig' .
-- * 'tags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'endpointConfigName'
  Lude.Text ->
  CreateEndpoint
mkCreateEndpoint pEndpointName_ pEndpointConfigName_ =
  CreateEndpoint'
    { endpointName = pEndpointName_,
      endpointConfigName = pEndpointConfigName_,
      tags = Lude.Nothing
    }

-- | The name of the endpoint.The name must be unique within an AWS Region in your AWS account. The name is case-insensitive in @CreateEndpoint@ , but the case is preserved and must be matched in .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointName :: Lens.Lens' CreateEndpoint Lude.Text
ceEndpointName = Lens.lens (endpointName :: CreateEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: CreateEndpoint)
{-# DEPRECATED ceEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The name of an endpoint configuration. For more information, see 'CreateEndpointConfig' .
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointConfigName :: Lens.Lens' CreateEndpoint Lude.Text
ceEndpointConfigName = Lens.lens (endpointConfigName :: CreateEndpoint -> Lude.Text) (\s a -> s {endpointConfigName = a} :: CreateEndpoint)
{-# DEPRECATED ceEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Lude.Maybe [Tag])
ceTags = Lens.lens (tags :: CreateEndpoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEndpoint)
{-# DEPRECATED ceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Lude.<$> (x Lude..:> "EndpointArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EndpointName" Lude..= endpointName),
            Lude.Just ("EndpointConfigName" Lude..= endpointConfigName),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) of the endpoint.
-- * 'responseStatus' - The response status code.
mkCreateEndpointResponse ::
  -- | 'endpointARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateEndpointResponse
mkCreateEndpointResponse pEndpointARN_ pResponseStatus_ =
  CreateEndpointResponse'
    { endpointARN = pEndpointARN_,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cefrsEndpointARN :: Lens.Lens' CreateEndpointResponse Lude.Text
cefrsEndpointARN = Lens.lens (endpointARN :: CreateEndpointResponse -> Lude.Text) (\s a -> s {endpointARN = a} :: CreateEndpointResponse)
{-# DEPRECATED cefrsEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cefrsResponseStatus :: Lens.Lens' CreateEndpointResponse Lude.Int
cefrsResponseStatus = Lens.lens (responseStatus :: CreateEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEndpointResponse)
{-# DEPRECATED cefrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

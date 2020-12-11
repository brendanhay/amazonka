{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.CreateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model-specific endpoint for synchronous inference for a previously trained custom model
module Network.AWS.Comprehend.CreateEndpoint
  ( -- * Creating a request
    CreateEndpoint (..),
    mkCreateEndpoint,

    -- ** Request lenses
    ceClientRequestToken,
    ceTags,
    ceEndpointName,
    ceModelARN,
    ceDesiredInferenceUnits,

    -- * Destructuring the response
    CreateEndpointResponse (..),
    mkCreateEndpointResponse,

    -- ** Response lenses
    cersEndpointARN,
    cersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { clientRequestToken ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    endpointName :: Lude.Text,
    modelARN :: Lude.Text,
    desiredInferenceUnits :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ .
-- * 'desiredInferenceUnits' - The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
-- * 'endpointName' - This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource.
-- * 'modelARN' - The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
-- * 'tags' - Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department.
mkCreateEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'modelARN'
  Lude.Text ->
  -- | 'desiredInferenceUnits'
  Lude.Natural ->
  CreateEndpoint
mkCreateEndpoint pEndpointName_ pModelARN_ pDesiredInferenceUnits_ =
  CreateEndpoint'
    { clientRequestToken = Lude.Nothing,
      tags = Lude.Nothing,
      endpointName = pEndpointName_,
      modelARN = pModelARN_,
      desiredInferenceUnits = pDesiredInferenceUnits_
    }

-- | An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceClientRequestToken :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceClientRequestToken = Lens.lens (clientRequestToken :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateEndpoint)
{-# DEPRECATED ceClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Lude.Maybe [Tag])
ceTags = Lens.lens (tags :: CreateEndpoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEndpoint)
{-# DEPRECATED ceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointName :: Lens.Lens' CreateEndpoint Lude.Text
ceEndpointName = Lens.lens (endpointName :: CreateEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: CreateEndpoint)
{-# DEPRECATED ceEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
--
-- /Note:/ Consider using 'modelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceModelARN :: Lens.Lens' CreateEndpoint Lude.Text
ceModelARN = Lens.lens (modelARN :: CreateEndpoint -> Lude.Text) (\s a -> s {modelARN = a} :: CreateEndpoint)
{-# DEPRECATED ceModelARN "Use generic-lens or generic-optics with 'modelARN' instead." #-}

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDesiredInferenceUnits :: Lens.Lens' CreateEndpoint Lude.Natural
ceDesiredInferenceUnits = Lens.lens (desiredInferenceUnits :: CreateEndpoint -> Lude.Natural) (\s a -> s {desiredInferenceUnits = a} :: CreateEndpoint)
{-# DEPRECATED ceDesiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead." #-}

instance Lude.AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Lude.<$> (x Lude..?> "EndpointArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.CreateEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("EndpointName" Lude..= endpointName),
            Lude.Just ("ModelArn" Lude..= modelARN),
            Lude.Just ("DesiredInferenceUnits" Lude..= desiredInferenceUnits)
          ]
      )

instance Lude.ToPath CreateEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { endpointARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Number (ARN) of the endpoint being created.
-- * 'responseStatus' - The response status code.
mkCreateEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEndpointResponse
mkCreateEndpointResponse pResponseStatus_ =
  CreateEndpointResponse'
    { endpointARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Number (ARN) of the endpoint being created.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cersEndpointARN :: Lens.Lens' CreateEndpointResponse (Lude.Maybe Lude.Text)
cersEndpointARN = Lens.lens (endpointARN :: CreateEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: CreateEndpointResponse)
{-# DEPRECATED cersEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cersResponseStatus :: Lens.Lens' CreateEndpointResponse Lude.Int
cersResponseStatus = Lens.lens (responseStatus :: CreateEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEndpointResponse)
{-# DEPRECATED cersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys the new @EndpointConfig@ specified in the request, switches to using newly created endpoint, and then deletes resources provisioned for the endpoint using the previous @EndpointConfig@ (there is no availability loss).
--
-- When Amazon SageMaker receives the request, it sets the endpoint status to @Updating@ . After updating the endpoint, it sets the status to @InService@ . To check the status of an endpoint, use the 'DescribeEndpoint' API.
module Network.AWS.SageMaker.UpdateEndpoint
  ( -- * Creating a request
    UpdateEndpoint (..),
    mkUpdateEndpoint,

    -- ** Request lenses
    ueEndpointName,
    ueExcludeRetainedVariantProperties,
    ueRetainAllVariantProperties,
    ueEndpointConfigName,

    -- * Destructuring the response
    UpdateEndpointResponse (..),
    mkUpdateEndpointResponse,

    -- ** Response lenses
    uersEndpointARN,
    uersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | The name of the endpoint whose configuration you want to update.
    endpointName :: Lude.Text,
    -- | When you are updating endpoint resources with 'UpdateEndpointInput$RetainAllVariantProperties' , whose value is set to @true@ , @ExcludeRetainedVariantProperties@ specifies the list of type 'VariantProperty' to override with the values provided by @EndpointConfig@ . If you don't specify a value for @ExcludeAllVariantProperties@ , no variant properties are overridden.
    excludeRetainedVariantProperties :: Lude.Maybe [VariantProperty],
    -- | When updating endpoint resources, enables or disables the retention of variant properties, such as the instance count or the variant weight. To retain the variant properties of an endpoint when updating it, set @RetainAllVariantProperties@ to @true@ . To use the variant properties specified in a new @EndpointConfig@ call when updating an endpoint, set @RetainAllVariantProperties@ to @false@ .
    retainAllVariantProperties :: Lude.Maybe Lude.Bool,
    -- | The name of the new endpoint configuration.
    endpointConfigName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name of the endpoint whose configuration you want to update.
-- * 'excludeRetainedVariantProperties' - When you are updating endpoint resources with 'UpdateEndpointInput$RetainAllVariantProperties' , whose value is set to @true@ , @ExcludeRetainedVariantProperties@ specifies the list of type 'VariantProperty' to override with the values provided by @EndpointConfig@ . If you don't specify a value for @ExcludeAllVariantProperties@ , no variant properties are overridden.
-- * 'retainAllVariantProperties' - When updating endpoint resources, enables or disables the retention of variant properties, such as the instance count or the variant weight. To retain the variant properties of an endpoint when updating it, set @RetainAllVariantProperties@ to @true@ . To use the variant properties specified in a new @EndpointConfig@ call when updating an endpoint, set @RetainAllVariantProperties@ to @false@ .
-- * 'endpointConfigName' - The name of the new endpoint configuration.
mkUpdateEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'endpointConfigName'
  Lude.Text ->
  UpdateEndpoint
mkUpdateEndpoint pEndpointName_ pEndpointConfigName_ =
  UpdateEndpoint'
    { endpointName = pEndpointName_,
      excludeRetainedVariantProperties = Lude.Nothing,
      retainAllVariantProperties = Lude.Nothing,
      endpointConfigName = pEndpointConfigName_
    }

-- | The name of the endpoint whose configuration you want to update.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointName :: Lens.Lens' UpdateEndpoint Lude.Text
ueEndpointName = Lens.lens (endpointName :: UpdateEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: UpdateEndpoint)
{-# DEPRECATED ueEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | When you are updating endpoint resources with 'UpdateEndpointInput$RetainAllVariantProperties' , whose value is set to @true@ , @ExcludeRetainedVariantProperties@ specifies the list of type 'VariantProperty' to override with the values provided by @EndpointConfig@ . If you don't specify a value for @ExcludeAllVariantProperties@ , no variant properties are overridden.
--
-- /Note:/ Consider using 'excludeRetainedVariantProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueExcludeRetainedVariantProperties :: Lens.Lens' UpdateEndpoint (Lude.Maybe [VariantProperty])
ueExcludeRetainedVariantProperties = Lens.lens (excludeRetainedVariantProperties :: UpdateEndpoint -> Lude.Maybe [VariantProperty]) (\s a -> s {excludeRetainedVariantProperties = a} :: UpdateEndpoint)
{-# DEPRECATED ueExcludeRetainedVariantProperties "Use generic-lens or generic-optics with 'excludeRetainedVariantProperties' instead." #-}

-- | When updating endpoint resources, enables or disables the retention of variant properties, such as the instance count or the variant weight. To retain the variant properties of an endpoint when updating it, set @RetainAllVariantProperties@ to @true@ . To use the variant properties specified in a new @EndpointConfig@ call when updating an endpoint, set @RetainAllVariantProperties@ to @false@ .
--
-- /Note:/ Consider using 'retainAllVariantProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueRetainAllVariantProperties :: Lens.Lens' UpdateEndpoint (Lude.Maybe Lude.Bool)
ueRetainAllVariantProperties = Lens.lens (retainAllVariantProperties :: UpdateEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {retainAllVariantProperties = a} :: UpdateEndpoint)
{-# DEPRECATED ueRetainAllVariantProperties "Use generic-lens or generic-optics with 'retainAllVariantProperties' instead." #-}

-- | The name of the new endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointConfigName :: Lens.Lens' UpdateEndpoint Lude.Text
ueEndpointConfigName = Lens.lens (endpointConfigName :: UpdateEndpoint -> Lude.Text) (\s a -> s {endpointConfigName = a} :: UpdateEndpoint)
{-# DEPRECATED ueEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

instance Lude.AWSRequest UpdateEndpoint where
  type Rs UpdateEndpoint = UpdateEndpointResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEndpointResponse'
            Lude.<$> (x Lude..:> "EndpointArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EndpointName" Lude..= endpointName),
            ("ExcludeRetainedVariantProperties" Lude..=)
              Lude.<$> excludeRetainedVariantProperties,
            ("RetainAllVariantProperties" Lude..=)
              Lude.<$> retainAllVariantProperties,
            Lude.Just ("EndpointConfigName" Lude..= endpointConfigName)
          ]
      )

instance Lude.ToPath UpdateEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) of the endpoint.
-- * 'responseStatus' - The response status code.
mkUpdateEndpointResponse ::
  -- | 'endpointARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEndpointResponse
mkUpdateEndpointResponse pEndpointARN_ pResponseStatus_ =
  UpdateEndpointResponse'
    { endpointARN = pEndpointARN_,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersEndpointARN :: Lens.Lens' UpdateEndpointResponse Lude.Text
uersEndpointARN = Lens.lens (endpointARN :: UpdateEndpointResponse -> Lude.Text) (\s a -> s {endpointARN = a} :: UpdateEndpointResponse)
{-# DEPRECATED uersEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersResponseStatus :: Lens.Lens' UpdateEndpointResponse Lude.Int
uersResponseStatus = Lens.lens (responseStatus :: UpdateEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEndpointResponse)
{-# DEPRECATED uersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

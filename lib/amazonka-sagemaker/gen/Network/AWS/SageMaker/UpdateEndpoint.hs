{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ueExcludeRetainedVariantProperties,
    ueRetainAllVariantProperties,
    ueEndpointName,
    ueEndpointConfigName,

    -- * Destructuring the response
    UpdateEndpointResponse (..),
    mkUpdateEndpointResponse,

    -- ** Response lenses
    uersResponseStatus,
    uersEndpointARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { excludeRetainedVariantProperties ::
      Lude.Maybe [VariantProperty],
    retainAllVariantProperties :: Lude.Maybe Lude.Bool,
    endpointName :: Lude.Text,
    endpointConfigName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointConfigName' - The name of the new endpoint configuration.
-- * 'endpointName' - The name of the endpoint whose configuration you want to update.
-- * 'excludeRetainedVariantProperties' - When you are updating endpoint resources with 'UpdateEndpointInput$RetainAllVariantProperties' , whose value is set to @true@ , @ExcludeRetainedVariantProperties@ specifies the list of type 'VariantProperty' to override with the values provided by @EndpointConfig@ . If you don't specify a value for @ExcludeAllVariantProperties@ , no variant properties are overridden.
-- * 'retainAllVariantProperties' - When updating endpoint resources, enables or disables the retention of variant properties, such as the instance count or the variant weight. To retain the variant properties of an endpoint when updating it, set @RetainAllVariantProperties@ to @true@ . To use the variant properties specified in a new @EndpointConfig@ call when updating an endpoint, set @RetainAllVariantProperties@ to @false@ .
mkUpdateEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'endpointConfigName'
  Lude.Text ->
  UpdateEndpoint
mkUpdateEndpoint pEndpointName_ pEndpointConfigName_ =
  UpdateEndpoint'
    { excludeRetainedVariantProperties = Lude.Nothing,
      retainAllVariantProperties = Lude.Nothing,
      endpointName = pEndpointName_,
      endpointConfigName = pEndpointConfigName_
    }

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

-- | The name of the endpoint whose configuration you want to update.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointName :: Lens.Lens' UpdateEndpoint Lude.Text
ueEndpointName = Lens.lens (endpointName :: UpdateEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: UpdateEndpoint)
{-# DEPRECATED ueEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

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
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "EndpointArn")
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
          [ ("ExcludeRetainedVariantProperties" Lude..=)
              Lude.<$> excludeRetainedVariantProperties,
            ("RetainAllVariantProperties" Lude..=)
              Lude.<$> retainAllVariantProperties,
            Lude.Just ("EndpointName" Lude..= endpointName),
            Lude.Just ("EndpointConfigName" Lude..= endpointConfigName)
          ]
      )

instance Lude.ToPath UpdateEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { responseStatus ::
      Lude.Int,
    endpointARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) of the endpoint.
-- * 'responseStatus' - The response status code.
mkUpdateEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'endpointARN'
  Lude.Text ->
  UpdateEndpointResponse
mkUpdateEndpointResponse pResponseStatus_ pEndpointARN_ =
  UpdateEndpointResponse'
    { responseStatus = pResponseStatus_,
      endpointARN = pEndpointARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersResponseStatus :: Lens.Lens' UpdateEndpointResponse Lude.Int
uersResponseStatus = Lens.lens (responseStatus :: UpdateEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEndpointResponse)
{-# DEPRECATED uersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersEndpointARN :: Lens.Lens' UpdateEndpointResponse Lude.Text
uersEndpointARN = Lens.lens (endpointARN :: UpdateEndpointResponse -> Lude.Text) (\s a -> s {endpointARN = a} :: UpdateEndpointResponse)
{-# DEPRECATED uersEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

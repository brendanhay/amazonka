{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates variant weight of one or more variants associated with an existing endpoint, or capacity of one variant associated with an existing endpoint. When it receives the request, Amazon SageMaker sets the endpoint status to @Updating@ . After updating the endpoint, it sets the status to @InService@ . To check the status of an endpoint, use the 'DescribeEndpoint' API.
module Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
  ( -- * Creating a request
    UpdateEndpointWeightsAndCapacities (..),
    mkUpdateEndpointWeightsAndCapacities,

    -- ** Request lenses
    uewacEndpointName,
    uewacDesiredWeightsAndCapacities,

    -- * Destructuring the response
    UpdateEndpointWeightsAndCapacitiesResponse (..),
    mkUpdateEndpointWeightsAndCapacitiesResponse,

    -- ** Response lenses
    uewacrsEndpointARN,
    uewacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateEndpointWeightsAndCapacities' smart constructor.
data UpdateEndpointWeightsAndCapacities = UpdateEndpointWeightsAndCapacities'
  { -- | The name of an existing Amazon SageMaker endpoint.
    endpointName :: Lude.Text,
    -- | An object that provides new capacity and weight values for a variant.
    desiredWeightsAndCapacities :: Lude.NonEmpty DesiredWeightAndCapacity
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointWeightsAndCapacities' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name of an existing Amazon SageMaker endpoint.
-- * 'desiredWeightsAndCapacities' - An object that provides new capacity and weight values for a variant.
mkUpdateEndpointWeightsAndCapacities ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'desiredWeightsAndCapacities'
  Lude.NonEmpty DesiredWeightAndCapacity ->
  UpdateEndpointWeightsAndCapacities
mkUpdateEndpointWeightsAndCapacities
  pEndpointName_
  pDesiredWeightsAndCapacities_ =
    UpdateEndpointWeightsAndCapacities'
      { endpointName =
          pEndpointName_,
        desiredWeightsAndCapacities = pDesiredWeightsAndCapacities_
      }

-- | The name of an existing Amazon SageMaker endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacEndpointName :: Lens.Lens' UpdateEndpointWeightsAndCapacities Lude.Text
uewacEndpointName = Lens.lens (endpointName :: UpdateEndpointWeightsAndCapacities -> Lude.Text) (\s a -> s {endpointName = a} :: UpdateEndpointWeightsAndCapacities)
{-# DEPRECATED uewacEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | An object that provides new capacity and weight values for a variant.
--
-- /Note:/ Consider using 'desiredWeightsAndCapacities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacDesiredWeightsAndCapacities :: Lens.Lens' UpdateEndpointWeightsAndCapacities (Lude.NonEmpty DesiredWeightAndCapacity)
uewacDesiredWeightsAndCapacities = Lens.lens (desiredWeightsAndCapacities :: UpdateEndpointWeightsAndCapacities -> Lude.NonEmpty DesiredWeightAndCapacity) (\s a -> s {desiredWeightsAndCapacities = a} :: UpdateEndpointWeightsAndCapacities)
{-# DEPRECATED uewacDesiredWeightsAndCapacities "Use generic-lens or generic-optics with 'desiredWeightsAndCapacities' instead." #-}

instance Lude.AWSRequest UpdateEndpointWeightsAndCapacities where
  type
    Rs UpdateEndpointWeightsAndCapacities =
      UpdateEndpointWeightsAndCapacitiesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEndpointWeightsAndCapacitiesResponse'
            Lude.<$> (x Lude..:> "EndpointArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEndpointWeightsAndCapacities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.UpdateEndpointWeightsAndCapacities" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEndpointWeightsAndCapacities where
  toJSON UpdateEndpointWeightsAndCapacities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EndpointName" Lude..= endpointName),
            Lude.Just
              ( "DesiredWeightsAndCapacities"
                  Lude..= desiredWeightsAndCapacities
              )
          ]
      )

instance Lude.ToPath UpdateEndpointWeightsAndCapacities where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEndpointWeightsAndCapacities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEndpointWeightsAndCapacitiesResponse' smart constructor.
data UpdateEndpointWeightsAndCapacitiesResponse = UpdateEndpointWeightsAndCapacitiesResponse'
  { -- | The Amazon Resource Name (ARN) of the updated endpoint.
    endpointARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointWeightsAndCapacitiesResponse' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) of the updated endpoint.
-- * 'responseStatus' - The response status code.
mkUpdateEndpointWeightsAndCapacitiesResponse ::
  -- | 'endpointARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEndpointWeightsAndCapacitiesResponse
mkUpdateEndpointWeightsAndCapacitiesResponse
  pEndpointARN_
  pResponseStatus_ =
    UpdateEndpointWeightsAndCapacitiesResponse'
      { endpointARN =
          pEndpointARN_,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (ARN) of the updated endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacrsEndpointARN :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Lude.Text
uewacrsEndpointARN = Lens.lens (endpointARN :: UpdateEndpointWeightsAndCapacitiesResponse -> Lude.Text) (\s a -> s {endpointARN = a} :: UpdateEndpointWeightsAndCapacitiesResponse)
{-# DEPRECATED uewacrsEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uewacrsResponseStatus :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Lude.Int
uewacrsResponseStatus = Lens.lens (responseStatus :: UpdateEndpointWeightsAndCapacitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEndpointWeightsAndCapacitiesResponse)
{-# DEPRECATED uewacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

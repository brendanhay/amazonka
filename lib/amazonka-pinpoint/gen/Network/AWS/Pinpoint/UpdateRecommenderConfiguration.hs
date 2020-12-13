{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.UpdateRecommenderConfiguration
  ( -- * Creating a request
    UpdateRecommenderConfiguration (..),
    mkUpdateRecommenderConfiguration,

    -- ** Request lenses
    urcUpdateRecommenderConfiguration,
    urcRecommenderId,

    -- * Destructuring the response
    UpdateRecommenderConfigurationResponse (..),
    mkUpdateRecommenderConfigurationResponse,

    -- ** Response lenses
    urcrsRecommenderConfigurationResponse,
    urcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRecommenderConfiguration' smart constructor.
data UpdateRecommenderConfiguration = UpdateRecommenderConfiguration'
  { updateRecommenderConfiguration :: UpdateRecommenderConfiguration,
    -- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
    recommenderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRecommenderConfiguration' with the minimum fields required to make a request.
--
-- * 'updateRecommenderConfiguration' -
-- * 'recommenderId' - The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
mkUpdateRecommenderConfiguration ::
  -- | 'updateRecommenderConfiguration'
  UpdateRecommenderConfiguration ->
  -- | 'recommenderId'
  Lude.Text ->
  UpdateRecommenderConfiguration
mkUpdateRecommenderConfiguration
  pUpdateRecommenderConfiguration_
  pRecommenderId_ =
    UpdateRecommenderConfiguration'
      { updateRecommenderConfiguration =
          pUpdateRecommenderConfiguration_,
        recommenderId = pRecommenderId_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateRecommenderConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcUpdateRecommenderConfiguration :: Lens.Lens' UpdateRecommenderConfiguration UpdateRecommenderConfiguration
urcUpdateRecommenderConfiguration = Lens.lens (updateRecommenderConfiguration :: UpdateRecommenderConfiguration -> UpdateRecommenderConfiguration) (\s a -> s {updateRecommenderConfiguration = a} :: UpdateRecommenderConfiguration)
{-# DEPRECATED urcUpdateRecommenderConfiguration "Use generic-lens or generic-optics with 'updateRecommenderConfiguration' instead." #-}

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcRecommenderId :: Lens.Lens' UpdateRecommenderConfiguration Lude.Text
urcRecommenderId = Lens.lens (recommenderId :: UpdateRecommenderConfiguration -> Lude.Text) (\s a -> s {recommenderId = a} :: UpdateRecommenderConfiguration)
{-# DEPRECATED urcRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

instance Lude.AWSRequest UpdateRecommenderConfiguration where
  type
    Rs UpdateRecommenderConfiguration =
      UpdateRecommenderConfigurationResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRecommenderConfigurationResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRecommenderConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRecommenderConfiguration where
  toJSON UpdateRecommenderConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "UpdateRecommenderConfiguration"
                  Lude..= updateRecommenderConfiguration
              )
          ]
      )

instance Lude.ToPath UpdateRecommenderConfiguration where
  toPath UpdateRecommenderConfiguration' {..} =
    Lude.mconcat ["/v1/recommenders/", Lude.toBS recommenderId]

instance Lude.ToQuery UpdateRecommenderConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRecommenderConfigurationResponse' smart constructor.
data UpdateRecommenderConfigurationResponse = UpdateRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: RecommenderConfigurationResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'recommenderConfigurationResponse' -
-- * 'responseStatus' - The response status code.
mkUpdateRecommenderConfigurationResponse ::
  -- | 'recommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRecommenderConfigurationResponse
mkUpdateRecommenderConfigurationResponse
  pRecommenderConfigurationResponse_
  pResponseStatus_ =
    UpdateRecommenderConfigurationResponse'
      { recommenderConfigurationResponse =
          pRecommenderConfigurationResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrsRecommenderConfigurationResponse :: Lens.Lens' UpdateRecommenderConfigurationResponse RecommenderConfigurationResponse
urcrsRecommenderConfigurationResponse = Lens.lens (recommenderConfigurationResponse :: UpdateRecommenderConfigurationResponse -> RecommenderConfigurationResponse) (\s a -> s {recommenderConfigurationResponse = a} :: UpdateRecommenderConfigurationResponse)
{-# DEPRECATED urcrsRecommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrsResponseStatus :: Lens.Lens' UpdateRecommenderConfigurationResponse Lude.Int
urcrsResponseStatus = Lens.lens (responseStatus :: UpdateRecommenderConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRecommenderConfigurationResponse)
{-# DEPRECATED urcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

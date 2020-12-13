{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.DeleteRecommenderConfiguration
  ( -- * Creating a request
    DeleteRecommenderConfiguration (..),
    mkDeleteRecommenderConfiguration,

    -- ** Request lenses
    drcRecommenderId,

    -- * Destructuring the response
    DeleteRecommenderConfigurationResponse (..),
    mkDeleteRecommenderConfigurationResponse,

    -- ** Response lenses
    drcrsRecommenderConfigurationResponse,
    drcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRecommenderConfiguration' smart constructor.
newtype DeleteRecommenderConfiguration = DeleteRecommenderConfiguration'
  { -- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
    recommenderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRecommenderConfiguration' with the minimum fields required to make a request.
--
-- * 'recommenderId' - The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
mkDeleteRecommenderConfiguration ::
  -- | 'recommenderId'
  Lude.Text ->
  DeleteRecommenderConfiguration
mkDeleteRecommenderConfiguration pRecommenderId_ =
  DeleteRecommenderConfiguration' {recommenderId = pRecommenderId_}

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRecommenderId :: Lens.Lens' DeleteRecommenderConfiguration Lude.Text
drcRecommenderId = Lens.lens (recommenderId :: DeleteRecommenderConfiguration -> Lude.Text) (\s a -> s {recommenderId = a} :: DeleteRecommenderConfiguration)
{-# DEPRECATED drcRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

instance Lude.AWSRequest DeleteRecommenderConfiguration where
  type
    Rs DeleteRecommenderConfiguration =
      DeleteRecommenderConfigurationResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRecommenderConfigurationResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRecommenderConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteRecommenderConfiguration where
  toPath DeleteRecommenderConfiguration' {..} =
    Lude.mconcat ["/v1/recommenders/", Lude.toBS recommenderId]

instance Lude.ToQuery DeleteRecommenderConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRecommenderConfigurationResponse' smart constructor.
data DeleteRecommenderConfigurationResponse = DeleteRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: RecommenderConfigurationResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'recommenderConfigurationResponse' -
-- * 'responseStatus' - The response status code.
mkDeleteRecommenderConfigurationResponse ::
  -- | 'recommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRecommenderConfigurationResponse
mkDeleteRecommenderConfigurationResponse
  pRecommenderConfigurationResponse_
  pResponseStatus_ =
    DeleteRecommenderConfigurationResponse'
      { recommenderConfigurationResponse =
          pRecommenderConfigurationResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsRecommenderConfigurationResponse :: Lens.Lens' DeleteRecommenderConfigurationResponse RecommenderConfigurationResponse
drcrsRecommenderConfigurationResponse = Lens.lens (recommenderConfigurationResponse :: DeleteRecommenderConfigurationResponse -> RecommenderConfigurationResponse) (\s a -> s {recommenderConfigurationResponse = a} :: DeleteRecommenderConfigurationResponse)
{-# DEPRECATED drcrsRecommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsResponseStatus :: Lens.Lens' DeleteRecommenderConfigurationResponse Lude.Int
drcrsResponseStatus = Lens.lens (responseStatus :: DeleteRecommenderConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRecommenderConfigurationResponse)
{-# DEPRECATED drcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

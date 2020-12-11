{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.CreateRecommenderConfiguration
  ( -- * Creating a request
    CreateRecommenderConfiguration (..),
    mkCreateRecommenderConfiguration,

    -- ** Request lenses
    crcCreateRecommenderConfiguration,

    -- * Destructuring the response
    CreateRecommenderConfigurationResponse (..),
    mkCreateRecommenderConfigurationResponse,

    -- ** Response lenses
    crcrsResponseStatus,
    crcrsRecommenderConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRecommenderConfiguration' smart constructor.
newtype CreateRecommenderConfiguration = CreateRecommenderConfiguration'
  { createRecommenderConfiguration ::
      CreateRecommenderConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRecommenderConfiguration' with the minimum fields required to make a request.
--
-- * 'createRecommenderConfiguration' - Undocumented field.
mkCreateRecommenderConfiguration ::
  -- | 'createRecommenderConfiguration'
  CreateRecommenderConfiguration ->
  CreateRecommenderConfiguration
mkCreateRecommenderConfiguration pCreateRecommenderConfiguration_ =
  CreateRecommenderConfiguration'
    { createRecommenderConfiguration =
        pCreateRecommenderConfiguration_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createRecommenderConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcCreateRecommenderConfiguration :: Lens.Lens' CreateRecommenderConfiguration CreateRecommenderConfiguration
crcCreateRecommenderConfiguration = Lens.lens (createRecommenderConfiguration :: CreateRecommenderConfiguration -> CreateRecommenderConfiguration) (\s a -> s {createRecommenderConfiguration = a} :: CreateRecommenderConfiguration)
{-# DEPRECATED crcCreateRecommenderConfiguration "Use generic-lens or generic-optics with 'createRecommenderConfiguration' instead." #-}

instance Lude.AWSRequest CreateRecommenderConfiguration where
  type
    Rs CreateRecommenderConfiguration =
      CreateRecommenderConfigurationResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRecommenderConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders CreateRecommenderConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRecommenderConfiguration where
  toJSON CreateRecommenderConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "CreateRecommenderConfiguration"
                  Lude..= createRecommenderConfiguration
              )
          ]
      )

instance Lude.ToPath CreateRecommenderConfiguration where
  toPath = Lude.const "/v1/recommenders"

instance Lude.ToQuery CreateRecommenderConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRecommenderConfigurationResponse' smart constructor.
data CreateRecommenderConfigurationResponse = CreateRecommenderConfigurationResponse'
  { responseStatus ::
      Lude.Int,
    recommenderConfigurationResponse ::
      RecommenderConfigurationResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'recommenderConfigurationResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateRecommenderConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'recommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  CreateRecommenderConfigurationResponse
mkCreateRecommenderConfigurationResponse
  pResponseStatus_
  pRecommenderConfigurationResponse_ =
    CreateRecommenderConfigurationResponse'
      { responseStatus =
          pResponseStatus_,
        recommenderConfigurationResponse =
          pRecommenderConfigurationResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcrsResponseStatus :: Lens.Lens' CreateRecommenderConfigurationResponse Lude.Int
crcrsResponseStatus = Lens.lens (responseStatus :: CreateRecommenderConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRecommenderConfigurationResponse)
{-# DEPRECATED crcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcrsRecommenderConfigurationResponse :: Lens.Lens' CreateRecommenderConfigurationResponse RecommenderConfigurationResponse
crcrsRecommenderConfigurationResponse = Lens.lens (recommenderConfigurationResponse :: CreateRecommenderConfigurationResponse -> RecommenderConfigurationResponse) (\s a -> s {recommenderConfigurationResponse = a} :: CreateRecommenderConfigurationResponse)
{-# DEPRECATED crcrsRecommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead." #-}

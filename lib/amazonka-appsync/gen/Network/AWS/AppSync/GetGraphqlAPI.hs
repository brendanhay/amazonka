{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetGraphqlAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @GraphqlApi@ object.
module Network.AWS.AppSync.GetGraphqlAPI
  ( -- * Creating a request
    GetGraphqlAPI (..),
    mkGetGraphqlAPI,

    -- ** Request lenses
    ggaApiId,

    -- * Destructuring the response
    GetGraphqlAPIResponse (..),
    mkGetGraphqlAPIResponse,

    -- ** Response lenses
    ggarsGraphqlAPI,
    ggarsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGraphqlAPI' smart constructor.
newtype GetGraphqlAPI = GetGraphqlAPI' {apiId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGraphqlAPI' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID for the GraphQL API.
mkGetGraphqlAPI ::
  -- | 'apiId'
  Lude.Text ->
  GetGraphqlAPI
mkGetGraphqlAPI pApiId_ = GetGraphqlAPI' {apiId = pApiId_}

-- | The API ID for the GraphQL API.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggaApiId :: Lens.Lens' GetGraphqlAPI Lude.Text
ggaApiId = Lens.lens (apiId :: GetGraphqlAPI -> Lude.Text) (\s a -> s {apiId = a} :: GetGraphqlAPI)
{-# DEPRECATED ggaApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Lude.AWSRequest GetGraphqlAPI where
  type Rs GetGraphqlAPI = GetGraphqlAPIResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGraphqlAPIResponse'
            Lude.<$> (x Lude..?> "graphqlApi") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGraphqlAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetGraphqlAPI where
  toPath GetGraphqlAPI' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId]

instance Lude.ToQuery GetGraphqlAPI where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGraphqlAPIResponse' smart constructor.
data GetGraphqlAPIResponse = GetGraphqlAPIResponse'
  { graphqlAPI ::
      Lude.Maybe GraphqlAPI,
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

-- | Creates a value of 'GetGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- * 'graphqlAPI' - The @GraphqlApi@ object.
-- * 'responseStatus' - The response status code.
mkGetGraphqlAPIResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGraphqlAPIResponse
mkGetGraphqlAPIResponse pResponseStatus_ =
  GetGraphqlAPIResponse'
    { graphqlAPI = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @GraphqlApi@ object.
--
-- /Note:/ Consider using 'graphqlAPI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggarsGraphqlAPI :: Lens.Lens' GetGraphqlAPIResponse (Lude.Maybe GraphqlAPI)
ggarsGraphqlAPI = Lens.lens (graphqlAPI :: GetGraphqlAPIResponse -> Lude.Maybe GraphqlAPI) (\s a -> s {graphqlAPI = a} :: GetGraphqlAPIResponse)
{-# DEPRECATED ggarsGraphqlAPI "Use generic-lens or generic-optics with 'graphqlAPI' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggarsResponseStatus :: Lens.Lens' GetGraphqlAPIResponse Lude.Int
ggarsResponseStatus = Lens.lens (responseStatus :: GetGraphqlAPIResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGraphqlAPIResponse)
{-# DEPRECATED ggarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

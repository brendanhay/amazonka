{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetRecommenderConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the recommender model configurations that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.GetRecommenderConfigurations
  ( -- * Creating a request
    GetRecommenderConfigurations (..),
    mkGetRecommenderConfigurations,

    -- ** Request lenses
    grcToken,
    grcPageSize,

    -- * Destructuring the response
    GetRecommenderConfigurationsResponse (..),
    mkGetRecommenderConfigurationsResponse,

    -- ** Response lenses
    grcrsResponseStatus,
    grcrsListRecommenderConfigurationsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRecommenderConfigurations' smart constructor.
data GetRecommenderConfigurations = GetRecommenderConfigurations'
  { token ::
      Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRecommenderConfigurations' with the minimum fields required to make a request.
--
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
mkGetRecommenderConfigurations ::
  GetRecommenderConfigurations
mkGetRecommenderConfigurations =
  GetRecommenderConfigurations'
    { token = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcToken :: Lens.Lens' GetRecommenderConfigurations (Lude.Maybe Lude.Text)
grcToken = Lens.lens (token :: GetRecommenderConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetRecommenderConfigurations)
{-# DEPRECATED grcToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcPageSize :: Lens.Lens' GetRecommenderConfigurations (Lude.Maybe Lude.Text)
grcPageSize = Lens.lens (pageSize :: GetRecommenderConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetRecommenderConfigurations)
{-# DEPRECATED grcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetRecommenderConfigurations where
  type
    Rs GetRecommenderConfigurations =
      GetRecommenderConfigurationsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRecommenderConfigurationsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetRecommenderConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetRecommenderConfigurations where
  toPath = Lude.const "/v1/recommenders"

instance Lude.ToQuery GetRecommenderConfigurations where
  toQuery GetRecommenderConfigurations' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetRecommenderConfigurationsResponse' smart constructor.
data GetRecommenderConfigurationsResponse = GetRecommenderConfigurationsResponse'
  { responseStatus ::
      Lude.Int,
    listRecommenderConfigurationsResponse ::
      ListRecommenderConfigurationsResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRecommenderConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'listRecommenderConfigurationsResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetRecommenderConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'listRecommenderConfigurationsResponse'
  ListRecommenderConfigurationsResponse ->
  GetRecommenderConfigurationsResponse
mkGetRecommenderConfigurationsResponse
  pResponseStatus_
  pListRecommenderConfigurationsResponse_ =
    GetRecommenderConfigurationsResponse'
      { responseStatus =
          pResponseStatus_,
        listRecommenderConfigurationsResponse =
          pListRecommenderConfigurationsResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsResponseStatus :: Lens.Lens' GetRecommenderConfigurationsResponse Lude.Int
grcrsResponseStatus = Lens.lens (responseStatus :: GetRecommenderConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRecommenderConfigurationsResponse)
{-# DEPRECATED grcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'listRecommenderConfigurationsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsListRecommenderConfigurationsResponse :: Lens.Lens' GetRecommenderConfigurationsResponse ListRecommenderConfigurationsResponse
grcrsListRecommenderConfigurationsResponse = Lens.lens (listRecommenderConfigurationsResponse :: GetRecommenderConfigurationsResponse -> ListRecommenderConfigurationsResponse) (\s a -> s {listRecommenderConfigurationsResponse = a} :: GetRecommenderConfigurationsResponse)
{-# DEPRECATED grcrsListRecommenderConfigurationsResponse "Use generic-lens or generic-optics with 'listRecommenderConfigurationsResponse' instead." #-}

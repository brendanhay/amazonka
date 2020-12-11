{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the applications that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.GetApps
  ( -- * Creating a request
    GetApps (..),
    mkGetApps,

    -- ** Request lenses
    gaToken,
    gaPageSize,

    -- * Destructuring the response
    GetAppsResponse (..),
    mkGetAppsResponse,

    -- ** Response lenses
    gasrsResponseStatus,
    gasrsApplicationsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetApps' smart constructor.
data GetApps = GetApps'
  { token :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetApps' with the minimum fields required to make a request.
--
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
mkGetApps ::
  GetApps
mkGetApps = GetApps' {token = Lude.Nothing, pageSize = Lude.Nothing}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaToken :: Lens.Lens' GetApps (Lude.Maybe Lude.Text)
gaToken = Lens.lens (token :: GetApps -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetApps)
{-# DEPRECATED gaToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaPageSize :: Lens.Lens' GetApps (Lude.Maybe Lude.Text)
gaPageSize = Lens.lens (pageSize :: GetApps -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetApps)
{-# DEPRECATED gaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetApps where
  type Rs GetApps = GetAppsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetApps where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetApps where
  toPath = Lude.const "/v1/apps"

instance Lude.ToQuery GetApps where
  toQuery GetApps' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetAppsResponse' smart constructor.
data GetAppsResponse = GetAppsResponse'
  { responseStatus :: Lude.Int,
    applicationsResponse :: ApplicationsResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppsResponse' with the minimum fields required to make a request.
--
-- * 'applicationsResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetAppsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'applicationsResponse'
  ApplicationsResponse ->
  GetAppsResponse
mkGetAppsResponse pResponseStatus_ pApplicationsResponse_ =
  GetAppsResponse'
    { responseStatus = pResponseStatus_,
      applicationsResponse = pApplicationsResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResponseStatus :: Lens.Lens' GetAppsResponse Lude.Int
gasrsResponseStatus = Lens.lens (responseStatus :: GetAppsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppsResponse)
{-# DEPRECATED gasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsApplicationsResponse :: Lens.Lens' GetAppsResponse ApplicationsResponse
gasrsApplicationsResponse = Lens.lens (applicationsResponse :: GetAppsResponse -> ApplicationsResponse) (\s a -> s {applicationsResponse = a} :: GetAppsResponse)
{-# DEPRECATED gasrsApplicationsResponse "Use generic-lens or generic-optics with 'applicationsResponse' instead." #-}

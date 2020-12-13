{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListAppsLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @AppsListDataSummary@ objects.
module Network.AWS.FMS.ListAppsLists
  ( -- * Creating a request
    ListAppsLists (..),
    mkListAppsLists,

    -- ** Request lenses
    lalDefaultLists,
    lalNextToken,
    lalMaxResults,

    -- * Destructuring the response
    ListAppsListsResponse (..),
    mkListAppsListsResponse,

    -- ** Response lenses
    lalrsNextToken,
    lalrsAppsLists,
    lalrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAppsLists' smart constructor.
data ListAppsLists = ListAppsLists'
  { -- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
    defaultLists :: Lude.Maybe Lude.Bool,
    -- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
    --
    -- If you don't specify this, AWS Firewall Manager returns all available objects.
    maxResults :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAppsLists' with the minimum fields required to make a request.
--
-- * 'defaultLists' - Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
-- * 'nextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
-- * 'maxResults' - The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
mkListAppsLists ::
  -- | 'maxResults'
  Lude.Natural ->
  ListAppsLists
mkListAppsLists pMaxResults_ =
  ListAppsLists'
    { defaultLists = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = pMaxResults_
    }

-- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalDefaultLists :: Lens.Lens' ListAppsLists (Lude.Maybe Lude.Bool)
lalDefaultLists = Lens.lens (defaultLists :: ListAppsLists -> Lude.Maybe Lude.Bool) (\s a -> s {defaultLists = a} :: ListAppsLists)
{-# DEPRECATED lalDefaultLists "Use generic-lens or generic-optics with 'defaultLists' instead." #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalNextToken :: Lens.Lens' ListAppsLists (Lude.Maybe Lude.Text)
lalNextToken = Lens.lens (nextToken :: ListAppsLists -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAppsLists)
{-# DEPRECATED lalNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalMaxResults :: Lens.Lens' ListAppsLists Lude.Natural
lalMaxResults = Lens.lens (maxResults :: ListAppsLists -> Lude.Natural) (\s a -> s {maxResults = a} :: ListAppsLists)
{-# DEPRECATED lalMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListAppsLists where
  type Rs ListAppsLists = ListAppsListsResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAppsListsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AppsLists" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAppsLists where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.ListAppsLists" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAppsLists where
  toJSON ListAppsLists' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultLists" Lude..=) Lude.<$> defaultLists,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("MaxResults" Lude..= maxResults)
          ]
      )

instance Lude.ToPath ListAppsLists where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAppsLists where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAppsListsResponse' smart constructor.
data ListAppsListsResponse = ListAppsListsResponse'
  { -- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @AppsListDataSummary@ objects.
    appsLists :: Lude.Maybe [AppsListDataSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAppsListsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
-- * 'appsLists' - An array of @AppsListDataSummary@ objects.
-- * 'responseStatus' - The response status code.
mkListAppsListsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAppsListsResponse
mkListAppsListsResponse pResponseStatus_ =
  ListAppsListsResponse'
    { nextToken = Lude.Nothing,
      appsLists = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrsNextToken :: Lens.Lens' ListAppsListsResponse (Lude.Maybe Lude.Text)
lalrsNextToken = Lens.lens (nextToken :: ListAppsListsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAppsListsResponse)
{-# DEPRECATED lalrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @AppsListDataSummary@ objects.
--
-- /Note:/ Consider using 'appsLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrsAppsLists :: Lens.Lens' ListAppsListsResponse (Lude.Maybe [AppsListDataSummary])
lalrsAppsLists = Lens.lens (appsLists :: ListAppsListsResponse -> Lude.Maybe [AppsListDataSummary]) (\s a -> s {appsLists = a} :: ListAppsListsResponse)
{-# DEPRECATED lalrsAppsLists "Use generic-lens or generic-optics with 'appsLists' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrsResponseStatus :: Lens.Lens' ListAppsListsResponse Lude.Int
lalrsResponseStatus = Lens.lens (responseStatus :: ListAppsListsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAppsListsResponse)
{-# DEPRECATED lalrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

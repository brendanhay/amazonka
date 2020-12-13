{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTestGridProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all Selenium testing projects in your account.
module Network.AWS.DeviceFarm.ListTestGridProjects
  ( -- * Creating a request
    ListTestGridProjects (..),
    mkListTestGridProjects,

    -- ** Request lenses
    ltgpMaxResult,
    ltgpNextToken,

    -- * Destructuring the response
    ListTestGridProjectsResponse (..),
    mkListTestGridProjectsResponse,

    -- ** Response lenses
    ltgprsTestGridProjects,
    ltgprsNextToken,
    ltgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTestGridProjects' smart constructor.
data ListTestGridProjects = ListTestGridProjects'
  { -- | Return no more than this number of results.
    maxResult :: Lude.Maybe Lude.Natural,
    -- | From a response, used to continue a paginated listing.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTestGridProjects' with the minimum fields required to make a request.
--
-- * 'maxResult' - Return no more than this number of results.
-- * 'nextToken' - From a response, used to continue a paginated listing.
mkListTestGridProjects ::
  ListTestGridProjects
mkListTestGridProjects =
  ListTestGridProjects'
    { maxResult = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | Return no more than this number of results.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgpMaxResult :: Lens.Lens' ListTestGridProjects (Lude.Maybe Lude.Natural)
ltgpMaxResult = Lens.lens (maxResult :: ListTestGridProjects -> Lude.Maybe Lude.Natural) (\s a -> s {maxResult = a} :: ListTestGridProjects)
{-# DEPRECATED ltgpMaxResult "Use generic-lens or generic-optics with 'maxResult' instead." #-}

-- | From a response, used to continue a paginated listing.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgpNextToken :: Lens.Lens' ListTestGridProjects (Lude.Maybe Lude.Text)
ltgpNextToken = Lens.lens (nextToken :: ListTestGridProjects -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTestGridProjects)
{-# DEPRECATED ltgpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.AWSRequest ListTestGridProjects where
  type Rs ListTestGridProjects = ListTestGridProjectsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTestGridProjectsResponse'
            Lude.<$> (x Lude..?> "testGridProjects" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTestGridProjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListTestGridProjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTestGridProjects where
  toJSON ListTestGridProjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maxResult" Lude..=) Lude.<$> maxResult,
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListTestGridProjects where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTestGridProjects where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTestGridProjectsResponse' smart constructor.
data ListTestGridProjectsResponse = ListTestGridProjectsResponse'
  { -- | The list of TestGridProjects, based on a 'ListTestGridProjectsRequest' .
    testGridProjects :: Lude.Maybe [TestGridProject],
    -- | Used for pagination. Pass into 'ListTestGridProjects' to get more results in a paginated request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTestGridProjectsResponse' with the minimum fields required to make a request.
--
-- * 'testGridProjects' - The list of TestGridProjects, based on a 'ListTestGridProjectsRequest' .
-- * 'nextToken' - Used for pagination. Pass into 'ListTestGridProjects' to get more results in a paginated request.
-- * 'responseStatus' - The response status code.
mkListTestGridProjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTestGridProjectsResponse
mkListTestGridProjectsResponse pResponseStatus_ =
  ListTestGridProjectsResponse'
    { testGridProjects = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of TestGridProjects, based on a 'ListTestGridProjectsRequest' .
--
-- /Note:/ Consider using 'testGridProjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgprsTestGridProjects :: Lens.Lens' ListTestGridProjectsResponse (Lude.Maybe [TestGridProject])
ltgprsTestGridProjects = Lens.lens (testGridProjects :: ListTestGridProjectsResponse -> Lude.Maybe [TestGridProject]) (\s a -> s {testGridProjects = a} :: ListTestGridProjectsResponse)
{-# DEPRECATED ltgprsTestGridProjects "Use generic-lens or generic-optics with 'testGridProjects' instead." #-}

-- | Used for pagination. Pass into 'ListTestGridProjects' to get more results in a paginated request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgprsNextToken :: Lens.Lens' ListTestGridProjectsResponse (Lude.Maybe Lude.Text)
ltgprsNextToken = Lens.lens (nextToken :: ListTestGridProjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTestGridProjectsResponse)
{-# DEPRECATED ltgprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgprsResponseStatus :: Lens.Lens' ListTestGridProjectsResponse Lude.Int
ltgprsResponseStatus = Lens.lens (responseStatus :: ListTestGridProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTestGridProjectsResponse)
{-# DEPRECATED ltgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

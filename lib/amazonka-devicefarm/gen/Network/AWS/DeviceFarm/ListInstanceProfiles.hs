{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListInstanceProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all the instance profiles in an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListInstanceProfiles
  ( -- * Creating a request
    ListInstanceProfiles (..),
    mkListInstanceProfiles,

    -- ** Request lenses
    lipNextToken,
    lipMaxResults,

    -- * Destructuring the response
    ListInstanceProfilesResponse (..),
    mkListInstanceProfilesResponse,

    -- ** Response lenses
    liprsNextToken,
    liprsInstanceProfiles,
    liprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An integer that specifies the maximum number of items you want to return in the API response.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceProfiles' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'maxResults' - An integer that specifies the maximum number of items you want to return in the API response.
mkListInstanceProfiles ::
  ListInstanceProfiles
mkListInstanceProfiles =
  ListInstanceProfiles'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipNextToken :: Lens.Lens' ListInstanceProfiles (Lude.Maybe Lude.Text)
lipNextToken = Lens.lens (nextToken :: ListInstanceProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstanceProfiles)
{-# DEPRECATED lipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An integer that specifies the maximum number of items you want to return in the API response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxResults :: Lens.Lens' ListInstanceProfiles (Lude.Maybe Lude.Int)
lipMaxResults = Lens.lens (maxResults :: ListInstanceProfiles -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListInstanceProfiles)
{-# DEPRECATED lipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInstanceProfiles where
  page rq rs
    | Page.stop (rs Lens.^. liprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. liprsInstanceProfiles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lipNextToken Lens..~ rs Lens.^. liprsNextToken

instance Lude.AWSRequest ListInstanceProfiles where
  type Rs ListInstanceProfiles = ListInstanceProfilesResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstanceProfilesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "instanceProfiles" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstanceProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListInstanceProfiles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListInstanceProfiles where
  toJSON ListInstanceProfiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListInstanceProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInstanceProfiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
  { -- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An object that contains information about your instance profiles.
    instanceProfiles :: Lude.Maybe [InstanceProfile],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceProfilesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that can be used in the next call to this operation to return the next set of items in the list.
-- * 'instanceProfiles' - An object that contains information about your instance profiles.
-- * 'responseStatus' - The response status code.
mkListInstanceProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstanceProfilesResponse
mkListInstanceProfilesResponse pResponseStatus_ =
  ListInstanceProfilesResponse'
    { nextToken = Lude.Nothing,
      instanceProfiles = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsNextToken :: Lens.Lens' ListInstanceProfilesResponse (Lude.Maybe Lude.Text)
liprsNextToken = Lens.lens (nextToken :: ListInstanceProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstanceProfilesResponse)
{-# DEPRECATED liprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An object that contains information about your instance profiles.
--
-- /Note:/ Consider using 'instanceProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsInstanceProfiles :: Lens.Lens' ListInstanceProfilesResponse (Lude.Maybe [InstanceProfile])
liprsInstanceProfiles = Lens.lens (instanceProfiles :: ListInstanceProfilesResponse -> Lude.Maybe [InstanceProfile]) (\s a -> s {instanceProfiles = a} :: ListInstanceProfilesResponse)
{-# DEPRECATED liprsInstanceProfiles "Use generic-lens or generic-optics with 'instanceProfiles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsResponseStatus :: Lens.Lens' ListInstanceProfilesResponse Lude.Int
liprsResponseStatus = Lens.lens (responseStatus :: ListInstanceProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstanceProfilesResponse)
{-# DEPRECATED liprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

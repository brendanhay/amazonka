{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeDirectoryConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified Directory Config objects for AppStream 2.0, if the names for these objects are provided. Otherwise, all Directory Config objects in the account are described. These objects include the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
-- Although the response syntax in this topic includes the account password, this password is not returned in the actual response.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeDirectoryConfigs
  ( -- * Creating a request
    DescribeDirectoryConfigs (..),
    mkDescribeDirectoryConfigs,

    -- ** Request lenses
    ddcNextToken,
    ddcDirectoryNames,
    ddcMaxResults,

    -- * Destructuring the response
    DescribeDirectoryConfigsResponse (..),
    mkDescribeDirectoryConfigsResponse,

    -- ** Response lenses
    ddcrsNextToken,
    ddcrsDirectoryConfigs,
    ddcrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDirectoryConfigs' smart constructor.
data DescribeDirectoryConfigs = DescribeDirectoryConfigs'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The directory names.
    directoryNames :: Lude.Maybe [Lude.Text],
    -- | The maximum size of each page of results.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectoryConfigs' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'directoryNames' - The directory names.
-- * 'maxResults' - The maximum size of each page of results.
mkDescribeDirectoryConfigs ::
  DescribeDirectoryConfigs
mkDescribeDirectoryConfigs =
  DescribeDirectoryConfigs'
    { nextToken = Lude.Nothing,
      directoryNames = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcNextToken :: Lens.Lens' DescribeDirectoryConfigs (Lude.Maybe Lude.Text)
ddcNextToken = Lens.lens (nextToken :: DescribeDirectoryConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectoryConfigs)
{-# DEPRECATED ddcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The directory names.
--
-- /Note:/ Consider using 'directoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDirectoryNames :: Lens.Lens' DescribeDirectoryConfigs (Lude.Maybe [Lude.Text])
ddcDirectoryNames = Lens.lens (directoryNames :: DescribeDirectoryConfigs -> Lude.Maybe [Lude.Text]) (\s a -> s {directoryNames = a} :: DescribeDirectoryConfigs)
{-# DEPRECATED ddcDirectoryNames "Use generic-lens or generic-optics with 'directoryNames' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcMaxResults :: Lens.Lens' DescribeDirectoryConfigs (Lude.Maybe Lude.Int)
ddcMaxResults = Lens.lens (maxResults :: DescribeDirectoryConfigs -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeDirectoryConfigs)
{-# DEPRECATED ddcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeDirectoryConfigs where
  page rq rs
    | Page.stop (rs Lens.^. ddcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcrsDirectoryConfigs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcNextToken Lens..~ rs Lens.^. ddcrsNextToken

instance Lude.AWSRequest DescribeDirectoryConfigs where
  type Rs DescribeDirectoryConfigs = DescribeDirectoryConfigsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDirectoryConfigsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "DirectoryConfigs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDirectoryConfigs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DescribeDirectoryConfigs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDirectoryConfigs where
  toJSON DescribeDirectoryConfigs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("DirectoryNames" Lude..=) Lude.<$> directoryNames,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeDirectoryConfigs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDirectoryConfigs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDirectoryConfigsResponse' smart constructor.
data DescribeDirectoryConfigsResponse = DescribeDirectoryConfigsResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
    directoryConfigs :: Lude.Maybe [DirectoryConfig],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectoryConfigsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'directoryConfigs' - Information about the directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
-- * 'responseStatus' - The response status code.
mkDescribeDirectoryConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDirectoryConfigsResponse
mkDescribeDirectoryConfigsResponse pResponseStatus_ =
  DescribeDirectoryConfigsResponse'
    { nextToken = Lude.Nothing,
      directoryConfigs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsNextToken :: Lens.Lens' DescribeDirectoryConfigsResponse (Lude.Maybe Lude.Text)
ddcrsNextToken = Lens.lens (nextToken :: DescribeDirectoryConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectoryConfigsResponse)
{-# DEPRECATED ddcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
--
-- /Note:/ Consider using 'directoryConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDirectoryConfigs :: Lens.Lens' DescribeDirectoryConfigsResponse (Lude.Maybe [DirectoryConfig])
ddcrsDirectoryConfigs = Lens.lens (directoryConfigs :: DescribeDirectoryConfigsResponse -> Lude.Maybe [DirectoryConfig]) (\s a -> s {directoryConfigs = a} :: DescribeDirectoryConfigsResponse)
{-# DEPRECATED ddcrsDirectoryConfigs "Use generic-lens or generic-optics with 'directoryConfigs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsResponseStatus :: Lens.Lens' DescribeDirectoryConfigsResponse Lude.Int
ddcrsResponseStatus = Lens.lens (responseStatus :: DescribeDirectoryConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDirectoryConfigsResponse)
{-# DEPRECATED ddcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

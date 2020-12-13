{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstanceInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your instances, including information about the operating system platform, the version of SSM Agent installed on the instance, instance status, and so on.
--
-- If you specify one or more instance IDs, it returns information for those instances. If you do not specify instance IDs, it returns information for all your instances. If you specify an instance ID that is not valid or an instance that you do not own, you receive an error.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstanceInformation
  ( -- * Creating a request
    DescribeInstanceInformation (..),
    mkDescribeInstanceInformation,

    -- ** Request lenses
    diiInstanceInformationFilterList,
    diiFilters,
    diiNextToken,
    diiMaxResults,

    -- * Destructuring the response
    DescribeInstanceInformationResponse (..),
    mkDescribeInstanceInformationResponse,

    -- ** Response lenses
    diirsNextToken,
    diirsInstanceInformationList,
    diirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeInstanceInformation' smart constructor.
data DescribeInstanceInformation = DescribeInstanceInformation'
  { -- | This is a legacy method. We recommend that you don't use this method. Instead, use the @Filters@ data type. @Filters@ enables you to return instance information by filtering based on tags applied to managed instances.
    instanceInformationFilterList :: Lude.Maybe [InstanceInformationFilter],
    -- | One or more filters. Use a filter to return a more specific list of instances. You can filter based on tags applied to EC2 instances. Use this @Filters@ data type instead of @InstanceInformationFilterList@ , which is deprecated.
    filters :: Lude.Maybe [InstanceInformationStringFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceInformation' with the minimum fields required to make a request.
--
-- * 'instanceInformationFilterList' - This is a legacy method. We recommend that you don't use this method. Instead, use the @Filters@ data type. @Filters@ enables you to return instance information by filtering based on tags applied to managed instances.
-- * 'filters' - One or more filters. Use a filter to return a more specific list of instances. You can filter based on tags applied to EC2 instances. Use this @Filters@ data type instead of @InstanceInformationFilterList@ , which is deprecated.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeInstanceInformation ::
  DescribeInstanceInformation
mkDescribeInstanceInformation =
  DescribeInstanceInformation'
    { instanceInformationFilterList =
        Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | This is a legacy method. We recommend that you don't use this method. Instead, use the @Filters@ data type. @Filters@ enables you to return instance information by filtering based on tags applied to managed instances.
--
-- /Note:/ Consider using 'instanceInformationFilterList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiInstanceInformationFilterList :: Lens.Lens' DescribeInstanceInformation (Lude.Maybe [InstanceInformationFilter])
diiInstanceInformationFilterList = Lens.lens (instanceInformationFilterList :: DescribeInstanceInformation -> Lude.Maybe [InstanceInformationFilter]) (\s a -> s {instanceInformationFilterList = a} :: DescribeInstanceInformation)
{-# DEPRECATED diiInstanceInformationFilterList "Use generic-lens or generic-optics with 'instanceInformationFilterList' instead." #-}

-- | One or more filters. Use a filter to return a more specific list of instances. You can filter based on tags applied to EC2 instances. Use this @Filters@ data type instead of @InstanceInformationFilterList@ , which is deprecated.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiFilters :: Lens.Lens' DescribeInstanceInformation (Lude.Maybe [InstanceInformationStringFilter])
diiFilters = Lens.lens (filters :: DescribeInstanceInformation -> Lude.Maybe [InstanceInformationStringFilter]) (\s a -> s {filters = a} :: DescribeInstanceInformation)
{-# DEPRECATED diiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiNextToken :: Lens.Lens' DescribeInstanceInformation (Lude.Maybe Lude.Text)
diiNextToken = Lens.lens (nextToken :: DescribeInstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceInformation)
{-# DEPRECATED diiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiMaxResults :: Lens.Lens' DescribeInstanceInformation (Lude.Maybe Lude.Natural)
diiMaxResults = Lens.lens (maxResults :: DescribeInstanceInformation -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstanceInformation)
{-# DEPRECATED diiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstanceInformation where
  page rq rs
    | Page.stop (rs Lens.^. diirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. diirsInstanceInformationList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& diiNextToken Lens..~ rs Lens.^. diirsNextToken

instance Lude.AWSRequest DescribeInstanceInformation where
  type
    Rs DescribeInstanceInformation =
      DescribeInstanceInformationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstanceInformationResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "InstanceInformationList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceInformation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeInstanceInformation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInstanceInformation where
  toJSON DescribeInstanceInformation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceInformationFilterList" Lude..=)
              Lude.<$> instanceInformationFilterList,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeInstanceInformation where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceInformation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInstanceInformationResponse' smart constructor.
data DescribeInstanceInformationResponse = DescribeInstanceInformationResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The instance information list.
    instanceInformationList :: Lude.Maybe [InstanceInformation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceInformationResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'instanceInformationList' - The instance information list.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceInformationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceInformationResponse
mkDescribeInstanceInformationResponse pResponseStatus_ =
  DescribeInstanceInformationResponse'
    { nextToken = Lude.Nothing,
      instanceInformationList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsNextToken :: Lens.Lens' DescribeInstanceInformationResponse (Lude.Maybe Lude.Text)
diirsNextToken = Lens.lens (nextToken :: DescribeInstanceInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceInformationResponse)
{-# DEPRECATED diirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The instance information list.
--
-- /Note:/ Consider using 'instanceInformationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsInstanceInformationList :: Lens.Lens' DescribeInstanceInformationResponse (Lude.Maybe [InstanceInformation])
diirsInstanceInformationList = Lens.lens (instanceInformationList :: DescribeInstanceInformationResponse -> Lude.Maybe [InstanceInformation]) (\s a -> s {instanceInformationList = a} :: DescribeInstanceInformationResponse)
{-# DEPRECATED diirsInstanceInformationList "Use generic-lens or generic-optics with 'instanceInformationList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsResponseStatus :: Lens.Lens' DescribeInstanceInformationResponse Lude.Int
diirsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceInformationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceInformationResponse)
{-# DEPRECATED diirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

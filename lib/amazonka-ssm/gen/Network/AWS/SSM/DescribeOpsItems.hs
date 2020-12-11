{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeOpsItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query a set of OpsItems. You must have permission in AWS Identity and Access Management (IAM) to query a list of OpsItems. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeOpsItems
  ( -- * Creating a request
    DescribeOpsItems (..),
    mkDescribeOpsItems,

    -- ** Request lenses
    doiOpsItemFilters,
    doiNextToken,
    doiMaxResults,

    -- * Destructuring the response
    DescribeOpsItemsResponse (..),
    mkDescribeOpsItemsResponse,

    -- ** Response lenses
    doirsNextToken,
    doirsOpsItemSummaries,
    doirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeOpsItems' smart constructor.
data DescribeOpsItems = DescribeOpsItems'
  { opsItemFilters ::
      Lude.Maybe [OpsItemFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOpsItems' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
-- * 'opsItemFilters' - One or more filters to limit the response.
--
--
--     * Key: CreatedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: LastModifiedBy
-- Operations: Contains, Equals
--
--
--     * Key: LastModifiedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: Priority
-- Operations: Equals
--
--
--     * Key: Source
-- Operations: Contains, Equals
--
--
--     * Key: Status
-- Operations: Equals
--
--
--     * Key: Title
-- Operations: Contains
--
--
--     * Key: OperationalData*
-- Operations: Equals
--
--
--     * Key: OperationalDataKey
-- Operations: Equals
--
--
--     * Key: OperationalDataValue
-- Operations: Equals, Contains
--
--
--     * Key: OpsItemId
-- Operations: Equals
--
--
--     * Key: ResourceId
-- Operations: Contains
--
--
--     * Key: AutomationId
-- Operations: Equals
--
--
-- *If you filter the response by using the OperationalData operator, specify a key-value pair by using the following JSON format: {"key":"key_name","value":"a_value"}
mkDescribeOpsItems ::
  DescribeOpsItems
mkDescribeOpsItems =
  DescribeOpsItems'
    { opsItemFilters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters to limit the response.
--
--
--     * Key: CreatedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: LastModifiedBy
-- Operations: Contains, Equals
--
--
--     * Key: LastModifiedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: Priority
-- Operations: Equals
--
--
--     * Key: Source
-- Operations: Contains, Equals
--
--
--     * Key: Status
-- Operations: Equals
--
--
--     * Key: Title
-- Operations: Contains
--
--
--     * Key: OperationalData*
-- Operations: Equals
--
--
--     * Key: OperationalDataKey
-- Operations: Equals
--
--
--     * Key: OperationalDataValue
-- Operations: Equals, Contains
--
--
--     * Key: OpsItemId
-- Operations: Equals
--
--
--     * Key: ResourceId
-- Operations: Contains
--
--
--     * Key: AutomationId
-- Operations: Equals
--
--
-- *If you filter the response by using the OperationalData operator, specify a key-value pair by using the following JSON format: {"key":"key_name","value":"a_value"}
--
-- /Note:/ Consider using 'opsItemFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doiOpsItemFilters :: Lens.Lens' DescribeOpsItems (Lude.Maybe [OpsItemFilter])
doiOpsItemFilters = Lens.lens (opsItemFilters :: DescribeOpsItems -> Lude.Maybe [OpsItemFilter]) (\s a -> s {opsItemFilters = a} :: DescribeOpsItems)
{-# DEPRECATED doiOpsItemFilters "Use generic-lens or generic-optics with 'opsItemFilters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doiNextToken :: Lens.Lens' DescribeOpsItems (Lude.Maybe Lude.Text)
doiNextToken = Lens.lens (nextToken :: DescribeOpsItems -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOpsItems)
{-# DEPRECATED doiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doiMaxResults :: Lens.Lens' DescribeOpsItems (Lude.Maybe Lude.Natural)
doiMaxResults = Lens.lens (maxResults :: DescribeOpsItems -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeOpsItems)
{-# DEPRECATED doiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeOpsItems where
  page rq rs
    | Page.stop (rs Lens.^. doirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. doirsOpsItemSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& doiNextToken Lens..~ rs Lens.^. doirsNextToken

instance Lude.AWSRequest DescribeOpsItems where
  type Rs DescribeOpsItems = DescribeOpsItemsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOpsItemsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "OpsItemSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOpsItems where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeOpsItems" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOpsItems where
  toJSON DescribeOpsItems' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OpsItemFilters" Lude..=) Lude.<$> opsItemFilters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeOpsItems where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOpsItems where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOpsItemsResponse' smart constructor.
data DescribeOpsItemsResponse = DescribeOpsItemsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    opsItemSummaries ::
      Lude.Maybe [OpsItemSummary],
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

-- | Creates a value of 'DescribeOpsItemsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'opsItemSummaries' - A list of OpsItems.
-- * 'responseStatus' - The response status code.
mkDescribeOpsItemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOpsItemsResponse
mkDescribeOpsItemsResponse pResponseStatus_ =
  DescribeOpsItemsResponse'
    { nextToken = Lude.Nothing,
      opsItemSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doirsNextToken :: Lens.Lens' DescribeOpsItemsResponse (Lude.Maybe Lude.Text)
doirsNextToken = Lens.lens (nextToken :: DescribeOpsItemsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOpsItemsResponse)
{-# DEPRECATED doirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of OpsItems.
--
-- /Note:/ Consider using 'opsItemSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doirsOpsItemSummaries :: Lens.Lens' DescribeOpsItemsResponse (Lude.Maybe [OpsItemSummary])
doirsOpsItemSummaries = Lens.lens (opsItemSummaries :: DescribeOpsItemsResponse -> Lude.Maybe [OpsItemSummary]) (\s a -> s {opsItemSummaries = a} :: DescribeOpsItemsResponse)
{-# DEPRECATED doirsOpsItemSummaries "Use generic-lens or generic-optics with 'opsItemSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doirsResponseStatus :: Lens.Lens' DescribeOpsItemsResponse Lude.Int
doirsResponseStatus = Lens.lens (responseStatus :: DescribeOpsItemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOpsItemsResponse)
{-# DEPRECATED doirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

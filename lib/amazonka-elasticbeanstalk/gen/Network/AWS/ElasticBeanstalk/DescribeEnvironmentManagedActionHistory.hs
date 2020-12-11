{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment's completed and failed managed actions.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
  ( -- * Creating a request
    DescribeEnvironmentManagedActionHistory (..),
    mkDescribeEnvironmentManagedActionHistory,

    -- ** Request lenses
    demahNextToken,
    demahEnvironmentName,
    demahMaxItems,
    demahEnvironmentId,

    -- * Destructuring the response
    DescribeEnvironmentManagedActionHistoryResponse (..),
    mkDescribeEnvironmentManagedActionHistoryResponse,

    -- ** Response lenses
    demahrsManagedActionHistoryItems,
    demahrsNextToken,
    demahrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to list completed and failed managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActionHistory' smart constructor.
data DescribeEnvironmentManagedActionHistory = DescribeEnvironmentManagedActionHistory'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    environmentName ::
      Lude.Maybe
        Lude.Text,
    maxItems ::
      Lude.Maybe
        Lude.Natural,
    environmentId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentManagedActionHistory' with the minimum fields required to make a request.
--
-- * 'environmentId' - The environment ID of the target environment.
-- * 'environmentName' - The name of the target environment.
-- * 'maxItems' - The maximum number of items to return for a single request.
-- * 'nextToken' - The pagination token returned by a previous request.
mkDescribeEnvironmentManagedActionHistory ::
  DescribeEnvironmentManagedActionHistory
mkDescribeEnvironmentManagedActionHistory =
  DescribeEnvironmentManagedActionHistory'
    { nextToken =
        Lude.Nothing,
      environmentName = Lude.Nothing,
      maxItems = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | The pagination token returned by a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahNextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Lude.Maybe Lude.Text)
demahNextToken = Lens.lens (nextToken :: DescribeEnvironmentManagedActionHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEnvironmentManagedActionHistory)
{-# DEPRECATED demahNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the target environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahEnvironmentName :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Lude.Maybe Lude.Text)
demahEnvironmentName = Lens.lens (environmentName :: DescribeEnvironmentManagedActionHistory -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeEnvironmentManagedActionHistory)
{-# DEPRECATED demahEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The maximum number of items to return for a single request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahMaxItems :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Lude.Maybe Lude.Natural)
demahMaxItems = Lens.lens (maxItems :: DescribeEnvironmentManagedActionHistory -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: DescribeEnvironmentManagedActionHistory)
{-# DEPRECATED demahMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The environment ID of the target environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahEnvironmentId :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Lude.Maybe Lude.Text)
demahEnvironmentId = Lens.lens (environmentId :: DescribeEnvironmentManagedActionHistory -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeEnvironmentManagedActionHistory)
{-# DEPRECATED demahEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Page.AWSPager DescribeEnvironmentManagedActionHistory where
  page rq rs
    | Page.stop (rs Lens.^. demahrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. demahrsManagedActionHistoryItems) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& demahNextToken Lens..~ rs Lens.^. demahrsNextToken

instance Lude.AWSRequest DescribeEnvironmentManagedActionHistory where
  type
    Rs DescribeEnvironmentManagedActionHistory =
      DescribeEnvironmentManagedActionHistoryResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeEnvironmentManagedActionHistoryResult"
      ( \s h x ->
          DescribeEnvironmentManagedActionHistoryResponse'
            Lude.<$> ( x Lude..@? "ManagedActionHistoryItems" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLNonEmpty "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEnvironmentManagedActionHistory where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEnvironmentManagedActionHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironmentManagedActionHistory where
  toQuery DescribeEnvironmentManagedActionHistory' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeEnvironmentManagedActionHistory" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "EnvironmentName" Lude.=: environmentName,
        "MaxItems" Lude.=: maxItems,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | A result message containing a list of completed and failed managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActionHistoryResponse' smart constructor.
data DescribeEnvironmentManagedActionHistoryResponse = DescribeEnvironmentManagedActionHistoryResponse'
  { managedActionHistoryItems ::
      Lude.Maybe
        ( Lude.NonEmpty
            ManagedActionHistoryItem
        ),
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeEnvironmentManagedActionHistoryResponse' with the minimum fields required to make a request.
--
-- * 'managedActionHistoryItems' - A list of completed and failed managed actions.
-- * 'nextToken' - A pagination token that you pass to 'DescribeEnvironmentManagedActionHistory' to get the next page of results.
-- * 'responseStatus' - The response status code.
mkDescribeEnvironmentManagedActionHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEnvironmentManagedActionHistoryResponse
mkDescribeEnvironmentManagedActionHistoryResponse pResponseStatus_ =
  DescribeEnvironmentManagedActionHistoryResponse'
    { managedActionHistoryItems =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of completed and failed managed actions.
--
-- /Note:/ Consider using 'managedActionHistoryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahrsManagedActionHistoryItems :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Lude.Maybe (Lude.NonEmpty ManagedActionHistoryItem))
demahrsManagedActionHistoryItems = Lens.lens (managedActionHistoryItems :: DescribeEnvironmentManagedActionHistoryResponse -> Lude.Maybe (Lude.NonEmpty ManagedActionHistoryItem)) (\s a -> s {managedActionHistoryItems = a} :: DescribeEnvironmentManagedActionHistoryResponse)
{-# DEPRECATED demahrsManagedActionHistoryItems "Use generic-lens or generic-optics with 'managedActionHistoryItems' instead." #-}

-- | A pagination token that you pass to 'DescribeEnvironmentManagedActionHistory' to get the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahrsNextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Lude.Maybe Lude.Text)
demahrsNextToken = Lens.lens (nextToken :: DescribeEnvironmentManagedActionHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEnvironmentManagedActionHistoryResponse)
{-# DEPRECATED demahrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahrsResponseStatus :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse Lude.Int
demahrsResponseStatus = Lens.lens (responseStatus :: DescribeEnvironmentManagedActionHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEnvironmentManagedActionHistoryResponse)
{-# DEPRECATED demahrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackResourceDrifts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects configuration drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
-- Use 'DetectStackResourceDrift' to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all supported resources for a given stack.
module Network.AWS.CloudFormation.DescribeStackResourceDrifts
  ( -- * Creating a request
    DescribeStackResourceDrifts (..),
    mkDescribeStackResourceDrifts,

    -- ** Request lenses
    dsrdsNextToken,
    dsrdsMaxResults,
    dsrdsStackResourceDriftStatusFilters,
    dsrdsStackName,

    -- * Destructuring the response
    DescribeStackResourceDriftsResponse (..),
    mkDescribeStackResourceDriftsResponse,

    -- ** Response lenses
    dsrdrsNextToken,
    dsrdrsStackResourceDrifts,
    dsrdrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStackResourceDrifts' smart constructor.
data DescribeStackResourceDrifts = DescribeStackResourceDrifts'
  { -- | A string that identifies the next page of stack resource drift results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The resource drift status values to use as filters for the resource drift results returned.
    --
    --
    --     * @DELETED@ : The resource differs from its expected template configuration in that the resource has been deleted.
    --
    --
    --     * @MODIFIED@ : One or more resource properties differ from their expected template values.
    --
    --
    --     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
    stackResourceDriftStatusFilters :: Lude.Maybe (Lude.NonEmpty StackResourceDriftStatus),
    -- | The name of the stack for which you want drift information.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackResourceDrifts' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that identifies the next page of stack resource drift results.
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
-- * 'stackResourceDriftStatusFilters' - The resource drift status values to use as filters for the resource drift results returned.
--
--
--     * @DELETED@ : The resource differs from its expected template configuration in that the resource has been deleted.
--
--
--     * @MODIFIED@ : One or more resource properties differ from their expected template values.
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
--
--
-- * 'stackName' - The name of the stack for which you want drift information.
mkDescribeStackResourceDrifts ::
  -- | 'stackName'
  Lude.Text ->
  DescribeStackResourceDrifts
mkDescribeStackResourceDrifts pStackName_ =
  DescribeStackResourceDrifts'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      stackResourceDriftStatusFilters = Lude.Nothing,
      stackName = pStackName_
    }

-- | A string that identifies the next page of stack resource drift results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdsNextToken :: Lens.Lens' DescribeStackResourceDrifts (Lude.Maybe Lude.Text)
dsrdsNextToken = Lens.lens (nextToken :: DescribeStackResourceDrifts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStackResourceDrifts)
{-# DEPRECATED dsrdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdsMaxResults :: Lens.Lens' DescribeStackResourceDrifts (Lude.Maybe Lude.Natural)
dsrdsMaxResults = Lens.lens (maxResults :: DescribeStackResourceDrifts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeStackResourceDrifts)
{-# DEPRECATED dsrdsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The resource drift status values to use as filters for the resource drift results returned.
--
--
--     * @DELETED@ : The resource differs from its expected template configuration in that the resource has been deleted.
--
--
--     * @MODIFIED@ : One or more resource properties differ from their expected template values.
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
--
--
--
-- /Note:/ Consider using 'stackResourceDriftStatusFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdsStackResourceDriftStatusFilters :: Lens.Lens' DescribeStackResourceDrifts (Lude.Maybe (Lude.NonEmpty StackResourceDriftStatus))
dsrdsStackResourceDriftStatusFilters = Lens.lens (stackResourceDriftStatusFilters :: DescribeStackResourceDrifts -> Lude.Maybe (Lude.NonEmpty StackResourceDriftStatus)) (\s a -> s {stackResourceDriftStatusFilters = a} :: DescribeStackResourceDrifts)
{-# DEPRECATED dsrdsStackResourceDriftStatusFilters "Use generic-lens or generic-optics with 'stackResourceDriftStatusFilters' instead." #-}

-- | The name of the stack for which you want drift information.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdsStackName :: Lens.Lens' DescribeStackResourceDrifts Lude.Text
dsrdsStackName = Lens.lens (stackName :: DescribeStackResourceDrifts -> Lude.Text) (\s a -> s {stackName = a} :: DescribeStackResourceDrifts)
{-# DEPRECATED dsrdsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest DescribeStackResourceDrifts where
  type
    Rs DescribeStackResourceDrifts =
      DescribeStackResourceDriftsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStackResourceDriftsResult"
      ( \s h x ->
          DescribeStackResourceDriftsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "StackResourceDrifts" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackResourceDrifts where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStackResourceDrifts where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackResourceDrifts where
  toQuery DescribeStackResourceDrifts' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeStackResourceDrifts" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults,
        "StackResourceDriftStatusFilters"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "member"
                Lude.<$> stackResourceDriftStatusFilters
            ),
        "StackName" Lude.=: stackName
      ]

-- | /See:/ 'mkDescribeStackResourceDriftsResponse' smart constructor.
data DescribeStackResourceDriftsResponse = DescribeStackResourceDriftsResponse'
  { -- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @DescribeStackResourceDrifts@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | Drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects drift.
    --
    -- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
    stackResourceDrifts :: [StackResourceDrift],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackResourceDriftsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @DescribeStackResourceDrifts@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
-- * 'stackResourceDrifts' - Drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
-- * 'responseStatus' - The response status code.
mkDescribeStackResourceDriftsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackResourceDriftsResponse
mkDescribeStackResourceDriftsResponse pResponseStatus_ =
  DescribeStackResourceDriftsResponse'
    { nextToken = Lude.Nothing,
      stackResourceDrifts = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @DescribeStackResourceDrifts@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrsNextToken :: Lens.Lens' DescribeStackResourceDriftsResponse (Lude.Maybe Lude.Text)
dsrdrsNextToken = Lens.lens (nextToken :: DescribeStackResourceDriftsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStackResourceDriftsResponse)
{-# DEPRECATED dsrdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
-- /Note:/ Consider using 'stackResourceDrifts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrsStackResourceDrifts :: Lens.Lens' DescribeStackResourceDriftsResponse [StackResourceDrift]
dsrdrsStackResourceDrifts = Lens.lens (stackResourceDrifts :: DescribeStackResourceDriftsResponse -> [StackResourceDrift]) (\s a -> s {stackResourceDrifts = a} :: DescribeStackResourceDriftsResponse)
{-# DEPRECATED dsrdrsStackResourceDrifts "Use generic-lens or generic-optics with 'stackResourceDrifts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrsResponseStatus :: Lens.Lens' DescribeStackResourceDriftsResponse Lude.Int
dsrdrsResponseStatus = Lens.lens (responseStatus :: DescribeStackResourceDriftsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackResourceDriftsResponse)
{-# DEPRECATED dsrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

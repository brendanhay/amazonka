{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeIAMInstanceProfileAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IAM instance profile associations.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeIAMInstanceProfileAssociations
  ( -- * Creating a request
    DescribeIAMInstanceProfileAssociations (..),
    mkDescribeIAMInstanceProfileAssociations,

    -- ** Request lenses
    diapaFilters,
    diapaNextToken,
    diapaAssociationIds,
    diapaMaxResults,

    -- * Destructuring the response
    DescribeIAMInstanceProfileAssociationsResponse (..),
    mkDescribeIAMInstanceProfileAssociationsResponse,

    -- ** Response lenses
    diaparsIAMInstanceProfileAssociations,
    diaparsNextToken,
    diaparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeIAMInstanceProfileAssociations' smart constructor.
data DescribeIAMInstanceProfileAssociations = DescribeIAMInstanceProfileAssociations'
  { -- | The filters.
    --
    --
    --     * @instance-id@ - The ID of the instance.
    --
    --
    --     * @state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).
    filters :: Lude.Maybe [Filter],
    -- | The token to request the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The IAM instance profile associations.
    associationIds :: Lude.Maybe [Lude.Text],
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIAMInstanceProfileAssociations' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).
--
--
-- * 'nextToken' - The token to request the next page of results.
-- * 'associationIds' - The IAM instance profile associations.
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkDescribeIAMInstanceProfileAssociations ::
  DescribeIAMInstanceProfileAssociations
mkDescribeIAMInstanceProfileAssociations =
  DescribeIAMInstanceProfileAssociations'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      associationIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diapaFilters :: Lens.Lens' DescribeIAMInstanceProfileAssociations (Lude.Maybe [Filter])
diapaFilters = Lens.lens (filters :: DescribeIAMInstanceProfileAssociations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeIAMInstanceProfileAssociations)
{-# DEPRECATED diapaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diapaNextToken :: Lens.Lens' DescribeIAMInstanceProfileAssociations (Lude.Maybe Lude.Text)
diapaNextToken = Lens.lens (nextToken :: DescribeIAMInstanceProfileAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeIAMInstanceProfileAssociations)
{-# DEPRECATED diapaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IAM instance profile associations.
--
-- /Note:/ Consider using 'associationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diapaAssociationIds :: Lens.Lens' DescribeIAMInstanceProfileAssociations (Lude.Maybe [Lude.Text])
diapaAssociationIds = Lens.lens (associationIds :: DescribeIAMInstanceProfileAssociations -> Lude.Maybe [Lude.Text]) (\s a -> s {associationIds = a} :: DescribeIAMInstanceProfileAssociations)
{-# DEPRECATED diapaAssociationIds "Use generic-lens or generic-optics with 'associationIds' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diapaMaxResults :: Lens.Lens' DescribeIAMInstanceProfileAssociations (Lude.Maybe Lude.Natural)
diapaMaxResults = Lens.lens (maxResults :: DescribeIAMInstanceProfileAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeIAMInstanceProfileAssociations)
{-# DEPRECATED diapaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeIAMInstanceProfileAssociations where
  page rq rs
    | Page.stop (rs Lens.^. diaparsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. diaparsIAMInstanceProfileAssociations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& diapaNextToken Lens..~ rs Lens.^. diaparsNextToken

instance Lude.AWSRequest DescribeIAMInstanceProfileAssociations where
  type
    Rs DescribeIAMInstanceProfileAssociations =
      DescribeIAMInstanceProfileAssociationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeIAMInstanceProfileAssociationsResponse'
            Lude.<$> ( x Lude..@? "iamInstanceProfileAssociationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIAMInstanceProfileAssociations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeIAMInstanceProfileAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIAMInstanceProfileAssociations where
  toQuery DescribeIAMInstanceProfileAssociations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeIamInstanceProfileAssociations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "AssociationId" Lude.<$> associationIds),
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeIAMInstanceProfileAssociationsResponse' smart constructor.
data DescribeIAMInstanceProfileAssociationsResponse = DescribeIAMInstanceProfileAssociationsResponse'
  { -- | Information about the IAM instance profile associations.
    iamInstanceProfileAssociations :: Lude.Maybe [IAMInstanceProfileAssociation],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIAMInstanceProfileAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'iamInstanceProfileAssociations' - Information about the IAM instance profile associations.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeIAMInstanceProfileAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIAMInstanceProfileAssociationsResponse
mkDescribeIAMInstanceProfileAssociationsResponse pResponseStatus_ =
  DescribeIAMInstanceProfileAssociationsResponse'
    { iamInstanceProfileAssociations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the IAM instance profile associations.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaparsIAMInstanceProfileAssociations :: Lens.Lens' DescribeIAMInstanceProfileAssociationsResponse (Lude.Maybe [IAMInstanceProfileAssociation])
diaparsIAMInstanceProfileAssociations = Lens.lens (iamInstanceProfileAssociations :: DescribeIAMInstanceProfileAssociationsResponse -> Lude.Maybe [IAMInstanceProfileAssociation]) (\s a -> s {iamInstanceProfileAssociations = a} :: DescribeIAMInstanceProfileAssociationsResponse)
{-# DEPRECATED diaparsIAMInstanceProfileAssociations "Use generic-lens or generic-optics with 'iamInstanceProfileAssociations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaparsNextToken :: Lens.Lens' DescribeIAMInstanceProfileAssociationsResponse (Lude.Maybe Lude.Text)
diaparsNextToken = Lens.lens (nextToken :: DescribeIAMInstanceProfileAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeIAMInstanceProfileAssociationsResponse)
{-# DEPRECATED diaparsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaparsResponseStatus :: Lens.Lens' DescribeIAMInstanceProfileAssociationsResponse Lude.Int
diaparsResponseStatus = Lens.lens (responseStatus :: DescribeIAMInstanceProfileAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIAMInstanceProfileAssociationsResponse)
{-# DEPRECATED diaparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

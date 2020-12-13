{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstancesModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the modifications made to your Reserved Instances. If no parameter is specified, information about all your Reserved Instances modification requests is returned. If a modification ID is specified, only information about the specific modification is returned.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the Amazon Elastic Compute Cloud User Guide.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeReservedInstancesModifications
  ( -- * Creating a request
    DescribeReservedInstancesModifications (..),
    mkDescribeReservedInstancesModifications,

    -- ** Request lenses
    drimFilters,
    drimReservedInstancesModificationIds,
    drimNextToken,

    -- * Destructuring the response
    DescribeReservedInstancesModificationsResponse (..),
    mkDescribeReservedInstancesModificationsResponse,

    -- ** Response lenses
    drimrsNextToken,
    drimrsReservedInstancesModifications,
    drimrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeReservedInstancesModifications.
--
-- /See:/ 'mkDescribeReservedInstancesModifications' smart constructor.
data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications'
  { -- | One or more filters.
    --
    --
    --     * @client-token@ - The idempotency token for the modification request.
    --
    --
    --     * @create-date@ - The time when the modification request was created.
    --
    --
    --     * @effective-date@ - The time when the modification becomes effective.
    --
    --
    --     * @modification-result.reserved-instances-id@ - The ID for the Reserved Instances created as part of the modification request. This ID is only available when the status of the modification is @fulfilled@ .
    --
    --
    --     * @modification-result.target-configuration.availability-zone@ - The Availability Zone for the new Reserved Instances.
    --
    --
    --     * @modification-result.target-configuration.instance-count @ - The number of new Reserved Instances.
    --
    --
    --     * @modification-result.target-configuration.instance-type@ - The instance type of the new Reserved Instances.
    --
    --
    --     * @modification-result.target-configuration.platform@ - The network platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@ ).
    --
    --
    --     * @reserved-instances-id@ - The ID of the Reserved Instances modified.
    --
    --
    --     * @reserved-instances-modification-id@ - The ID of the modification request.
    --
    --
    --     * @status@ - The status of the Reserved Instances modification request (@processing@ | @fulfilled@ | @failed@ ).
    --
    --
    --     * @status-message@ - The reason for the status.
    --
    --
    --     * @update-date@ - The time when the modification request was last updated.
    filters :: Lude.Maybe [Filter],
    -- | IDs for the submitted modification request.
    reservedInstancesModificationIds :: Lude.Maybe [Lude.Text],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedInstancesModifications' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @client-token@ - The idempotency token for the modification request.
--
--
--     * @create-date@ - The time when the modification request was created.
--
--
--     * @effective-date@ - The time when the modification becomes effective.
--
--
--     * @modification-result.reserved-instances-id@ - The ID for the Reserved Instances created as part of the modification request. This ID is only available when the status of the modification is @fulfilled@ .
--
--
--     * @modification-result.target-configuration.availability-zone@ - The Availability Zone for the new Reserved Instances.
--
--
--     * @modification-result.target-configuration.instance-count @ - The number of new Reserved Instances.
--
--
--     * @modification-result.target-configuration.instance-type@ - The instance type of the new Reserved Instances.
--
--
--     * @modification-result.target-configuration.platform@ - The network platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@ ).
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instances modified.
--
--
--     * @reserved-instances-modification-id@ - The ID of the modification request.
--
--
--     * @status@ - The status of the Reserved Instances modification request (@processing@ | @fulfilled@ | @failed@ ).
--
--
--     * @status-message@ - The reason for the status.
--
--
--     * @update-date@ - The time when the modification request was last updated.
--
--
-- * 'reservedInstancesModificationIds' - IDs for the submitted modification request.
-- * 'nextToken' - The token to retrieve the next page of results.
mkDescribeReservedInstancesModifications ::
  DescribeReservedInstancesModifications
mkDescribeReservedInstancesModifications =
  DescribeReservedInstancesModifications'
    { filters = Lude.Nothing,
      reservedInstancesModificationIds = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @client-token@ - The idempotency token for the modification request.
--
--
--     * @create-date@ - The time when the modification request was created.
--
--
--     * @effective-date@ - The time when the modification becomes effective.
--
--
--     * @modification-result.reserved-instances-id@ - The ID for the Reserved Instances created as part of the modification request. This ID is only available when the status of the modification is @fulfilled@ .
--
--
--     * @modification-result.target-configuration.availability-zone@ - The Availability Zone for the new Reserved Instances.
--
--
--     * @modification-result.target-configuration.instance-count @ - The number of new Reserved Instances.
--
--
--     * @modification-result.target-configuration.instance-type@ - The instance type of the new Reserved Instances.
--
--
--     * @modification-result.target-configuration.platform@ - The network platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@ ).
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instances modified.
--
--
--     * @reserved-instances-modification-id@ - The ID of the modification request.
--
--
--     * @status@ - The status of the Reserved Instances modification request (@processing@ | @fulfilled@ | @failed@ ).
--
--
--     * @status-message@ - The reason for the status.
--
--
--     * @update-date@ - The time when the modification request was last updated.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drimFilters :: Lens.Lens' DescribeReservedInstancesModifications (Lude.Maybe [Filter])
drimFilters = Lens.lens (filters :: DescribeReservedInstancesModifications -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReservedInstancesModifications)
{-# DEPRECATED drimFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | IDs for the submitted modification request.
--
-- /Note:/ Consider using 'reservedInstancesModificationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drimReservedInstancesModificationIds :: Lens.Lens' DescribeReservedInstancesModifications (Lude.Maybe [Lude.Text])
drimReservedInstancesModificationIds = Lens.lens (reservedInstancesModificationIds :: DescribeReservedInstancesModifications -> Lude.Maybe [Lude.Text]) (\s a -> s {reservedInstancesModificationIds = a} :: DescribeReservedInstancesModifications)
{-# DEPRECATED drimReservedInstancesModificationIds "Use generic-lens or generic-optics with 'reservedInstancesModificationIds' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drimNextToken :: Lens.Lens' DescribeReservedInstancesModifications (Lude.Maybe Lude.Text)
drimNextToken = Lens.lens (nextToken :: DescribeReservedInstancesModifications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedInstancesModifications)
{-# DEPRECATED drimNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeReservedInstancesModifications where
  page rq rs
    | Page.stop (rs Lens.^. drimrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drimrsReservedInstancesModifications) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drimNextToken Lens..~ rs Lens.^. drimrsNextToken

instance Lude.AWSRequest DescribeReservedInstancesModifications where
  type
    Rs DescribeReservedInstancesModifications =
      DescribeReservedInstancesModificationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeReservedInstancesModificationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "reservedInstancesModificationsSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedInstancesModifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedInstancesModifications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedInstancesModifications where
  toQuery DescribeReservedInstancesModifications' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedInstancesModifications" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          ( Lude.toQueryList "ReservedInstancesModificationId"
              Lude.<$> reservedInstancesModificationIds
          ),
        "NextToken" Lude.=: nextToken
      ]

-- | Contains the output of DescribeReservedInstancesModifications.
--
-- /See:/ 'mkDescribeReservedInstancesModificationsResponse' smart constructor.
data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Reserved Instance modification information.
    reservedInstancesModifications :: Lude.Maybe [ReservedInstancesModification],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedInstancesModificationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'reservedInstancesModifications' - The Reserved Instance modification information.
-- * 'responseStatus' - The response status code.
mkDescribeReservedInstancesModificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedInstancesModificationsResponse
mkDescribeReservedInstancesModificationsResponse pResponseStatus_ =
  DescribeReservedInstancesModificationsResponse'
    { nextToken =
        Lude.Nothing,
      reservedInstancesModifications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drimrsNextToken :: Lens.Lens' DescribeReservedInstancesModificationsResponse (Lude.Maybe Lude.Text)
drimrsNextToken = Lens.lens (nextToken :: DescribeReservedInstancesModificationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedInstancesModificationsResponse)
{-# DEPRECATED drimrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Reserved Instance modification information.
--
-- /Note:/ Consider using 'reservedInstancesModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drimrsReservedInstancesModifications :: Lens.Lens' DescribeReservedInstancesModificationsResponse (Lude.Maybe [ReservedInstancesModification])
drimrsReservedInstancesModifications = Lens.lens (reservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> Lude.Maybe [ReservedInstancesModification]) (\s a -> s {reservedInstancesModifications = a} :: DescribeReservedInstancesModificationsResponse)
{-# DEPRECATED drimrsReservedInstancesModifications "Use generic-lens or generic-optics with 'reservedInstancesModifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drimrsResponseStatus :: Lens.Lens' DescribeReservedInstancesModificationsResponse Lude.Int
drimrsResponseStatus = Lens.lens (responseStatus :: DescribeReservedInstancesModificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedInstancesModificationsResponse)
{-# DEPRECATED drimrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

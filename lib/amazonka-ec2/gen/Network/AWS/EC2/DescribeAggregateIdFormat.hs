{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAggregateIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the longer ID format settings for all resource types in a specific Region. This request is useful for performing a quick audit to determine whether a specific Region is fully opted in for longer IDs (17-character IDs).
--
-- This request only returns information about resource types that support longer IDs.
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
module Network.AWS.EC2.DescribeAggregateIdFormat
  ( -- * Creating a request
    DescribeAggregateIdFormat (..),
    mkDescribeAggregateIdFormat,

    -- ** Request lenses
    daifDryRun,

    -- * Destructuring the response
    DescribeAggregateIdFormatResponse (..),
    mkDescribeAggregateIdFormatResponse,

    -- ** Response lenses
    daifrsUseLongIdsAggregated,
    daifrsStatuses,
    daifrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAggregateIdFormat' smart constructor.
newtype DescribeAggregateIdFormat = DescribeAggregateIdFormat'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAggregateIdFormat' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeAggregateIdFormat ::
  DescribeAggregateIdFormat
mkDescribeAggregateIdFormat =
  DescribeAggregateIdFormat' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifDryRun :: Lens.Lens' DescribeAggregateIdFormat (Lude.Maybe Lude.Bool)
daifDryRun = Lens.lens (dryRun :: DescribeAggregateIdFormat -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeAggregateIdFormat)
{-# DEPRECATED daifDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeAggregateIdFormat where
  type
    Rs DescribeAggregateIdFormat =
      DescribeAggregateIdFormatResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeAggregateIdFormatResponse'
            Lude.<$> (x Lude..@? "useLongIdsAggregated")
            Lude.<*> ( x Lude..@? "statusSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAggregateIdFormat where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAggregateIdFormat where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAggregateIdFormat where
  toQuery DescribeAggregateIdFormat' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAggregateIdFormat" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeAggregateIdFormatResponse' smart constructor.
data DescribeAggregateIdFormatResponse = DescribeAggregateIdFormatResponse'
  { -- | Indicates whether all resource types in the Region are configured to use longer IDs. This value is only @true@ if all users are configured to use longer IDs for all resources types in the Region.
    useLongIdsAggregated :: Lude.Maybe Lude.Bool,
    -- | Information about each resource's ID format.
    statuses :: Lude.Maybe [IdFormat],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAggregateIdFormatResponse' with the minimum fields required to make a request.
--
-- * 'useLongIdsAggregated' - Indicates whether all resource types in the Region are configured to use longer IDs. This value is only @true@ if all users are configured to use longer IDs for all resources types in the Region.
-- * 'statuses' - Information about each resource's ID format.
-- * 'responseStatus' - The response status code.
mkDescribeAggregateIdFormatResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAggregateIdFormatResponse
mkDescribeAggregateIdFormatResponse pResponseStatus_ =
  DescribeAggregateIdFormatResponse'
    { useLongIdsAggregated =
        Lude.Nothing,
      statuses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether all resource types in the Region are configured to use longer IDs. This value is only @true@ if all users are configured to use longer IDs for all resources types in the Region.
--
-- /Note:/ Consider using 'useLongIdsAggregated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifrsUseLongIdsAggregated :: Lens.Lens' DescribeAggregateIdFormatResponse (Lude.Maybe Lude.Bool)
daifrsUseLongIdsAggregated = Lens.lens (useLongIdsAggregated :: DescribeAggregateIdFormatResponse -> Lude.Maybe Lude.Bool) (\s a -> s {useLongIdsAggregated = a} :: DescribeAggregateIdFormatResponse)
{-# DEPRECATED daifrsUseLongIdsAggregated "Use generic-lens or generic-optics with 'useLongIdsAggregated' instead." #-}

-- | Information about each resource's ID format.
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifrsStatuses :: Lens.Lens' DescribeAggregateIdFormatResponse (Lude.Maybe [IdFormat])
daifrsStatuses = Lens.lens (statuses :: DescribeAggregateIdFormatResponse -> Lude.Maybe [IdFormat]) (\s a -> s {statuses = a} :: DescribeAggregateIdFormatResponse)
{-# DEPRECATED daifrsStatuses "Use generic-lens or generic-optics with 'statuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifrsResponseStatus :: Lens.Lens' DescribeAggregateIdFormatResponse Lude.Int
daifrsResponseStatus = Lens.lens (responseStatus :: DescribeAggregateIdFormatResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAggregateIdFormatResponse)
{-# DEPRECATED daifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

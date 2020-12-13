{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for your resources on a per-Region basis, for example, to view which resource types are enabled for longer IDs. This request only returns information about resource types whose ID formats can be modified; it does not return information about other resource types.
--
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
-- These settings apply to the IAM user who makes the request; they do not apply to the entire AWS account. By default, an IAM user defaults to the same settings as the root user, unless they explicitly override the settings by running the 'ModifyIdFormat' command. Resources created with longer IDs are visible to all IAM users, regardless of these settings and provided that they have permission to use the relevant @Describe@ command for the resource type.
module Network.AWS.EC2.DescribeIdFormat
  ( -- * Creating a request
    DescribeIdFormat (..),
    mkDescribeIdFormat,

    -- ** Request lenses
    difResource,

    -- * Destructuring the response
    DescribeIdFormatResponse (..),
    mkDescribeIdFormatResponse,

    -- ** Response lenses
    difrsStatuses,
    difrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeIdFormat' smart constructor.
newtype DescribeIdFormat = DescribeIdFormat'
  { -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
    resource :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdFormat' with the minimum fields required to make a request.
--
-- * 'resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
mkDescribeIdFormat ::
  DescribeIdFormat
mkDescribeIdFormat = DescribeIdFormat' {resource = Lude.Nothing}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difResource :: Lens.Lens' DescribeIdFormat (Lude.Maybe Lude.Text)
difResource = Lens.lens (resource :: DescribeIdFormat -> Lude.Maybe Lude.Text) (\s a -> s {resource = a} :: DescribeIdFormat)
{-# DEPRECATED difResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.AWSRequest DescribeIdFormat where
  type Rs DescribeIdFormat = DescribeIdFormatResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeIdFormatResponse'
            Lude.<$> ( x Lude..@? "statusSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIdFormat where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeIdFormat where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIdFormat where
  toQuery DescribeIdFormat' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeIdFormat" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Resource" Lude.=: resource
      ]

-- | /See:/ 'mkDescribeIdFormatResponse' smart constructor.
data DescribeIdFormatResponse = DescribeIdFormatResponse'
  { -- | Information about the ID format for the resource.
    statuses :: Lude.Maybe [IdFormat],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdFormatResponse' with the minimum fields required to make a request.
--
-- * 'statuses' - Information about the ID format for the resource.
-- * 'responseStatus' - The response status code.
mkDescribeIdFormatResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIdFormatResponse
mkDescribeIdFormatResponse pResponseStatus_ =
  DescribeIdFormatResponse'
    { statuses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the ID format for the resource.
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsStatuses :: Lens.Lens' DescribeIdFormatResponse (Lude.Maybe [IdFormat])
difrsStatuses = Lens.lens (statuses :: DescribeIdFormatResponse -> Lude.Maybe [IdFormat]) (\s a -> s {statuses = a} :: DescribeIdFormatResponse)
{-# DEPRECATED difrsStatuses "Use generic-lens or generic-optics with 'statuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsResponseStatus :: Lens.Lens' DescribeIdFormatResponse Lude.Int
difrsResponseStatus = Lens.lens (responseStatus :: DescribeIdFormatResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIdFormatResponse)
{-# DEPRECATED difrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

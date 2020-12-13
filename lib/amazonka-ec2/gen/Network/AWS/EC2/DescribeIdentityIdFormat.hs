{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeIdentityIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for resources for the specified IAM user, IAM role, or root user. For example, you can view the resource types that are enabled for longer IDs. This request only returns information about resource types whose ID formats can be modified; it does not return information about other resource types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
-- These settings apply to the principal specified in the request. They do not apply to the principal that makes the request.
module Network.AWS.EC2.DescribeIdentityIdFormat
  ( -- * Creating a request
    DescribeIdentityIdFormat (..),
    mkDescribeIdentityIdFormat,

    -- ** Request lenses
    diifPrincipalARN,
    diifResource,

    -- * Destructuring the response
    DescribeIdentityIdFormatResponse (..),
    mkDescribeIdentityIdFormatResponse,

    -- ** Response lenses
    diifrsStatuses,
    diifrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeIdentityIdFormat' smart constructor.
data DescribeIdentityIdFormat = DescribeIdentityIdFormat'
  { -- | The ARN of the principal, which can be an IAM role, IAM user, or the root user.
    principalARN :: Lude.Text,
    -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
    resource :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityIdFormat' with the minimum fields required to make a request.
--
-- * 'principalARN' - The ARN of the principal, which can be an IAM role, IAM user, or the root user.
-- * 'resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
mkDescribeIdentityIdFormat ::
  -- | 'principalARN'
  Lude.Text ->
  DescribeIdentityIdFormat
mkDescribeIdentityIdFormat pPrincipalARN_ =
  DescribeIdentityIdFormat'
    { principalARN = pPrincipalARN_,
      resource = Lude.Nothing
    }

-- | The ARN of the principal, which can be an IAM role, IAM user, or the root user.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifPrincipalARN :: Lens.Lens' DescribeIdentityIdFormat Lude.Text
diifPrincipalARN = Lens.lens (principalARN :: DescribeIdentityIdFormat -> Lude.Text) (\s a -> s {principalARN = a} :: DescribeIdentityIdFormat)
{-# DEPRECATED diifPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifResource :: Lens.Lens' DescribeIdentityIdFormat (Lude.Maybe Lude.Text)
diifResource = Lens.lens (resource :: DescribeIdentityIdFormat -> Lude.Maybe Lude.Text) (\s a -> s {resource = a} :: DescribeIdentityIdFormat)
{-# DEPRECATED diifResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.AWSRequest DescribeIdentityIdFormat where
  type Rs DescribeIdentityIdFormat = DescribeIdentityIdFormatResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeIdentityIdFormatResponse'
            Lude.<$> ( x Lude..@? "statusSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIdentityIdFormat where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeIdentityIdFormat where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIdentityIdFormat where
  toQuery DescribeIdentityIdFormat' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeIdentityIdFormat" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "PrincipalArn" Lude.=: principalARN,
        "Resource" Lude.=: resource
      ]

-- | /See:/ 'mkDescribeIdentityIdFormatResponse' smart constructor.
data DescribeIdentityIdFormatResponse = DescribeIdentityIdFormatResponse'
  { -- | Information about the ID format for the resources.
    statuses :: Lude.Maybe [IdFormat],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityIdFormatResponse' with the minimum fields required to make a request.
--
-- * 'statuses' - Information about the ID format for the resources.
-- * 'responseStatus' - The response status code.
mkDescribeIdentityIdFormatResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIdentityIdFormatResponse
mkDescribeIdentityIdFormatResponse pResponseStatus_ =
  DescribeIdentityIdFormatResponse'
    { statuses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the ID format for the resources.
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifrsStatuses :: Lens.Lens' DescribeIdentityIdFormatResponse (Lude.Maybe [IdFormat])
diifrsStatuses = Lens.lens (statuses :: DescribeIdentityIdFormatResponse -> Lude.Maybe [IdFormat]) (\s a -> s {statuses = a} :: DescribeIdentityIdFormatResponse)
{-# DEPRECATED diifrsStatuses "Use generic-lens or generic-optics with 'statuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifrsResponseStatus :: Lens.Lens' DescribeIdentityIdFormatResponse Lude.Int
diifrsResponseStatus = Lens.lens (responseStatus :: DescribeIdentityIdFormatResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIdentityIdFormatResponse)
{-# DEPRECATED diifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

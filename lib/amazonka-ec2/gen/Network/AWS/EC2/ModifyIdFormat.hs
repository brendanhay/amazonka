{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ID format for the specified resource on a per-Region basis. You can specify that resources should receive longer IDs (17-character IDs) when they are created.
--
-- This request can only be used to modify longer ID settings for resource types that are within the opt-in period. Resources currently in their opt-in period include: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
-- This setting applies to the IAM user who makes the request; it does not apply to the entire AWS account. By default, an IAM user defaults to the same settings as the root user. If you're using this action as the root user, then these settings apply to the entire account, unless an IAM user explicitly overrides these settings for themselves. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs> in the /Amazon Elastic Compute Cloud User Guide/ .
-- Resources created with longer IDs are visible to all IAM roles and users, regardless of these settings and provided that they have permission to use the relevant @Describe@ command for the resource type.
module Network.AWS.EC2.ModifyIdFormat
  ( -- * Creating a request
    ModifyIdFormat (..),
    mkModifyIdFormat,

    -- ** Request lenses
    mifResource,
    mifUseLongIds,

    -- * Destructuring the response
    ModifyIdFormatResponse (..),
    mkModifyIdFormatResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyIdFormat' smart constructor.
data ModifyIdFormat = ModifyIdFormat'
  { resource :: Lude.Text,
    useLongIds :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyIdFormat' with the minimum fields required to make a request.
--
-- * 'resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
-- * 'useLongIds' - Indicate whether the resource should use longer IDs (17-character IDs).
mkModifyIdFormat ::
  -- | 'resource'
  Lude.Text ->
  -- | 'useLongIds'
  Lude.Bool ->
  ModifyIdFormat
mkModifyIdFormat pResource_ pUseLongIds_ =
  ModifyIdFormat' {resource = pResource_, useLongIds = pUseLongIds_}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mifResource :: Lens.Lens' ModifyIdFormat Lude.Text
mifResource = Lens.lens (resource :: ModifyIdFormat -> Lude.Text) (\s a -> s {resource = a} :: ModifyIdFormat)
{-# DEPRECATED mifResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | Indicate whether the resource should use longer IDs (17-character IDs).
--
-- /Note:/ Consider using 'useLongIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mifUseLongIds :: Lens.Lens' ModifyIdFormat Lude.Bool
mifUseLongIds = Lens.lens (useLongIds :: ModifyIdFormat -> Lude.Bool) (\s a -> s {useLongIds = a} :: ModifyIdFormat)
{-# DEPRECATED mifUseLongIds "Use generic-lens or generic-optics with 'useLongIds' instead." #-}

instance Lude.AWSRequest ModifyIdFormat where
  type Rs ModifyIdFormat = ModifyIdFormatResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifyIdFormatResponse'

instance Lude.ToHeaders ModifyIdFormat where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyIdFormat where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyIdFormat where
  toQuery ModifyIdFormat' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyIdFormat" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Resource" Lude.=: resource,
        "UseLongIds" Lude.=: useLongIds
      ]

-- | /See:/ 'mkModifyIdFormatResponse' smart constructor.
data ModifyIdFormatResponse = ModifyIdFormatResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyIdFormatResponse' with the minimum fields required to make a request.
mkModifyIdFormatResponse ::
  ModifyIdFormatResponse
mkModifyIdFormatResponse = ModifyIdFormatResponse'

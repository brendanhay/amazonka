{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyIdentityIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ID format of a resource for a specified IAM user, IAM role, or the root user for an account; or all IAM users, IAM roles, and the root user for an account. You can specify that resources should receive longer IDs (17-character IDs) when they are created.
--
-- This request can only be used to modify longer ID settings for resource types that are within the opt-in period. Resources currently in their opt-in period include: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs> in the /Amazon Elastic Compute Cloud User Guide/ .
-- This setting applies to the principal specified in the request; it does not apply to the principal that makes the request.
-- Resources created with longer IDs are visible to all IAM roles and users, regardless of these settings and provided that they have permission to use the relevant @Describe@ command for the resource type.
module Network.AWS.EC2.ModifyIdentityIdFormat
  ( -- * Creating a request
    ModifyIdentityIdFormat (..),
    mkModifyIdentityIdFormat,

    -- ** Request lenses
    miifUseLongIds,
    miifPrincipalARN,
    miifResource,

    -- * Destructuring the response
    ModifyIdentityIdFormatResponse (..),
    mkModifyIdentityIdFormatResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyIdentityIdFormat' smart constructor.
data ModifyIdentityIdFormat = ModifyIdentityIdFormat'
  { -- | Indicates whether the resource should use longer IDs (17-character IDs)
    useLongIds :: Lude.Bool,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. Specify @all@ to modify the ID format for all IAM users, IAM roles, and the root user of the account.
    principalARN :: Lude.Text,
    -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
    --
    -- Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyIdentityIdFormat' with the minimum fields required to make a request.
--
-- * 'useLongIds' - Indicates whether the resource should use longer IDs (17-character IDs)
-- * 'principalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. Specify @all@ to modify the ID format for all IAM users, IAM roles, and the root user of the account.
-- * 'resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
mkModifyIdentityIdFormat ::
  -- | 'useLongIds'
  Lude.Bool ->
  -- | 'principalARN'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  ModifyIdentityIdFormat
mkModifyIdentityIdFormat pUseLongIds_ pPrincipalARN_ pResource_ =
  ModifyIdentityIdFormat'
    { useLongIds = pUseLongIds_,
      principalARN = pPrincipalARN_,
      resource = pResource_
    }

-- | Indicates whether the resource should use longer IDs (17-character IDs)
--
-- /Note:/ Consider using 'useLongIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifUseLongIds :: Lens.Lens' ModifyIdentityIdFormat Lude.Bool
miifUseLongIds = Lens.lens (useLongIds :: ModifyIdentityIdFormat -> Lude.Bool) (\s a -> s {useLongIds = a} :: ModifyIdentityIdFormat)
{-# DEPRECATED miifUseLongIds "Use generic-lens or generic-optics with 'useLongIds' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. Specify @all@ to modify the ID format for all IAM users, IAM roles, and the root user of the account.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifPrincipalARN :: Lens.Lens' ModifyIdentityIdFormat Lude.Text
miifPrincipalARN = Lens.lens (principalARN :: ModifyIdentityIdFormat -> Lude.Text) (\s a -> s {principalARN = a} :: ModifyIdentityIdFormat)
{-# DEPRECATED miifPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifResource :: Lens.Lens' ModifyIdentityIdFormat Lude.Text
miifResource = Lens.lens (resource :: ModifyIdentityIdFormat -> Lude.Text) (\s a -> s {resource = a} :: ModifyIdentityIdFormat)
{-# DEPRECATED miifResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.AWSRequest ModifyIdentityIdFormat where
  type Rs ModifyIdentityIdFormat = ModifyIdentityIdFormatResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifyIdentityIdFormatResponse'

instance Lude.ToHeaders ModifyIdentityIdFormat where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyIdentityIdFormat where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyIdentityIdFormat where
  toQuery ModifyIdentityIdFormat' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyIdentityIdFormat" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "UseLongIds" Lude.=: useLongIds,
        "PrincipalArn" Lude.=: principalARN,
        "Resource" Lude.=: resource
      ]

-- | /See:/ 'mkModifyIdentityIdFormatResponse' smart constructor.
data ModifyIdentityIdFormatResponse = ModifyIdentityIdFormatResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyIdentityIdFormatResponse' with the minimum fields required to make a request.
mkModifyIdentityIdFormatResponse ::
  ModifyIdentityIdFormatResponse
mkModifyIdentityIdFormatResponse = ModifyIdentityIdFormatResponse'

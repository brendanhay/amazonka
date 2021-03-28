{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyIdentityIdFormat (..)
    , mkModifyIdentityIdFormat
    -- ** Request lenses
    , miifPrincipalArn
    , miifResource
    , miifUseLongIds

    -- * Destructuring the response
    , ModifyIdentityIdFormatResponse (..)
    , mkModifyIdentityIdFormatResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyIdentityIdFormat' smart constructor.
data ModifyIdentityIdFormat = ModifyIdentityIdFormat'
  { principalArn :: Core.Text
    -- ^ The ARN of the principal, which can be an IAM user, IAM role, or the root user. Specify @all@ to modify the ID format for all IAM users, IAM roles, and the root user of the account.
  , resource :: Core.Text
    -- ^ The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
  , useLongIds :: Core.Bool
    -- ^ Indicates whether the resource should use longer IDs (17-character IDs)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyIdentityIdFormat' value with any optional fields omitted.
mkModifyIdentityIdFormat
    :: Core.Text -- ^ 'principalArn'
    -> Core.Text -- ^ 'resource'
    -> Core.Bool -- ^ 'useLongIds'
    -> ModifyIdentityIdFormat
mkModifyIdentityIdFormat principalArn resource useLongIds
  = ModifyIdentityIdFormat'{principalArn, resource, useLongIds}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. Specify @all@ to modify the ID format for all IAM users, IAM roles, and the root user of the account.
--
-- /Note:/ Consider using 'principalArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifPrincipalArn :: Lens.Lens' ModifyIdentityIdFormat Core.Text
miifPrincipalArn = Lens.field @"principalArn"
{-# INLINEABLE miifPrincipalArn #-}
{-# DEPRECATED principalArn "Use generic-lens or generic-optics with 'principalArn' instead"  #-}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @route-table@ | @route-table-association@ | @security-group@ | @subnet@ | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- Alternatively, use the @all-current@ option to include all resource types that are currently within their opt-in period for longer IDs.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifResource :: Lens.Lens' ModifyIdentityIdFormat Core.Text
miifResource = Lens.field @"resource"
{-# INLINEABLE miifResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

-- | Indicates whether the resource should use longer IDs (17-character IDs)
--
-- /Note:/ Consider using 'useLongIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifUseLongIds :: Lens.Lens' ModifyIdentityIdFormat Core.Bool
miifUseLongIds = Lens.field @"useLongIds"
{-# INLINEABLE miifUseLongIds #-}
{-# DEPRECATED useLongIds "Use generic-lens or generic-optics with 'useLongIds' instead"  #-}

instance Core.ToQuery ModifyIdentityIdFormat where
        toQuery ModifyIdentityIdFormat{..}
          = Core.toQueryPair "Action" ("ModifyIdentityIdFormat" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PrincipalArn" principalArn
              Core.<> Core.toQueryPair "Resource" resource
              Core.<> Core.toQueryPair "UseLongIds" useLongIds

instance Core.ToHeaders ModifyIdentityIdFormat where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyIdentityIdFormat where
        type Rs ModifyIdentityIdFormat = ModifyIdentityIdFormatResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull ModifyIdentityIdFormatResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyIdentityIdFormatResponse' smart constructor.
data ModifyIdentityIdFormatResponse = ModifyIdentityIdFormatResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyIdentityIdFormatResponse' value with any optional fields omitted.
mkModifyIdentityIdFormatResponse
    :: ModifyIdentityIdFormatResponse
mkModifyIdentityIdFormatResponse = ModifyIdentityIdFormatResponse'

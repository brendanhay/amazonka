{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified AMI. You can specify only one attribute at a time. You can use the @Attribute@ parameter to specify the attribute or one of the following parameters: @Description@ , @LaunchPermission@ , or @ProductCode@ .
--
-- AWS Marketplace product codes cannot be modified. Images with an AWS Marketplace product code cannot be made public.
-- To enable the SriovNetSupport enhanced networking attribute of an image, enable SriovNetSupport on an instance and create an AMI from the instance.
module Network.AWS.EC2.ModifyImageAttribute
    (
    -- * Creating a request
      ModifyImageAttribute (..)
    , mkModifyImageAttribute
    -- ** Request lenses
    , miaImageId
    , miaAttribute
    , miaDescription
    , miaDryRun
    , miaLaunchPermission
    , miaOperationType
    , miaProductCodes
    , miaUserGroups
    , miaUserIds
    , miaValue

    -- * Destructuring the response
    , ModifyImageAttributeResponse (..)
    , mkModifyImageAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyImageAttribute.
--
-- /See:/ 'mkModifyImageAttribute' smart constructor.
data ModifyImageAttribute = ModifyImageAttribute'
  { imageId :: Types.ImageId
    -- ^ The ID of the AMI.
  , attribute :: Core.Maybe Core.Text
    -- ^ The name of the attribute to modify. The valid values are @description@ , @launchPermission@ , and @productCodes@ .
  , description :: Core.Maybe Types.AttributeValue
    -- ^ A new description for the AMI.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , launchPermission :: Core.Maybe Types.LaunchPermissionModifications
    -- ^ A new launch permission for the AMI.
  , operationType :: Core.Maybe Types.OperationType
    -- ^ The operation type. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
  , productCodes :: Core.Maybe [Core.Text]
    -- ^ The DevPay product codes. After you add a product code to an AMI, it can't be removed.
  , userGroups :: Core.Maybe [Core.Text]
    -- ^ The user groups. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
  , userIds :: Core.Maybe [Core.Text]
    -- ^ The AWS account IDs. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
  , value :: Core.Maybe Core.Text
    -- ^ The value of the attribute being modified. This parameter can be used only when the @Attribute@ parameter is @description@ or @productCodes@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyImageAttribute' value with any optional fields omitted.
mkModifyImageAttribute
    :: Types.ImageId -- ^ 'imageId'
    -> ModifyImageAttribute
mkModifyImageAttribute imageId
  = ModifyImageAttribute'{imageId, attribute = Core.Nothing,
                          description = Core.Nothing, dryRun = Core.Nothing,
                          launchPermission = Core.Nothing, operationType = Core.Nothing,
                          productCodes = Core.Nothing, userGroups = Core.Nothing,
                          userIds = Core.Nothing, value = Core.Nothing}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaImageId :: Lens.Lens' ModifyImageAttribute Types.ImageId
miaImageId = Lens.field @"imageId"
{-# INLINEABLE miaImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The name of the attribute to modify. The valid values are @description@ , @launchPermission@ , and @productCodes@ .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaAttribute :: Lens.Lens' ModifyImageAttribute (Core.Maybe Core.Text)
miaAttribute = Lens.field @"attribute"
{-# INLINEABLE miaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | A new description for the AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaDescription :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.AttributeValue)
miaDescription = Lens.field @"description"
{-# INLINEABLE miaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaDryRun :: Lens.Lens' ModifyImageAttribute (Core.Maybe Core.Bool)
miaDryRun = Lens.field @"dryRun"
{-# INLINEABLE miaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | A new launch permission for the AMI.
--
-- /Note:/ Consider using 'launchPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaLaunchPermission :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.LaunchPermissionModifications)
miaLaunchPermission = Lens.field @"launchPermission"
{-# INLINEABLE miaLaunchPermission #-}
{-# DEPRECATED launchPermission "Use generic-lens or generic-optics with 'launchPermission' instead"  #-}

-- | The operation type. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaOperationType :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.OperationType)
miaOperationType = Lens.field @"operationType"
{-# INLINEABLE miaOperationType #-}
{-# DEPRECATED operationType "Use generic-lens or generic-optics with 'operationType' instead"  #-}

-- | The DevPay product codes. After you add a product code to an AMI, it can't be removed.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaProductCodes :: Lens.Lens' ModifyImageAttribute (Core.Maybe [Core.Text])
miaProductCodes = Lens.field @"productCodes"
{-# INLINEABLE miaProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | The user groups. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaUserGroups :: Lens.Lens' ModifyImageAttribute (Core.Maybe [Core.Text])
miaUserGroups = Lens.field @"userGroups"
{-# INLINEABLE miaUserGroups #-}
{-# DEPRECATED userGroups "Use generic-lens or generic-optics with 'userGroups' instead"  #-}

-- | The AWS account IDs. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaUserIds :: Lens.Lens' ModifyImageAttribute (Core.Maybe [Core.Text])
miaUserIds = Lens.field @"userIds"
{-# INLINEABLE miaUserIds #-}
{-# DEPRECATED userIds "Use generic-lens or generic-optics with 'userIds' instead"  #-}

-- | The value of the attribute being modified. This parameter can be used only when the @Attribute@ parameter is @description@ or @productCodes@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaValue :: Lens.Lens' ModifyImageAttribute (Core.Maybe Core.Text)
miaValue = Lens.field @"value"
{-# INLINEABLE miaValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery ModifyImageAttribute where
        toQuery ModifyImageAttribute{..}
          = Core.toQueryPair "Action" ("ModifyImageAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ImageId" imageId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Attribute") attribute
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchPermission")
                launchPermission
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationType")
                operationType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ProductCode")
                productCodes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "UserGroup") userGroups
              Core.<> Core.maybe Core.mempty (Core.toQueryList "UserId") userIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Value") value

instance Core.ToHeaders ModifyImageAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyImageAttribute where
        type Rs ModifyImageAttribute = ModifyImageAttributeResponse
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
        parseResponse = Response.receiveNull ModifyImageAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyImageAttributeResponse' smart constructor.
data ModifyImageAttributeResponse = ModifyImageAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyImageAttributeResponse' value with any optional fields omitted.
mkModifyImageAttributeResponse
    :: ModifyImageAttributeResponse
mkModifyImageAttributeResponse = ModifyImageAttributeResponse'

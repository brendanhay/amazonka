{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyImageAttribute (..),
    mkModifyImageAttribute,

    -- ** Request lenses
    miaImageId,
    miaAttribute,
    miaDescription,
    miaDryRun,
    miaLaunchPermission,
    miaOperationType,
    miaProductCodes,
    miaUserGroups,
    miaUserIds,
    miaValue,

    -- * Destructuring the response
    ModifyImageAttributeResponse (..),
    mkModifyImageAttributeResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyImageAttribute.
--
-- /See:/ 'mkModifyImageAttribute' smart constructor.
data ModifyImageAttribute = ModifyImageAttribute'
  { -- | The ID of the AMI.
    imageId :: Types.ImageId,
    -- | The name of the attribute to modify. The valid values are @description@ , @launchPermission@ , and @productCodes@ .
    attribute :: Core.Maybe Types.Attribute,
    -- | A new description for the AMI.
    description :: Core.Maybe Types.AttributeValue,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | A new launch permission for the AMI.
    launchPermission :: Core.Maybe Types.LaunchPermissionModifications,
    -- | The operation type. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
    operationType :: Core.Maybe Types.OperationType,
    -- | The DevPay product codes. After you add a product code to an AMI, it can't be removed.
    productCodes :: Core.Maybe [Types.String],
    -- | The user groups. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
    userGroups :: Core.Maybe [Types.String],
    -- | The AWS account IDs. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
    userIds :: Core.Maybe [Types.String],
    -- | The value of the attribute being modified. This parameter can be used only when the @Attribute@ parameter is @description@ or @productCodes@ .
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyImageAttribute' value with any optional fields omitted.
mkModifyImageAttribute ::
  -- | 'imageId'
  Types.ImageId ->
  ModifyImageAttribute
mkModifyImageAttribute imageId =
  ModifyImageAttribute'
    { imageId,
      attribute = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      launchPermission = Core.Nothing,
      operationType = Core.Nothing,
      productCodes = Core.Nothing,
      userGroups = Core.Nothing,
      userIds = Core.Nothing,
      value = Core.Nothing
    }

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaImageId :: Lens.Lens' ModifyImageAttribute Types.ImageId
miaImageId = Lens.field @"imageId"
{-# DEPRECATED miaImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The name of the attribute to modify. The valid values are @description@ , @launchPermission@ , and @productCodes@ .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaAttribute :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.Attribute)
miaAttribute = Lens.field @"attribute"
{-# DEPRECATED miaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | A new description for the AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaDescription :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.AttributeValue)
miaDescription = Lens.field @"description"
{-# DEPRECATED miaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaDryRun :: Lens.Lens' ModifyImageAttribute (Core.Maybe Core.Bool)
miaDryRun = Lens.field @"dryRun"
{-# DEPRECATED miaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A new launch permission for the AMI.
--
-- /Note:/ Consider using 'launchPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaLaunchPermission :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.LaunchPermissionModifications)
miaLaunchPermission = Lens.field @"launchPermission"
{-# DEPRECATED miaLaunchPermission "Use generic-lens or generic-optics with 'launchPermission' instead." #-}

-- | The operation type. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaOperationType :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.OperationType)
miaOperationType = Lens.field @"operationType"
{-# DEPRECATED miaOperationType "Use generic-lens or generic-optics with 'operationType' instead." #-}

-- | The DevPay product codes. After you add a product code to an AMI, it can't be removed.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaProductCodes :: Lens.Lens' ModifyImageAttribute (Core.Maybe [Types.String])
miaProductCodes = Lens.field @"productCodes"
{-# DEPRECATED miaProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The user groups. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaUserGroups :: Lens.Lens' ModifyImageAttribute (Core.Maybe [Types.String])
miaUserGroups = Lens.field @"userGroups"
{-# DEPRECATED miaUserGroups "Use generic-lens or generic-optics with 'userGroups' instead." #-}

-- | The AWS account IDs. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaUserIds :: Lens.Lens' ModifyImageAttribute (Core.Maybe [Types.String])
miaUserIds = Lens.field @"userIds"
{-# DEPRECATED miaUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | The value of the attribute being modified. This parameter can be used only when the @Attribute@ parameter is @description@ or @productCodes@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaValue :: Lens.Lens' ModifyImageAttribute (Core.Maybe Types.Value)
miaValue = Lens.field @"value"
{-# DEPRECATED miaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.AWSRequest ModifyImageAttribute where
  type Rs ModifyImageAttribute = ModifyImageAttributeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyImageAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "ImageId" imageId)
                Core.<> (Core.toQueryValue "Attribute" Core.<$> attribute)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "LaunchPermission" Core.<$> launchPermission)
                Core.<> (Core.toQueryValue "OperationType" Core.<$> operationType)
                Core.<> (Core.toQueryList "ProductCode" Core.<$> productCodes)
                Core.<> (Core.toQueryList "UserGroup" Core.<$> userGroups)
                Core.<> (Core.toQueryList "UserId" Core.<$> userIds)
                Core.<> (Core.toQueryValue "Value" Core.<$> value)
            )
      }
  response = Response.receiveNull ModifyImageAttributeResponse'

-- | /See:/ 'mkModifyImageAttributeResponse' smart constructor.
data ModifyImageAttributeResponse = ModifyImageAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyImageAttributeResponse' value with any optional fields omitted.
mkModifyImageAttributeResponse ::
  ModifyImageAttributeResponse
mkModifyImageAttributeResponse = ModifyImageAttributeResponse'

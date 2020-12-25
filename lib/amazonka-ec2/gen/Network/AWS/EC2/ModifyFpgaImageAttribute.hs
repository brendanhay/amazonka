{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyFpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified Amazon FPGA Image (AFI).
module Network.AWS.EC2.ModifyFpgaImageAttribute
  ( -- * Creating a request
    ModifyFpgaImageAttribute (..),
    mkModifyFpgaImageAttribute,

    -- ** Request lenses
    mfiaFpgaImageId,
    mfiaAttribute,
    mfiaDescription,
    mfiaDryRun,
    mfiaLoadPermission,
    mfiaName,
    mfiaOperationType,
    mfiaProductCodes,
    mfiaUserGroups,
    mfiaUserIds,

    -- * Destructuring the response
    ModifyFpgaImageAttributeResponse (..),
    mkModifyFpgaImageAttributeResponse,

    -- ** Response lenses
    mfiarrsFpgaImageAttribute,
    mfiarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyFpgaImageAttribute' smart constructor.
data ModifyFpgaImageAttribute = ModifyFpgaImageAttribute'
  { -- | The ID of the AFI.
    fpgaImageId :: Types.FpgaImageId,
    -- | The name of the attribute.
    attribute :: Core.Maybe Types.FpgaImageAttributeName,
    -- | A description for the AFI.
    description :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The load permission for the AFI.
    loadPermission :: Core.Maybe Types.LoadPermissionModifications,
    -- | A name for the AFI.
    name :: Core.Maybe Types.String,
    -- | The operation type.
    operationType :: Core.Maybe Types.OperationType,
    -- | The product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
    productCodes :: Core.Maybe [Types.String],
    -- | The user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
    userGroups :: Core.Maybe [Types.String],
    -- | The AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
    userIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyFpgaImageAttribute' value with any optional fields omitted.
mkModifyFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Types.FpgaImageId ->
  ModifyFpgaImageAttribute
mkModifyFpgaImageAttribute fpgaImageId =
  ModifyFpgaImageAttribute'
    { fpgaImageId,
      attribute = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      loadPermission = Core.Nothing,
      name = Core.Nothing,
      operationType = Core.Nothing,
      productCodes = Core.Nothing,
      userGroups = Core.Nothing,
      userIds = Core.Nothing
    }

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaFpgaImageId :: Lens.Lens' ModifyFpgaImageAttribute Types.FpgaImageId
mfiaFpgaImageId = Lens.field @"fpgaImageId"
{-# DEPRECATED mfiaFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaAttribute :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.FpgaImageAttributeName)
mfiaAttribute = Lens.field @"attribute"
{-# DEPRECATED mfiaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | A description for the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaDescription :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.String)
mfiaDescription = Lens.field @"description"
{-# DEPRECATED mfiaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaDryRun :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Core.Bool)
mfiaDryRun = Lens.field @"dryRun"
{-# DEPRECATED mfiaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The load permission for the AFI.
--
-- /Note:/ Consider using 'loadPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaLoadPermission :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.LoadPermissionModifications)
mfiaLoadPermission = Lens.field @"loadPermission"
{-# DEPRECATED mfiaLoadPermission "Use generic-lens or generic-optics with 'loadPermission' instead." #-}

-- | A name for the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaName :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.String)
mfiaName = Lens.field @"name"
{-# DEPRECATED mfiaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The operation type.
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaOperationType :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.OperationType)
mfiaOperationType = Lens.field @"operationType"
{-# DEPRECATED mfiaOperationType "Use generic-lens or generic-optics with 'operationType' instead." #-}

-- | The product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaProductCodes :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Types.String])
mfiaProductCodes = Lens.field @"productCodes"
{-# DEPRECATED mfiaProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaUserGroups :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Types.String])
mfiaUserGroups = Lens.field @"userGroups"
{-# DEPRECATED mfiaUserGroups "Use generic-lens or generic-optics with 'userGroups' instead." #-}

-- | The AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaUserIds :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Types.String])
mfiaUserIds = Lens.field @"userIds"
{-# DEPRECATED mfiaUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

instance Core.AWSRequest ModifyFpgaImageAttribute where
  type Rs ModifyFpgaImageAttribute = ModifyFpgaImageAttributeResponse
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
            ( Core.pure ("Action", "ModifyFpgaImageAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "FpgaImageId" fpgaImageId)
                Core.<> (Core.toQueryValue "Attribute" Core.<$> attribute)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "LoadPermission" Core.<$> loadPermission)
                Core.<> (Core.toQueryValue "Name" Core.<$> name)
                Core.<> (Core.toQueryValue "OperationType" Core.<$> operationType)
                Core.<> (Core.toQueryList "ProductCode" Core.<$> productCodes)
                Core.<> (Core.toQueryList "UserGroup" Core.<$> userGroups)
                Core.<> (Core.toQueryList "UserId" Core.<$> userIds)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyFpgaImageAttributeResponse'
            Core.<$> (x Core..@? "fpgaImageAttribute")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyFpgaImageAttributeResponse' smart constructor.
data ModifyFpgaImageAttributeResponse = ModifyFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Core.Maybe Types.FpgaImageAttribute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyFpgaImageAttributeResponse' value with any optional fields omitted.
mkModifyFpgaImageAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyFpgaImageAttributeResponse
mkModifyFpgaImageAttributeResponse responseStatus =
  ModifyFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Core.Nothing,
      responseStatus
    }

-- | Information about the attribute.
--
-- /Note:/ Consider using 'fpgaImageAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiarrsFpgaImageAttribute :: Lens.Lens' ModifyFpgaImageAttributeResponse (Core.Maybe Types.FpgaImageAttribute)
mfiarrsFpgaImageAttribute = Lens.field @"fpgaImageAttribute"
{-# DEPRECATED mfiarrsFpgaImageAttribute "Use generic-lens or generic-optics with 'fpgaImageAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiarrsResponseStatus :: Lens.Lens' ModifyFpgaImageAttributeResponse Core.Int
mfiarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mfiarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

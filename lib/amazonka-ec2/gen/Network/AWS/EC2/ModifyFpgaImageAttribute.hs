{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyFpgaImageAttribute (..)
    , mkModifyFpgaImageAttribute
    -- ** Request lenses
    , mfiaFpgaImageId
    , mfiaAttribute
    , mfiaDescription
    , mfiaDryRun
    , mfiaLoadPermission
    , mfiaName
    , mfiaOperationType
    , mfiaProductCodes
    , mfiaUserGroups
    , mfiaUserIds

    -- * Destructuring the response
    , ModifyFpgaImageAttributeResponse (..)
    , mkModifyFpgaImageAttributeResponse
    -- ** Response lenses
    , mfiarrsFpgaImageAttribute
    , mfiarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyFpgaImageAttribute' smart constructor.
data ModifyFpgaImageAttribute = ModifyFpgaImageAttribute'
  { fpgaImageId :: Types.FpgaImageId
    -- ^ The ID of the AFI.
  , attribute :: Core.Maybe Types.FpgaImageAttributeName
    -- ^ The name of the attribute.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the AFI.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , loadPermission :: Core.Maybe Types.LoadPermissionModifications
    -- ^ The load permission for the AFI.
  , name :: Core.Maybe Core.Text
    -- ^ A name for the AFI.
  , operationType :: Core.Maybe Types.OperationType
    -- ^ The operation type.
  , productCodes :: Core.Maybe [Core.Text]
    -- ^ The product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
  , userGroups :: Core.Maybe [Core.Text]
    -- ^ The user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
  , userIds :: Core.Maybe [Core.Text]
    -- ^ The AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyFpgaImageAttribute' value with any optional fields omitted.
mkModifyFpgaImageAttribute
    :: Types.FpgaImageId -- ^ 'fpgaImageId'
    -> ModifyFpgaImageAttribute
mkModifyFpgaImageAttribute fpgaImageId
  = ModifyFpgaImageAttribute'{fpgaImageId, attribute = Core.Nothing,
                              description = Core.Nothing, dryRun = Core.Nothing,
                              loadPermission = Core.Nothing, name = Core.Nothing,
                              operationType = Core.Nothing, productCodes = Core.Nothing,
                              userGroups = Core.Nothing, userIds = Core.Nothing}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaFpgaImageId :: Lens.Lens' ModifyFpgaImageAttribute Types.FpgaImageId
mfiaFpgaImageId = Lens.field @"fpgaImageId"
{-# INLINEABLE mfiaFpgaImageId #-}
{-# DEPRECATED fpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead"  #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaAttribute :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.FpgaImageAttributeName)
mfiaAttribute = Lens.field @"attribute"
{-# INLINEABLE mfiaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | A description for the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaDescription :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Core.Text)
mfiaDescription = Lens.field @"description"
{-# INLINEABLE mfiaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaDryRun :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Core.Bool)
mfiaDryRun = Lens.field @"dryRun"
{-# INLINEABLE mfiaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The load permission for the AFI.
--
-- /Note:/ Consider using 'loadPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaLoadPermission :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.LoadPermissionModifications)
mfiaLoadPermission = Lens.field @"loadPermission"
{-# INLINEABLE mfiaLoadPermission #-}
{-# DEPRECATED loadPermission "Use generic-lens or generic-optics with 'loadPermission' instead"  #-}

-- | A name for the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaName :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Core.Text)
mfiaName = Lens.field @"name"
{-# INLINEABLE mfiaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The operation type.
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaOperationType :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Types.OperationType)
mfiaOperationType = Lens.field @"operationType"
{-# INLINEABLE mfiaOperationType #-}
{-# DEPRECATED operationType "Use generic-lens or generic-optics with 'operationType' instead"  #-}

-- | The product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaProductCodes :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Core.Text])
mfiaProductCodes = Lens.field @"productCodes"
{-# INLINEABLE mfiaProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | The user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaUserGroups :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Core.Text])
mfiaUserGroups = Lens.field @"userGroups"
{-# INLINEABLE mfiaUserGroups #-}
{-# DEPRECATED userGroups "Use generic-lens or generic-optics with 'userGroups' instead"  #-}

-- | The AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaUserIds :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Core.Text])
mfiaUserIds = Lens.field @"userIds"
{-# INLINEABLE mfiaUserIds #-}
{-# DEPRECATED userIds "Use generic-lens or generic-optics with 'userIds' instead"  #-}

instance Core.ToQuery ModifyFpgaImageAttribute where
        toQuery ModifyFpgaImageAttribute{..}
          = Core.toQueryPair "Action"
              ("ModifyFpgaImageAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "FpgaImageId" fpgaImageId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Attribute") attribute
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LoadPermission")
                loadPermission
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Name") name
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationType")
                operationType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ProductCode")
                productCodes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "UserGroup") userGroups
              Core.<> Core.maybe Core.mempty (Core.toQueryList "UserId") userIds

instance Core.ToHeaders ModifyFpgaImageAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyFpgaImageAttribute where
        type Rs ModifyFpgaImageAttribute = ModifyFpgaImageAttributeResponse
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
          = Response.receiveXML
              (\ s h x ->
                 ModifyFpgaImageAttributeResponse' Core.<$>
                   (x Core..@? "fpgaImageAttribute") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyFpgaImageAttributeResponse' smart constructor.
data ModifyFpgaImageAttributeResponse = ModifyFpgaImageAttributeResponse'
  { fpgaImageAttribute :: Core.Maybe Types.FpgaImageAttribute
    -- ^ Information about the attribute.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyFpgaImageAttributeResponse' value with any optional fields omitted.
mkModifyFpgaImageAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyFpgaImageAttributeResponse
mkModifyFpgaImageAttributeResponse responseStatus
  = ModifyFpgaImageAttributeResponse'{fpgaImageAttribute =
                                        Core.Nothing,
                                      responseStatus}

-- | Information about the attribute.
--
-- /Note:/ Consider using 'fpgaImageAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiarrsFpgaImageAttribute :: Lens.Lens' ModifyFpgaImageAttributeResponse (Core.Maybe Types.FpgaImageAttribute)
mfiarrsFpgaImageAttribute = Lens.field @"fpgaImageAttribute"
{-# INLINEABLE mfiarrsFpgaImageAttribute #-}
{-# DEPRECATED fpgaImageAttribute "Use generic-lens or generic-optics with 'fpgaImageAttribute' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiarrsResponseStatus :: Lens.Lens' ModifyFpgaImageAttributeResponse Core.Int
mfiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mfiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

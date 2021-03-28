{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SetTypeDefaultVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the default version of a type. The default version of a type will be used in CloudFormation operations.
module Network.AWS.CloudFormation.SetTypeDefaultVersion
    (
    -- * Creating a request
      SetTypeDefaultVersion (..)
    , mkSetTypeDefaultVersion
    -- ** Request lenses
    , stdvArn
    , stdvType
    , stdvTypeName
    , stdvVersionId

    -- * Destructuring the response
    , SetTypeDefaultVersionResponse (..)
    , mkSetTypeDefaultVersionResponse
    -- ** Response lenses
    , stdvrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetTypeDefaultVersion' smart constructor.
data SetTypeDefaultVersion = SetTypeDefaultVersion'
  { arn :: Core.Maybe Types.PrivateTypeArn
    -- ^ The Amazon Resource Name (ARN) of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
  , type' :: Core.Maybe Types.RegistryType
    -- ^ The kind of type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
  , typeName :: Core.Maybe Types.TypeName
    -- ^ The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
  , versionId :: Core.Maybe Types.TypeVersionId
    -- ^ The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTypeDefaultVersion' value with any optional fields omitted.
mkSetTypeDefaultVersion
    :: SetTypeDefaultVersion
mkSetTypeDefaultVersion
  = SetTypeDefaultVersion'{arn = Core.Nothing, type' = Core.Nothing,
                           typeName = Core.Nothing, versionId = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvArn :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe Types.PrivateTypeArn)
stdvArn = Lens.field @"arn"
{-# INLINEABLE stdvArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The kind of type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvType :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe Types.RegistryType)
stdvType = Lens.field @"type'"
{-# INLINEABLE stdvType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvTypeName :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe Types.TypeName)
stdvTypeName = Lens.field @"typeName"
{-# INLINEABLE stdvTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvVersionId :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe Types.TypeVersionId)
stdvVersionId = Lens.field @"versionId"
{-# INLINEABLE stdvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery SetTypeDefaultVersion where
        toQuery SetTypeDefaultVersion{..}
          = Core.toQueryPair "Action" ("SetTypeDefaultVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Arn") arn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TypeName") typeName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VersionId") versionId

instance Core.ToHeaders SetTypeDefaultVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetTypeDefaultVersion where
        type Rs SetTypeDefaultVersion = SetTypeDefaultVersionResponse
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
          = Response.receiveXMLWrapper "SetTypeDefaultVersionResult"
              (\ s h x ->
                 SetTypeDefaultVersionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetTypeDefaultVersionResponse' smart constructor.
newtype SetTypeDefaultVersionResponse = SetTypeDefaultVersionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetTypeDefaultVersionResponse' value with any optional fields omitted.
mkSetTypeDefaultVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetTypeDefaultVersionResponse
mkSetTypeDefaultVersionResponse responseStatus
  = SetTypeDefaultVersionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvrrsResponseStatus :: Lens.Lens' SetTypeDefaultVersionResponse Core.Int
stdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

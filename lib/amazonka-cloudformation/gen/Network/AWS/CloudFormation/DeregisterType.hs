{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DeregisterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a type or type version from active use in the CloudFormation registry. If a type or type version is deregistered, it cannot be used in CloudFormation operations.
--
-- To deregister a type, you must individually deregister all registered versions of that type. If a type has only a single registered version, deregistering that version results in the type itself being deregistered. 
-- You cannot deregister the default version of a type, unless it is the only registered version of that type, in which case the type itself is deregistered as well. 
module Network.AWS.CloudFormation.DeregisterType
    (
    -- * Creating a request
      DeregisterType (..)
    , mkDeregisterType
    -- ** Request lenses
    , dArn
    , dType
    , dTypeName
    , dVersionId

    -- * Destructuring the response
    , DeregisterTypeResponse (..)
    , mkDeregisterTypeResponse
    -- ** Response lenses
    , dtrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterType' smart constructor.
data DeregisterType = DeregisterType'
  { arn :: Core.Maybe Types.PrivateTypeArn
    -- ^ The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
  , type' :: Core.Maybe Types.RegistryType
    -- ^ The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
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

-- | Creates a 'DeregisterType' value with any optional fields omitted.
mkDeregisterType
    :: DeregisterType
mkDeregisterType
  = DeregisterType'{arn = Core.Nothing, type' = Core.Nothing,
                    typeName = Core.Nothing, versionId = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' DeregisterType (Core.Maybe Types.PrivateTypeArn)
dArn = Lens.field @"arn"
{-# INLINEABLE dArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dType :: Lens.Lens' DeregisterType (Core.Maybe Types.RegistryType)
dType = Lens.field @"type'"
{-# INLINEABLE dType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTypeName :: Lens.Lens' DeregisterType (Core.Maybe Types.TypeName)
dTypeName = Lens.field @"typeName"
{-# INLINEABLE dTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionId :: Lens.Lens' DeregisterType (Core.Maybe Types.TypeVersionId)
dVersionId = Lens.field @"versionId"
{-# INLINEABLE dVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery DeregisterType where
        toQuery DeregisterType{..}
          = Core.toQueryPair "Action" ("DeregisterType" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Arn") arn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TypeName") typeName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VersionId") versionId

instance Core.ToHeaders DeregisterType where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeregisterType where
        type Rs DeregisterType = DeregisterTypeResponse
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
          = Response.receiveXMLWrapper "DeregisterTypeResult"
              (\ s h x ->
                 DeregisterTypeResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterTypeResponse' smart constructor.
newtype DeregisterTypeResponse = DeregisterTypeResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTypeResponse' value with any optional fields omitted.
mkDeregisterTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterTypeResponse
mkDeregisterTypeResponse responseStatus
  = DeregisterTypeResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeregisterTypeResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeregisterType (..),
    mkDeregisterType,

    -- ** Request lenses
    dArn,
    dType,
    dTypeName,
    dVersionId,

    -- * Destructuring the response
    DeregisterTypeResponse (..),
    mkDeregisterTypeResponse,

    -- ** Response lenses
    dtrfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterType' smart constructor.
data DeregisterType = DeregisterType'
  { -- | The Amazon Resource Name (ARN) of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    arn :: Core.Maybe Types.PrivateTypeArn,
    -- | The kind of type.
    --
    -- Currently the only valid value is @RESOURCE@ .
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    type' :: Core.Maybe Types.RegistryType,
    -- | The name of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    typeName :: Core.Maybe Types.TypeName,
    -- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
    versionId :: Core.Maybe Types.TypeVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterType' value with any optional fields omitted.
mkDeregisterType ::
  DeregisterType
mkDeregisterType =
  DeregisterType'
    { arn = Core.Nothing,
      type' = Core.Nothing,
      typeName = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' DeregisterType (Core.Maybe Types.PrivateTypeArn)
dArn = Lens.field @"arn"
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dType :: Lens.Lens' DeregisterType (Core.Maybe Types.RegistryType)
dType = Lens.field @"type'"
{-# DEPRECATED dType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTypeName :: Lens.Lens' DeregisterType (Core.Maybe Types.TypeName)
dTypeName = Lens.field @"typeName"
{-# DEPRECATED dTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionId :: Lens.Lens' DeregisterType (Core.Maybe Types.TypeVersionId)
dVersionId = Lens.field @"versionId"
{-# DEPRECATED dVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest DeregisterType where
  type Rs DeregisterType = DeregisterTypeResponse
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
            ( Core.pure ("Action", "DeregisterType")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "Arn" Core.<$> arn)
                Core.<> (Core.toQueryValue "Type" Core.<$> type')
                Core.<> (Core.toQueryValue "TypeName" Core.<$> typeName)
                Core.<> (Core.toQueryValue "VersionId" Core.<$> versionId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeregisterTypeResult"
      ( \s h x ->
          DeregisterTypeResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterTypeResponse' smart constructor.
newtype DeregisterTypeResponse = DeregisterTypeResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTypeResponse' value with any optional fields omitted.
mkDeregisterTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterTypeResponse
mkDeregisterTypeResponse responseStatus =
  DeregisterTypeResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeregisterTypeResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

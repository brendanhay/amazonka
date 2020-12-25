{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a given object from the parent object. The object that is to be detached from the parent is specified by the link name.
module Network.AWS.CloudDirectory.DetachObject
  ( -- * Creating a request
    DetachObject (..),
    mkDetachObject,

    -- ** Request lenses
    dofDirectoryArn,
    dofParentReference,
    dofLinkName,

    -- * Destructuring the response
    DetachObjectResponse (..),
    mkDetachObjectResponse,

    -- ** Response lenses
    dorfrsDetachedObjectIdentifier,
    dorfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachObject' smart constructor.
data DetachObject = DetachObject'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | The parent reference from which the object with the specified link name is detached.
    parentReference :: Types.ObjectReference,
    -- | The link name associated with the object that needs to be detached.
    linkName :: Types.LinkName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachObject' value with any optional fields omitted.
mkDetachObject ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'parentReference'
  Types.ObjectReference ->
  -- | 'linkName'
  Types.LinkName ->
  DetachObject
mkDetachObject directoryArn parentReference linkName =
  DetachObject' {directoryArn, parentReference, linkName}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofDirectoryArn :: Lens.Lens' DetachObject Types.Arn
dofDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED dofDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The parent reference from which the object with the specified link name is detached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofParentReference :: Lens.Lens' DetachObject Types.ObjectReference
dofParentReference = Lens.field @"parentReference"
{-# DEPRECATED dofParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The link name associated with the object that needs to be detached.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofLinkName :: Lens.Lens' DetachObject Types.LinkName
dofLinkName = Lens.field @"linkName"
{-# DEPRECATED dofLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

instance Core.FromJSON DetachObject where
  toJSON DetachObject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParentReference" Core..= parentReference),
            Core.Just ("LinkName" Core..= linkName)
          ]
      )

instance Core.AWSRequest DetachObject where
  type Rs DetachObject = DetachObjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/object/detach",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachObjectResponse'
            Core.<$> (x Core..:? "DetachedObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetachObjectResponse' smart constructor.
data DetachObjectResponse = DetachObjectResponse'
  { -- | The @ObjectIdentifier@ that was detached from the object.
    detachedObjectIdentifier :: Core.Maybe Types.ObjectIdentifier,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachObjectResponse' value with any optional fields omitted.
mkDetachObjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachObjectResponse
mkDetachObjectResponse responseStatus =
  DetachObjectResponse'
    { detachedObjectIdentifier = Core.Nothing,
      responseStatus
    }

-- | The @ObjectIdentifier@ that was detached from the object.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorfrsDetachedObjectIdentifier :: Lens.Lens' DetachObjectResponse (Core.Maybe Types.ObjectIdentifier)
dorfrsDetachedObjectIdentifier = Lens.field @"detachedObjectIdentifier"
{-# DEPRECATED dorfrsDetachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorfrsResponseStatus :: Lens.Lens' DetachObjectResponse Core.Int
dorfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dorfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

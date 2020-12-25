{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserHierarchyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the user hierarchy structure: add, remove, and rename user hierarchy levels.
module Network.AWS.Connect.UpdateUserHierarchyStructure
  ( -- * Creating a request
    UpdateUserHierarchyStructure (..),
    mkUpdateUserHierarchyStructure,

    -- ** Request lenses
    uuhsHierarchyStructure,
    uuhsInstanceId,

    -- * Destructuring the response
    UpdateUserHierarchyStructureResponse (..),
    mkUpdateUserHierarchyStructureResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserHierarchyStructure' smart constructor.
data UpdateUserHierarchyStructure = UpdateUserHierarchyStructure'
  { -- | The hierarchy levels to update.
    hierarchyStructure :: Types.HierarchyStructureUpdate,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserHierarchyStructure' value with any optional fields omitted.
mkUpdateUserHierarchyStructure ::
  -- | 'hierarchyStructure'
  Types.HierarchyStructureUpdate ->
  -- | 'instanceId'
  Types.InstanceId ->
  UpdateUserHierarchyStructure
mkUpdateUserHierarchyStructure hierarchyStructure instanceId =
  UpdateUserHierarchyStructure' {hierarchyStructure, instanceId}

-- | The hierarchy levels to update.
--
-- /Note:/ Consider using 'hierarchyStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhsHierarchyStructure :: Lens.Lens' UpdateUserHierarchyStructure Types.HierarchyStructureUpdate
uuhsHierarchyStructure = Lens.field @"hierarchyStructure"
{-# DEPRECATED uuhsHierarchyStructure "Use generic-lens or generic-optics with 'hierarchyStructure' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhsInstanceId :: Lens.Lens' UpdateUserHierarchyStructure Types.InstanceId
uuhsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uuhsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON UpdateUserHierarchyStructure where
  toJSON UpdateUserHierarchyStructure {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("HierarchyStructure" Core..= hierarchyStructure)]
      )

instance Core.AWSRequest UpdateUserHierarchyStructure where
  type
    Rs UpdateUserHierarchyStructure =
      UpdateUserHierarchyStructureResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/user-hierarchy-structure/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull UpdateUserHierarchyStructureResponse'

-- | /See:/ 'mkUpdateUserHierarchyStructureResponse' smart constructor.
data UpdateUserHierarchyStructureResponse = UpdateUserHierarchyStructureResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserHierarchyStructureResponse' value with any optional fields omitted.
mkUpdateUserHierarchyStructureResponse ::
  UpdateUserHierarchyStructureResponse
mkUpdateUserHierarchyStructureResponse =
  UpdateUserHierarchyStructureResponse'

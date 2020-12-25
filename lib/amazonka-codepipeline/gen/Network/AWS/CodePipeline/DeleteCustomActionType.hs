{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeleteCustomActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks a custom action as deleted. @PollForJobs@ for the custom action fails after the action is marked for deletion. Used for custom actions only.
--
-- /Important:/ To re-create a custom action after it has been deleted you must use a string in the version field that has never been used before. This string can be an incremented version number, for example. To restore a deleted custom action, use a JSON file that is identical to the deleted action, including the original string in the version field.
module Network.AWS.CodePipeline.DeleteCustomActionType
  ( -- * Creating a request
    DeleteCustomActionType (..),
    mkDeleteCustomActionType,

    -- ** Request lenses
    dcatCategory,
    dcatProvider,
    dcatVersion,

    -- * Destructuring the response
    DeleteCustomActionTypeResponse (..),
    mkDeleteCustomActionTypeResponse,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCustomActionType@ operation. The custom action will be marked as deleted.
--
-- /See:/ 'mkDeleteCustomActionType' smart constructor.
data DeleteCustomActionType = DeleteCustomActionType'
  { -- | The category of the custom action that you want to delete, such as source or deploy.
    category :: Types.ActionCategory,
    -- | The provider of the service used in the custom action, such as AWS CodeDeploy.
    provider :: Types.ActionProvider,
    -- | The version of the custom action to delete.
    version :: Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomActionType' value with any optional fields omitted.
mkDeleteCustomActionType ::
  -- | 'category'
  Types.ActionCategory ->
  -- | 'provider'
  Types.ActionProvider ->
  -- | 'version'
  Types.Version ->
  DeleteCustomActionType
mkDeleteCustomActionType category provider version =
  DeleteCustomActionType' {category, provider, version}

-- | The category of the custom action that you want to delete, such as source or deploy.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatCategory :: Lens.Lens' DeleteCustomActionType Types.ActionCategory
dcatCategory = Lens.field @"category"
{-# DEPRECATED dcatCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The provider of the service used in the custom action, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatProvider :: Lens.Lens' DeleteCustomActionType Types.ActionProvider
dcatProvider = Lens.field @"provider"
{-# DEPRECATED dcatProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

-- | The version of the custom action to delete.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatVersion :: Lens.Lens' DeleteCustomActionType Types.Version
dcatVersion = Lens.field @"version"
{-# DEPRECATED dcatVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON DeleteCustomActionType where
  toJSON DeleteCustomActionType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("category" Core..= category),
            Core.Just ("provider" Core..= provider),
            Core.Just ("version" Core..= version)
          ]
      )

instance Core.AWSRequest DeleteCustomActionType where
  type Rs DeleteCustomActionType = DeleteCustomActionTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.DeleteCustomActionType")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteCustomActionTypeResponse'

-- | /See:/ 'mkDeleteCustomActionTypeResponse' smart constructor.
data DeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomActionTypeResponse' value with any optional fields omitted.
mkDeleteCustomActionTypeResponse ::
  DeleteCustomActionTypeResponse
mkDeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'

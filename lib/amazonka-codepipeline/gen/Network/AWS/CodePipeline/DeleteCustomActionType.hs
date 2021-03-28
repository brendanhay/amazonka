{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteCustomActionType (..)
    , mkDeleteCustomActionType
    -- ** Request lenses
    , dcatCategory
    , dcatProvider
    , dcatVersion

    -- * Destructuring the response
    , DeleteCustomActionTypeResponse (..)
    , mkDeleteCustomActionTypeResponse
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCustomActionType@ operation. The custom action will be marked as deleted.
--
-- /See:/ 'mkDeleteCustomActionType' smart constructor.
data DeleteCustomActionType = DeleteCustomActionType'
  { category :: Types.ActionCategory
    -- ^ The category of the custom action that you want to delete, such as source or deploy.
  , provider :: Types.ActionProvider
    -- ^ The provider of the service used in the custom action, such as AWS CodeDeploy.
  , version :: Types.Version
    -- ^ The version of the custom action to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomActionType' value with any optional fields omitted.
mkDeleteCustomActionType
    :: Types.ActionCategory -- ^ 'category'
    -> Types.ActionProvider -- ^ 'provider'
    -> Types.Version -- ^ 'version'
    -> DeleteCustomActionType
mkDeleteCustomActionType category provider version
  = DeleteCustomActionType'{category, provider, version}

-- | The category of the custom action that you want to delete, such as source or deploy.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatCategory :: Lens.Lens' DeleteCustomActionType Types.ActionCategory
dcatCategory = Lens.field @"category"
{-# INLINEABLE dcatCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The provider of the service used in the custom action, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatProvider :: Lens.Lens' DeleteCustomActionType Types.ActionProvider
dcatProvider = Lens.field @"provider"
{-# INLINEABLE dcatProvider #-}
{-# DEPRECATED provider "Use generic-lens or generic-optics with 'provider' instead"  #-}

-- | The version of the custom action to delete.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatVersion :: Lens.Lens' DeleteCustomActionType Types.Version
dcatVersion = Lens.field @"version"
{-# INLINEABLE dcatVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery DeleteCustomActionType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCustomActionType where
        toHeaders DeleteCustomActionType{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.DeleteCustomActionType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCustomActionType where
        toJSON DeleteCustomActionType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("category" Core..= category),
                  Core.Just ("provider" Core..= provider),
                  Core.Just ("version" Core..= version)])

instance Core.AWSRequest DeleteCustomActionType where
        type Rs DeleteCustomActionType = DeleteCustomActionTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteCustomActionTypeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCustomActionTypeResponse' smart constructor.
data DeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomActionTypeResponse' value with any optional fields omitted.
mkDeleteCustomActionTypeResponse
    :: DeleteCustomActionTypeResponse
mkDeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'

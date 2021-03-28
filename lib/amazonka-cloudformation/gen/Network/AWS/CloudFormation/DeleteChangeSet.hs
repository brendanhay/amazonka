{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DeleteChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified change set. Deleting change sets ensures that no one executes the wrong change set.
--
-- If the call successfully completes, AWS CloudFormation successfully deleted the change set.
-- If @IncludeNestedStacks@ specifies @True@ during the creation of the nested change set, then @DeleteChangeSet@ will delete all change sets that belong to the stacks hierarchy and will also delete all change sets for nested stacks with the status of @REVIEW_IN_PROGRESS@ .
module Network.AWS.CloudFormation.DeleteChangeSet
    (
    -- * Creating a request
      DeleteChangeSet (..)
    , mkDeleteChangeSet
    -- ** Request lenses
    , dcsChangeSetName
    , dcsStackName

    -- * Destructuring the response
    , DeleteChangeSetResponse (..)
    , mkDeleteChangeSetResponse
    -- ** Response lenses
    , dcsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DeleteChangeSet' action.
--
-- /See:/ 'mkDeleteChangeSet' smart constructor.
data DeleteChangeSet = DeleteChangeSet'
  { changeSetName :: Types.ChangeSetNameOrId
    -- ^ The name or Amazon Resource Name (ARN) of the change set that you want to delete.
  , stackName :: Core.Maybe Types.StackName
    -- ^ If you specified the name of a change set to delete, specify the stack name or ID (ARN) that is associated with it.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChangeSet' value with any optional fields omitted.
mkDeleteChangeSet
    :: Types.ChangeSetNameOrId -- ^ 'changeSetName'
    -> DeleteChangeSet
mkDeleteChangeSet changeSetName
  = DeleteChangeSet'{changeSetName, stackName = Core.Nothing}

-- | The name or Amazon Resource Name (ARN) of the change set that you want to delete.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsChangeSetName :: Lens.Lens' DeleteChangeSet Types.ChangeSetNameOrId
dcsChangeSetName = Lens.field @"changeSetName"
{-# INLINEABLE dcsChangeSetName #-}
{-# DEPRECATED changeSetName "Use generic-lens or generic-optics with 'changeSetName' instead"  #-}

-- | If you specified the name of a change set to delete, specify the stack name or ID (ARN) that is associated with it.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStackName :: Lens.Lens' DeleteChangeSet (Core.Maybe Types.StackName)
dcsStackName = Lens.field @"stackName"
{-# INLINEABLE dcsStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

instance Core.ToQuery DeleteChangeSet where
        toQuery DeleteChangeSet{..}
          = Core.toQueryPair "Action" ("DeleteChangeSet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "ChangeSetName" changeSetName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackName") stackName

instance Core.ToHeaders DeleteChangeSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteChangeSet where
        type Rs DeleteChangeSet = DeleteChangeSetResponse
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
          = Response.receiveXMLWrapper "DeleteChangeSetResult"
              (\ s h x ->
                 DeleteChangeSetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'DeleteChangeSet' action.
--
-- /See:/ 'mkDeleteChangeSetResponse' smart constructor.
newtype DeleteChangeSetResponse = DeleteChangeSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChangeSetResponse' value with any optional fields omitted.
mkDeleteChangeSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteChangeSetResponse
mkDeleteChangeSetResponse responseStatus
  = DeleteChangeSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DeleteChangeSetResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
